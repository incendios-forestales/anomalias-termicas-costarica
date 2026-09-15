# Visualizaciones: mapa animado (GIF/MP4), mapa interactivo con deslizador
# temporal y gráficos estadísticos.

# Color único para la serie de detecciones (naranja quemado) y rampa
# secuencial perceptualmente uniforme (inferno) para la magnitud FRP.
COLOR_DETECCIONES <- "#bf5b17"

# Nombres de mes abreviados en español (independientes del locale del sistema).
MESES_ES <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
              "Jul", "Ago", "Set", "Oct", "Nov", "Dic")

# Los mismos, completos: los rótulos de figura usan la forma abreviada, pero la
# prosa de los reportes nombra los meses enteros.
MESES_ES_LARGO <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                    "Julio", "Agosto", "Setiembre", "Octubre", "Noviembre",
                    "Diciembre")

# Mapa animado de detecciones por mes sobre el límite del área de estudio.
# `mensual` (con meses en 0 incluidos) define la secuencia completa de cuadros;
# los meses sin detecciones se representan con un cuadro vacío mediante puntos
# fantasma invisibles, para que el tiempo avance a ritmo constante.
# El formato de salida se infiere de la extensión de `dest` (.gif o .mp4).
# El lienzo se calcula de la proporción del área: a escala nacional los puntos
# van pequeños (a ~450 m/px un punto grande taparía un cantón).
animar_detecciones <- function(puntos, area, mensual, archivos_worldcover,
                               bbox, dest, etiqueta_fuente, fuente_datos,
                               fps = 4) {
  etiquetas <- format(mensual$aniomes, "%Y-%m")
  puntos <- puntos |>
    dplyr::mutate(cuadro = factor(format(aniomes, "%Y-%m"), levels = etiquetas))

  centro <- sf::st_point_on_surface(sf::st_geometry(sf::st_union(area)))
  fantasma <- sf::st_sf(
    cuadro = factor(etiquetas, levels = etiquetas),
    frp = NA_real_,
    geometry = rep(centro, length(etiquetas))
  )

  b <- sf::st_bbox(a_crtm05(area))
  proporcion <- as.numeric((b["ymax"] - b["ymin"]) / (b["xmax"] - b["xmin"]))
  ancho_px <- 1000L
  alto_px <- as.integer(round(ancho_px * proporcion)) + 160L
  # libx264 (renderizador MP4 de av) exige dimensiones pares.
  if (alto_px %% 2L == 1L) alto_px <- alto_px + 1L

  fondo <- fondo_cobertura_animacion(archivos_worldcover, bbox,
                                     ancho_px = ancho_px)
  p <- ggplot2::ggplot() +
    ggplot2::annotation_raster(fondo$imagen, xmin = fondo$xmin, xmax = fondo$xmax,
                               ymin = fondo$ymin, ymax = fondo$ymax) +
    ggplot2::geom_sf(data = area, fill = NA, color = "grey30",
                     linewidth = 0.4) +
    ggplot2::geom_sf(data = fantasma, alpha = 0, show.legend = FALSE) +
    # shape 21 con borde oscuro: sin él, las detecciones de FRP bajo (extremo
    # claro de inferno) se camuflan contra el pastizal amarillo del fondo.
    ggplot2::geom_sf(data = puntos, ggplot2::aes(fill = frp),
                     shape = 21, size = 1.5, stroke = 0.2,
                     color = "grey15", alpha = 0.85) +
    ggplot2::scale_fill_viridis_c(option = "inferno", direction = -1,
                                  trans = "sqrt", na.value = "transparent",
                                  name = "FRP (MW)") +
    ggplot2::labs(
      title = AREA_TITULO,
      subtitle = paste0("Detecciones ", etiqueta_fuente, " — mes: {current_frame}"),
      caption = fuente_datos
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "grey92", linewidth = 0.3),
      plot.title = ggplot2::element_text(face = "bold")
    ) +
    gganimate::transition_manual(cuadro)

  renderizador <- if (grepl("\\.mp4$", dest)) {
    gganimate::av_renderer(dest)
  } else {
    gganimate::gifski_renderer(dest)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  gganimate::animate(
    p, renderer = renderizador,
    nframes = length(etiquetas), fps = fps,
    width = ancho_px, height = alto_px, res = 96
  )
  dest
}

# Mapa interactivo leaflet con deslizador de tiempo (leaflet.extras2).
# Retorna el widget (para incrustar en el reporte Quarto).
#
# A escala nacional el registro completo son 10⁵-10⁶ detecciones: un marcador
# por detección con popup rebasaría por mucho lo que un HTML autocontenido
# puede cargar. El mapa divide el trabajo en dos capas:
#   - El deslizador temporal opera solo sobre los ÚLTIMOS `meses_recientes`
#     meses (la ventana de monitoreo), con marcadores completos y popups.
#   - El registro completo va como puntos WebGL ({leafgl}), sin popups,
#     oculto al inicio: da el contexto histórico a costo mínimo.
# Los píxeles de área quemada embebidos se limitan a la misma ventana
# reciente. `area_web` y `areas_conservacion` llegan ya en la resolución de
# publicación (simplificar_para_web()).
#
# El plugin del deslizador (leaflet.SliderControl) tiene tres limitaciones que
# se corrigen aquí sin parchar el paquete:
#   1. En modo rango omite el rótulo de fecha inicial (solo lo pinta al arrastrar).
#   2. En modo rango borra el rótulo en CADA mouseup del documento.
#   3. El rótulo se dibuja debajo de la barra, por lo que en "bottomleft" queda
#      recortado por el borde del mapa (por eso el control va en "topright").
# 1 y 2 se resuelven con onRender: se repinta el rótulo con el rango de fechas
# seleccionado (los índices del deslizador corresponden al orden de los puntos).
# `time` se pasa como fecha ISO en texto: es lo que el plugin muestra al arrastrar.
crear_mapa_temporal <- function(puntos, area_web, cobertura, archivos_worldcover,
                                bbox, etiqueta_quemas,
                                areas_conservacion = NULL, quemas = NULL,
                                meses_recientes = 24,
                                celdas_temporada = NULL) {
  # Clase de cobertura dominante por detección, unida por id_deteccion (la
  # llave estable que asigna a_sf_puntos(); nada depende del orden de filas).
  puntos_wgs84 <- sf::st_transform(puntos, CRS_WGS84) |>
    dplyr::left_join(
      clase_dominante(cobertura) |>
        dplyr::select(id_deteccion, clase_cobertura = clase,
                      fraccion_cobertura = fraccion),
      by = "id_deteccion"
    ) |>
    dplyr::arrange(acq_date) |>
    dplyr::mutate(time = as.character(acq_date))

  corte_reciente <- min(seq(
    from = lubridate::floor_date(max(puntos_wgs84$aniomes), "month"),
    by = "-1 month", length.out = meses_recientes
  ))
  # Solo las columnas que usa el popup: el plugin serializa TODOS los
  # atributos de cada punto en el HTML y las columnas crudas de FIRMS
  # duplicarían el peso sin aportar nada.
  recientes <- puntos_wgs84[puntos_wgs84$aniomes >= corte_reciente,
                            c("acq_date", "acq_time", "frp", "confidence",
                              "nivel", "satellite", "clase_cobertura",
                              "fraccion_cobertura", "time")]

  # Capa de cobertura: raster categórico (method = "ngb" para no interpolar
  # entre códigos de clase), oculta al inicio y conmutable desde el control.
  # Agregado por moda a ~1500 columnas: el recorte nacional a 10 m tiene
  # ~46 000 y rebasaría maxBytes (y el navegador) por tres órdenes de
  # magnitud.
  recorte <- recortar_worldcover(archivos_worldcover, bbox)
  fact <- max(1, ceiling(terra::ncol(recorte) / 1500))
  if (fact > 1) {
    recorte <- terra::aggregate(recorte, fact = fact, fun = "modal",
                                na.rm = TRUE)
  }
  valores <- sort(terra::unique(recorte)[[1]])
  paleta <- leaflet::colorFactor(
    unname(COLORES_WORLDCOVER[as.character(valores)]),
    levels = valores, na.color = "transparent"
  )
  GRUPO_COBERTURA <- "Cobertura (WorldCover 2021)"
  GRUPO_AC        <- "Áreas de conservación (SINAC)"
  GRUPO_LIMITE    <- AREA_LIMITE_LABEL
  GRUPO_REGISTRO  <- "Registro completo (sin popups)"
  GRUPO_QUEMAS    <- paste0(etiqueta_quemas, " — ventana reciente")
  GRUPO_TEMPORADA <- "Temporada de fuego por celda (LON)"
  # Los píxeles quemados llevan layerId propio para que el deslizador pueda
  # mostrarlos u ocultarlos uno por uno (ver onRender). Solo se embeben los
  # de la ventana reciente: los históricos a escala nacional son demasiados.
  if (!is.null(quemas)) {
    quemas <- quemas[quemas$aniomes >= corte_reciente, ]
  }
  hay_quemas <- !is.null(quemas) && nrow(quemas) > 0
  if (hay_quemas) {
    quemas_wgs84 <- sf::st_transform(quemas, CRS_WGS84) |>
      dplyr::mutate(id_quema = paste0("quema_", dplyr::row_number()))
  }
  # Las detecciones recientes las dibuja el plugin del deslizador marcador por
  # marcador, fuera de todo grupo de leaflet: este grupo es solo el ancla del
  # conmutador en el control de capas; el mostrado/ocultado real está en
  # onRender.
  GRUPO_DETECCIONES <- paste0("Detecciones (últimos ", meses_recientes,
                              " meses)")

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB") |>
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Imágenes satelitales") |>
    leaflet::addRasterImage(
      recorte, colors = paleta, opacity = 0.7, method = "ngb",
      group = GRUPO_COBERTURA, maxBytes = 32 * 1024^2
    ) |>
    leaflet::addLegend(
      colors = unname(COLORES_WORLDCOVER[as.character(valores)]),
      labels = unname(CLASES_WORLDCOVER[as.character(valores)]),
      title = "Cobertura (2021)", position = "bottomright",
      group = GRUPO_COBERTURA
    ) |>
    # Registro completo como puntos WebGL: cientos de miles de detecciones sin
    # DOM ni popups; el detalle por detección vive en la capa reciente.
    leafgl::addGlPoints(
      data = puntos_wgs84["geometry"],
      fillColor = COLOR_DETECCIONES, fillOpacity = 0.35, radius = 4,
      group = GRUPO_REGISTRO
    )

  # Áreas de conservación, conmutables y ocultas al inicio. Un solo tono (el
  # nombre va en el popup): con la leyenda de WorldCover ya presente, colorear
  # por unidad competiría por la lectura.
  if (!is.null(areas_conservacion)) {
    m <- m |>
      leaflet::addPolygons(
        data = areas_conservacion,
        group = GRUPO_AC,
        fillColor = "#0096a0", fillOpacity = 0.10,
        color = "#00707a", weight = 1.2,
        popup = ~paste0(
          "<strong>", nombre_ac, "</strong>",
          "<br><strong>Siglas:</strong> ", siglas_ac
        )
      )
  }

  # Temporada de fuego consolidada por celda (ver R/temporada.R), conmutable
  # y oculta al inicio: color por longitud, gris bajo el umbral, y los tres
  # índices en el popup como fechas del año de fuego de referencia.
  hay_celdas <- !is.null(celdas_temporada) && nrow(celdas_temporada) > 0
  if (hay_celdas) {
    ref <- inicio_anio_fuego(2002L)
    a_fecha <- function(dia) ifelse(is.na(dia), "—",
                                    fecha_es(ref + dia - 1L, con_anio = FALSE))
    COLOR_SIN_ESTACION <- "#6baed6"
    COLOR_BAJO_UMBRAL  <- "#bdbdbd"
    paleta_lon <- leaflet::colorNumeric(rev(viridisLite::inferno(256)),
                                        domain = celdas_temporada$lon,
                                        na.color = COLOR_BAJO_UMBRAL)
    celdas_wgs84 <- sf::st_transform(celdas_temporada, CRS_WGS84) |>
      dplyr::mutate(
        color_celda = dplyr::case_when(
          sin_estacion ~ COLOR_SIN_ESTACION,
          !valida ~ COLOR_BAJO_UMBRAL,
          TRUE ~ paleta_lon(lon)
        ),
        popup_celda = paste0(
          "<strong>Celda ", celda_id, "</strong>",
          "<br><strong>Detecciones ", anio_inicio, "–", anio_fin, ":</strong> ",
          dtot,
          "<br><strong>Fuera de diciembre a mayo:</strong> ", num_es(fuera, 0), " %",
          ifelse(is.na(n50f), "",
                 paste0("<br><strong>Concentración N50F:</strong> ", num_es(n50f, 2),
                        " (0,5 repartido; hacia 0, en oleadas)")),
          ifelse(is.na(frec), "",
                 paste0("<br><strong>Periodo base ", base_inicio, "–", base_fin,
                        ":</strong> fuego en ", anios, " de ", base_fin - base_inicio + 1,
                        " años (FREC ", num_es(frec, 2), "); ", num_es(dens, 2),
                        " detecciones por km² y año")),
          ifelse(is.na(frpi), "",
                 paste0("<br><strong>FRP mediana:</strong> ", num_es(frpi, 1), " MW")),
          # AQ solo existe en las plataformas con satélite de control (MODIS)
          ifelse(is.na(aq), "",
                 paste0("; <strong>de Aqua (tarde):</strong> ", num_es(100 * aq, 0), " %")),
          dplyr::case_when(
            sin_estacion ~ "<br><em>Sin estación definida (bimodal o fuego todo el año): sin índices</em>",
            !valida ~ "<br><em>Bajo el umbral de detecciones: sin índices</em>",
            TRUE ~ paste0(
              "<br><strong>Inicio (10 %):</strong> ", a_fecha(ini_dia),
              "<br><strong>Fin (90 %):</strong> ", a_fecha(fin_dia),
              "<br><strong>Longitud:</strong> ", lon, " días"
            )
          )
        )
      )
    m <- m |>
      leaflet::addPolygons(
        data = celdas_wgs84, group = GRUPO_TEMPORADA,
        fillColor = ~color_celda, fillOpacity = 0.55,
        color = "#ffffff", weight = 0.6,
        popup = ~popup_celda
      ) |>
      leaflet::addLegend(
        pal = paleta_lon, values = celdas_wgs84$lon[!is.na(celdas_wgs84$lon)],
        title = "Longitud de la<br>temporada (días)", position = "bottomleft",
        group = GRUPO_TEMPORADA, na.label = "Bajo el umbral"
      ) |>
      leaflet::addLegend(
        colors = c(COLOR_SIN_ESTACION, COLOR_BAJO_UMBRAL),
        labels = c("Sin estación definida", "Bajo el umbral"),
        position = "bottomleft", group = GRUPO_TEMPORADA, opacity = 0.55
      )
  }

  # Píxeles de área quemada (ventana reciente), conmutables y ocultos al
  # inicio. Se agregan antes del límite nacional y de las detecciones para
  # quedar debajo de ambos. Los píxeles son rectángulos de 4 vértices: basta
  # transformarlos a WGS84, sin simplificar_para_web(). El deslizador los
  # filtra por su fecha de quema (ver onRender).
  if (hay_quemas) {
    m <- m |>
      leaflet::addPolygons(
        data = quemas_wgs84,
        group = GRUPO_QUEMAS, layerId = ~id_quema,
        fillColor = COLOR_AREA_QUEMADA, fillOpacity = 0.45,
        color = COLOR_AREA_QUEMADA, weight = 1,
        popup = ~paste0(
          "<strong>Fecha de quema:</strong> ", fecha,
          "<br><strong>Área del píxel (ha):</strong> ", num_es(area_ha)
        )
      )
  }

  m <- m |>
    leaflet::addPolygons(
      data = area_web, fill = FALSE, color = "#2b5876", weight = 2,
      label = AREA_NOMBRE, group = GRUPO_LIMITE
    ) |>
    leaflet.extras2::addTimeslider(
      data = recientes,
      radius = 6, color = COLOR_DETECCIONES, stroke = FALSE, fillOpacity = 0.8,
      popupOptions = leaflet::popupOptions(maxWidth = 300),
      popup = ~paste0(
        "<strong>Fecha:</strong> ", acq_date,
        "<br><strong>Hora (UTC):</strong> ", sprintf("%04d", acq_time),
        "<br><strong>FRP (MW):</strong> ", num_es(frp),
        "<br><strong>Confianza:</strong> ", etiqueta_confianza(confidence),
        "<br><strong>Procesamiento:</strong> ", etiqueta_nivel(nivel),
        "<br><strong>Satélite:</strong> ", satellite,
        "<br><strong>Cobertura dominante (2021):</strong> ", clase_cobertura,
        " (", round(fraccion_cobertura * 100), " %)"
      ),
      options = leaflet.extras2::timesliderOptions(
        position = "topright",
        timeAttribute = "time",
        range = TRUE,
        alwaysShowDate = TRUE,
        showAllOnStart = TRUE
      )
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("CartoDB", "Imágenes satelitales"),
      overlayGroups = c(GRUPO_DETECCIONES,
                        GRUPO_REGISTRO,
                        if (hay_quemas) GRUPO_QUEMAS,
                        GRUPO_LIMITE, GRUPO_COBERTURA,
                        if (!is.null(areas_conservacion)) GRUPO_AC,
                        if (hay_celdas) GRUPO_TEMPORADA),
      position = "topright"
    ) |>
    leaflet::hideGroup(c(GRUPO_REGISTRO,
                         if (hay_quemas) GRUPO_QUEMAS,
                         GRUPO_COBERTURA,
                         if (!is.null(areas_conservacion)) GRUPO_AC,
                         if (hay_celdas) GRUPO_TEMPORADA)) |>
    # Botón de pantalla completa con la API Fullscreen del navegador y el
    # plugin EasyButton (incluido en el paquete leaflet base): evita agregar
    # leaflet.extras solo para este control. Safari usa el prefijo webkit.
    leaflet::addEasyButton(leaflet::easyButton(
      position = "topleft",
      icon = "<span style='font-size:18px;'>&#x26F6;</span>",
      title = "Pantalla completa",
      onClick = leaflet::JS("function(btn, map) {
        var cont = map.getContainer();
        if (document.fullscreenElement || document.webkitFullscreenElement) {
          (document.exitFullscreen || document.webkitExitFullscreen).call(document);
        } else {
          (cont.requestFullscreen || cont.webkitRequestFullscreen).call(cont);
        }
      }")
    )) |>
    htmlwidgets::onRender(
      "function(el, x, data) {
        var mapa = this;
        var fechas = data.fechas;
        // El plugin fija el máximo del deslizador en la cantidad de fechas
        // ÚNICAS, pero usa los valores como índices de TODOS los puntos: con
        // fechas repetidas el tirador superior queda recortado. Restaurar el
        // máximo real y el rango completo.
        $('#leaflet-slider').slider('option', 'max', fechas.length - 1);
        $('#leaflet-slider').slider('values', [0, fechas.length - 1]);
        var pintar = function(vals) {
          var a = fechas[vals[0]], b = fechas[vals[1]];
          $('#slider-timestamp')
            .css({padding: '2px 6px', 'font-size': '12px', color: '#333'})
            .html(a === b ? a : a + ' \\u2013 ' + b);
        };
        // Conmutador de detecciones: el grupo del control de capas es un
        // ancla vacía (ver GRUPO_DETECCIONES); aquí se quitan o reponen los
        // marcadores del plugin, respetando el rango vigente del deslizador.
        var deteccionesActivas = true;
        var quitarDetecciones = function() {
          mapa.sliderCntr.options.markers.forEach(function(m) {
            mapa.removeLayer(m);
          });
        };
        var reponerDetecciones = function() {
          var vals = $('#leaflet-slider').slider('values');
          var ms = mapa.sliderCntr.options.markers;
          for (var i = vals[0]; i <= vals[1]; i++) {
            if (ms[i]) mapa.addLayer(ms[i]);
          }
        };
        // Filtro temporal de los píxeles quemados. El plugin del deslizador
        // solo conoce sus propios marcadores, así que los polígonos se
        // muestran u ocultan aquí comparando su fecha de quema contra el
        // rango vigente (las fechas ISO se comparan como texto). Se localizan
        // por layerId a través del layerManager de leaflet, igual de interno
        // que el sliderCntr que ya usa el conmutador de detecciones.
        // El grupo arranca oculto (hideGroup), de ahí quemasActivas = false.
        var hayQuemas = data.idsQuemas && data.idsQuemas.length > 0;
        var quemasActivas = false;
        var filtrarQuemas = function(vals) {
          if (!hayQuemas) return;
          // En los extremos el rango es abierto: MCD64A1 se publica con
          // rezago y sus píxeles más recientes son posteriores a la última
          // detección, así que con el deslizador completo deben verse todos.
          var a = vals[0] === 0 ? '0000-01-01' : fechas[vals[0]];
          var b = vals[1] === fechas.length - 1 ? '9999-12-31' : fechas[vals[1]];
          data.idsQuemas.forEach(function(id, i) {
            var capa = mapa.layerManager.getLayer('shape', id);
            if (!capa) return;
            var f = data.fechasQuemas[i];
            if (quemasActivas && f >= a && f <= b) {
              mapa.addLayer(capa);
            } else {
              mapa.removeLayer(capa);
            }
          });
        };
        var rangoVigente = function() {
          return $('#leaflet-slider').slider('values');
        };
        mapa.on('overlayremove', function(e) {
          if (e.name === data.grupoQuemas) { quemasActivas = false; return; }
          if (e.name !== data.grupoDetecciones) return;
          deteccionesActivas = false;
          quitarDetecciones();
        });
        mapa.on('overlayadd', function(e) {
          if (e.name === data.grupoQuemas) {
            // leaflet repone TODOS los polígonos del grupo: volver a filtrar.
            quemasActivas = true;
            filtrarQuemas(rangoVigente());
            return;
          }
          if (e.name !== data.grupoDetecciones) return;
          deteccionesActivas = true;
          reponerDetecciones();
        });
        // setTimeout(0): correr DESPUÉS del mouseup del plugin, que borra el
        // rótulo y (si la capa está desactivada) vuelve a pintar marcadores.
        var repintar = function() {
          var vals = rangoVigente();
          setTimeout(function() {
            pintar(vals);
            if (!deteccionesActivas) quitarDetecciones();
            filtrarQuemas(vals);
          }, 0);
        };
        pintar([0, fechas.length - 1]);
        $(document).on('mouseup', repintar);
        $('#leaflet-slider').on('slidechange', repintar);
        // El rótulo desborda el control del deslizador; separar el control de
        // capas (mismo rincón topright) para que no lo tape.
        $(el).find('.leaflet-control-layers').css('margin-top', '40px');
        // Al entrar o salir de pantalla completa el contenedor cambia de
        // tamaño sin disparar 'resize' de window; recalcular el mapa.
        ['fullscreenchange', 'webkitfullscreenchange'].forEach(function(ev) {
          el.addEventListener(ev, function() { mapa.invalidateSize(); });
        });
      }",
      # I(): fuerza arreglos JSON aunque haya un solo píxel quemado
      # (auto_unbox convertiría un vector de largo 1 en escalar y el
      # forEach del filtro fallaría).
      data = list(fechas = recientes$time,
                  grupoDetecciones = GRUPO_DETECCIONES,
                  grupoQuemas = GRUPO_QUEMAS,
                  idsQuemas = if (hay_quemas) I(quemas_wgs84$id_quema),
                  fechasQuemas = if (hay_quemas) I(as.character(quemas_wgs84$fecha)))
    )
  m
}

# Guarda el mapa temporal como HTML autocontenido (target con format = "file").
mapa_leaflet_temporal <- function(puntos, area_web, cobertura,
                                  archivos_worldcover, bbox,
                                  areas_conservacion, dest,
                                  etiqueta_quemas, quemas = NULL) {
  m <- crear_mapa_temporal(puntos, area_web, cobertura, archivos_worldcover,
                           bbox, etiqueta_quemas, areas_conservacion, quemas)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  htmlwidgets::saveWidget(m, file.path(normalizePath(dirname(dest)), basename(dest)),
                          selfcontained = TRUE)
  dest
}

# Banda que marca el tramo servido por el procesamiento en tiempo casi real.
# Se dibuja ANTES que las barras para no taparlas. Devuelve NULL si la serie
# es enteramente estándar, de modo que sumarla a un ggplot no cambie nada.
capa_tramo_provisional <- function(mensual) {
  provisionales <- mensual$aniomes[mensual$nivel != "SP"]
  if (length(provisionales) == 0) return(NULL)
  list(
    ggplot2::annotate("rect",
                      xmin = min(provisionales) - 15,
                      xmax = max(provisionales) + 15,
                      ymin = -Inf, ymax = Inf,
                      fill = "grey60", alpha = 0.18),
    ggplot2::annotate("text", x = min(provisionales) - 25, y = Inf,
                      label = "provisional →", hjust = 1, vjust = 1.6,
                      size = 2.7, color = "grey35")
  )
}

# Serie temporal mensual de detecciones (una serie: sin leyenda, un solo tono).
grafico_serie_temporal <- function(mensual, dest, etiqueta_fuente, fuente) {
  p <- ggplot2::ggplot(mensual, ggplot2::aes(x = aniomes, y = detecciones)) +
    capa_tramo_provisional(mensual) +
    ggplot2::geom_col(fill = COLOR_DETECCIONES, width = 25) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(
      title = "Detecciones mensuales de anomalías térmicas",
      subtitle = paste0(AREA_NOMBRE, " — ", etiqueta_fuente),
      x = NULL, y = "Detecciones por mes",
      caption = fuente
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92"),
      plot.title = ggplot2::element_text(face = "bold")
    )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 10, height = 4.5, dpi = 200)
  dest
}

# Climatología mensual: promedio de detecciones por mes calendario
# (estacionalidad de la época seca).
grafico_climatologia <- function(mensual, dest, etiqueta_fuente, fuente) {
  clima <- mensual |>
    dplyr::summarise(promedio = mean(detecciones), .by = mes) |>
    dplyr::mutate(nombre_mes = factor(MESES_ES[mes], levels = MESES_ES))
  p <- ggplot2::ggplot(clima, ggplot2::aes(x = nombre_mes, y = promedio)) +
    ggplot2::geom_col(fill = COLOR_DETECCIONES, width = 0.7) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(
      title = "Climatología mensual de anomalías térmicas",
      subtitle = paste0("Promedio de detecciones por mes calendario — ", AREA_NOMBRE, ", ",
                        etiqueta_fuente),
      x = NULL, y = "Detecciones promedio",
      caption = fuente
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92"),
      plot.title = ggplot2::element_text(face = "bold")
    )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 8, height = 4.5, dpi = 200)
  dest
}

# --- Versiones interactivas (plotly) ----------------------------------------
# Los constructores retornan el widget (para incrustar en el reporte Quarto);
# los envoltorios grafico_*_html() lo guardan como HTML autocontenido.
# ggplotly no traslada subtitle/caption de ggplot, por lo que el subtítulo va
# como segunda línea del título (<sup>) y la fuente como anotación.

# Configuración común: barra de herramientas mínima y UI en español.
# `margen_superior` debe crecer cuando el gráfico lleva leyenda arriba: la
# leyenda se coloca justo sobre el área de trazado y con el margen por
# defecto se traslapa con el subtítulo.
configurar_plotly <- function(w, titulo, subtitulo, fuente,
                              margen_superior = 70, margen_inferior = 70,
                              desplazamiento_fuente = -50) {
  w |>
    plotly::layout(
      # Sin `y`/`yanchor`: plotly ubica el título dentro del margen superior;
      # anclarlo explícitamente lo empuja fuera del contenedor.
      title = list(
        text = paste0("<b>", titulo, "</b><br><sup>", subtitulo, "</sup>"),
        x = 0, xanchor = "left", font = list(size = 18)
      ),
      margin = list(t = margen_superior, b = margen_inferior),
      # La fuente se ancla al borde inferior del área de trazado y se desplaza
      # en PÍXELES (yshift): con un desplazamiento relativo (y = -0.16) su
      # posición depende de la altura del gráfico y termina chocando con el
      # rótulo del eje x. `desplazamiento_fuente` debe superar la altura de
      # ese rótulo y caber dentro de `margen_inferior`.
      annotations = list(
        text = fuente, xref = "paper", yref = "paper",
        x = 1, y = 0, xanchor = "right", yanchor = "top",
        yshift = desplazamiento_fuente,
        showarrow = FALSE, font = list(size = 12, color = "grey")
      )
    ) |>
    plotly::config(
      locale = "es", displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d")
    )
}

# Serie temporal mensual interactiva (zoom/pan sobre todo el registro).
crear_serie_interactiva <- function(mensual, etiqueta_fuente, fuente) {
  datos <- mensual |>
    dplyr::mutate(etiqueta = paste0(
      "Mes: ", MESES_ES[mes], " ", format(aniomes, "%Y"),
      "<br>Detecciones: ", detecciones
    ))
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = aniomes, y = detecciones,
                                           text = etiqueta)) +
    ggplot2::geom_col(fill = COLOR_DETECCIONES, width = 25) +
    ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = NULL, y = "Detecciones por mes") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92")
    )
  plotly::ggplotly(p, tooltip = "text") |>
    configurar_plotly(
      "Detecciones mensuales de anomalías térmicas",
      paste0(AREA_NOMBRE, " — ", etiqueta_fuente),
      fuente = fuente
    )
}

# Climatología mensual interactiva.
crear_climatologia_interactiva <- function(mensual, etiqueta_fuente, fuente) {
  clima <- mensual |>
    dplyr::summarise(promedio = mean(detecciones), .by = mes) |>
    dplyr::mutate(
      nombre_mes = factor(MESES_ES[mes], levels = MESES_ES),
      etiqueta = paste0("Mes: ", nombre_mes,
                        "<br>Promedio: ", num_es(promedio))
    )
  p <- ggplot2::ggplot(clima, ggplot2::aes(x = nombre_mes, y = promedio,
                                           text = etiqueta)) +
    ggplot2::geom_col(fill = COLOR_DETECCIONES, width = 0.7) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = NULL, y = "Detecciones promedio") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92")
    )
  plotly::ggplotly(p, tooltip = "text") |>
    configurar_plotly(
      "Climatología mensual de anomalías térmicas",
      paste0("Promedio de detecciones por mes calendario — ", AREA_NOMBRE, ", ",
             etiqueta_fuente),
      fuente = fuente
    )
}

# Guardan los gráficos interactivos como HTML autocontenido
# (targets con format = "file", mismo patrón que mapa_leaflet_temporal).
grafico_serie_html <- function(mensual, dest, etiqueta_fuente, fuente) {
  w <- crear_serie_interactiva(mensual, etiqueta_fuente, fuente)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  htmlwidgets::saveWidget(w, file.path(normalizePath(dirname(dest)), basename(dest)),
                          selfcontained = TRUE)
  dest
}

grafico_climatologia_html <- function(mensual, dest, etiqueta_fuente, fuente) {
  w <- crear_climatologia_interactiva(mensual, etiqueta_fuente, fuente)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  htmlwidgets::saveWidget(w, file.path(normalizePath(dirname(dest)), basename(dest)),
                          selfcontained = TRUE)
  dest
}

# Las cuatro series mensuales en facetas con eje temporal común y escala y
# libre. Es el argumento visual de por qué las series no se suman: hace
# evidente que cada plataforma cubre un tramo distinto y detecta a un ritmo
# distinto sobre el mismo territorio.
grafico_series_plataformas <- function(mensuales, etiquetas, dest) {
  datos <- purrr::map2(mensuales, etiquetas, function(m, e) {
    dplyr::mutate(m[, c("aniomes", "detecciones")], plataforma = e$plataforma)
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(plataforma = factor(plataforma, levels = purrr::map_chr(etiquetas, "plataforma")))
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = aniomes, y = detecciones)) +
    ggplot2::geom_col(fill = COLOR_DETECCIONES, width = 25) +
    ggplot2::facet_wrap(~plataforma, ncol = 1, scales = "free_y") +
    ggplot2::scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(
      title = "Detecciones mensuales según la plataforma que observa",
      subtitle = paste("El eje temporal es común; cada panel tiene su propia",
                       "escala vertical.\nLas series no son sumables entre sí."),
      x = NULL, y = "Detecciones por mes",
      caption = "Datos: NASA FIRMS (MODIS y VIIRS)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92"),
      plot.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 9, height = 8, dpi = 200)
  dest
}
