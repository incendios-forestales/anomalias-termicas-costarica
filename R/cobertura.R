# Cobertura de la tierra (ESA WorldCover 10 m, 2021) en los footprints de
# las detecciones MODIS y en los píxeles de área quemada (MCD64A1).
#
# Método: cada detección MODIS es un píxel de ~1 km (mayor fuera del nadir);
# las columnas scan/track dan sus dimensiones reales en km. Asignar la clase
# del punto exacto sobre un mapa de 10 m sería precisión espuria, por lo que
# se calcula la composición de clases dentro de un footprint elíptico de ejes
# scan x track (aproximado como alineado a los ejes: el eje de barrido de MODIS
# es aproximadamente este-oeste en estas latitudes) y se reporta la fracción
# por clase y la clase dominante.
#
# Limitación documentada en el reporte: WorldCover es una foto fija de 2021
# frente a un registro de detecciones 2001-2026.

WORLDCOVER_BASE <- "https://esa-worldcover.s3.eu-central-1.amazonaws.com/v200/2021/map"

# Clases WorldCover v200 (valor del píxel -> etiqueta en español).
CLASES_WORLDCOVER <- c(
  `10`  = "Bosque",
  `20`  = "Matorral",
  `30`  = "Pastizal",
  `40`  = "Cultivos",
  `50`  = "Zonas construidas",
  `60`  = "Suelo desnudo / vegetación escasa",
  `70`  = "Nieve y hielo",
  `80`  = "Cuerpos de agua",
  `90`  = "Humedal herbáceo",
  `95`  = "Manglar",
  `100` = "Musgos y líquenes"
)

# Paleta oficial de WorldCover (mismos valores de píxel que CLASES_WORLDCOVER).
COLORES_WORLDCOVER <- c(
  `10`  = "#006400",
  `20`  = "#ffbb22",
  `30`  = "#ffff4c",
  `40`  = "#f096ff",
  `50`  = "#fa0000",
  `60`  = "#b4b4b4",
  `70`  = "#f0f0f0",
  `80`  = "#0064c8",
  `90`  = "#0096a0",
  `95`  = "#00cf75",
  `100` = "#fae6a0"
)

# Raster WorldCover recortado a un bbox WGS84 c(oeste, sur, este, norte).
recortar_worldcover <- function(archivos, bbox) {
  capa <- if (length(archivos) > 1) {
    terra::vrt(archivos)
  } else {
    terra::rast(archivos)
  }
  terra::crop(
    capa,
    terra::ext(bbox["oeste"], bbox["este"], bbox["sur"], bbox["norte"])
  )
}

# Fondo de cobertura para el mapa animado: imagen RGBA pre-renderizada en
# CRTM05 (una capa de celdas por cuadro haría lentísimo el render de ~300
# cuadros de gganimate; annotation_raster dibuja un bitmap y es barato).
# La agregación (modal) se calcula desde el ancho del bbox para que la matriz
# resultante tenga ~ancho_px columnas: a escala nacional el recorte a 10 m
# tiene ~46 000 columnas (~2·10⁹ celdas) y materializarlo como matriz de
# colores agotaría la memoria. Se atenúa con transparencia para no competir
# con los puntos de detección.
fondo_cobertura_animacion <- function(archivos, bbox, alfa = 0.5,
                                      ancho_px = 1000) {
  recorte <- recortar_worldcover(archivos, bbox)
  fact <- max(1, ceiling(terra::ncol(recorte) / ancho_px))
  if (fact > 1) {
    recorte <- terra::aggregate(recorte, fact = fact, fun = "modal",
                                na.rm = TRUE)
  }
  recorte <- terra::project(recorte, CRS_CRTM05, method = "near")
  celdas <- terra::as.matrix(recorte, wide = TRUE)
  colores <- grDevices::adjustcolor(
    COLORES_WORLDCOVER[as.character(celdas)], alpha.f = alfa
  )
  colores[is.na(celdas)] <- "#00000000"
  extension <- terra::ext(recorte)
  list(
    imagen = grDevices::as.raster(matrix(colores, nrow = nrow(celdas))),
    xmin = extension$xmin, xmax = extension$xmax,
    ymin = extension$ymin, ymax = extension$ymax
  )
}

# Composición de clases WorldCover dentro del área de estudio: el paisaje
# disponible contra el cual se comparan las detecciones. A escala nacional se
# calcula sobre el raster agregado por moda a ~100 m: la composición
# porcentual del país no cambia con la resolución y el mask a 10 m costaría
# gigas de E/S por una cifra idéntica.
composicion_paisaje <- function(area, archivos_worldcover, bbox) {
  recorte <- recortar_worldcover(archivos_worldcover, bbox) |>
    terra::aggregate(fact = 10, fun = "modal", na.rm = TRUE)
  poligono <- terra::vect(sf::st_transform(area, terra::crs(recorte)))
  conteo <- terra::freq(terra::mask(terra::crop(recorte, poligono), poligono))
  tibble::tibble(
    clase = unname(CLASES_WORLDCOVER[as.character(conteo$value)]),
    pct_paisaje = 100 * conteo$count / sum(conteo$count)
  ) |>
    dplyr::arrange(dplyr::desc(pct_paisaje))
}

# Nombres de las teselas de 3x3 grados que intersecan un bbox WGS84
# c(oeste, sur, este, norte) — el formato de bbox_con_buffer()
# (esquina suroeste, p. ej. "N09W087").
teselas_worldcover <- function(bbox) {
  lons <- seq(floor(bbox["oeste"] / 3) * 3, floor(bbox["este"] / 3) * 3, by = 3)
  lats <- seq(floor(bbox["sur"] / 3) * 3, floor(bbox["norte"] / 3) * 3, by = 3)
  rejilla <- expand.grid(lon = lons, lat = lats)
  sprintf(
    "%s%02d%s%03d",
    ifelse(rejilla$lat < 0, "S", "N"), abs(rejilla$lat),
    ifelse(rejilla$lon < 0, "W", "E"), abs(rejilla$lon)
  )
}

# Descarga cacheada de las teselas WorldCover que cubren el bbox.
# ESA no publica teselas 100 % oceánicas: un HTTP 404 significa que la tesela
# no existe (bbox que se asoma al mar) y se omite con un mensaje; cualquier
# otro fallo de descarga sigue siendo un error. Retorna los archivos locales
# de las teselas existentes.
descargar_worldcover <- function(bbox_wgs84, dir_destino = "data/raw/worldcover") {
  archivos <- lapply(teselas_worldcover(bbox_wgs84), function(tesela) {
    nombre <- glue::glue("ESA_WorldCover_10m_2021_v200_{tesela}_Map.tif")
    url <- glue::glue("{WORLDCOVER_BASE}/{nombre}")
    destino <- file.path(dir_destino, nombre)
    tryCatch(
      download_if_missing(url, destino),
      error = function(e) {
        estado <- tryCatch(
          httr2::request(url) |>
            httr2::req_method("HEAD") |>
            httr2::req_error(is_error = function(r) FALSE) |>
            httr2::req_perform() |>
            httr2::resp_status(),
          error = function(e2) NA_integer_
        )
        if (identical(estado, 404L)) {
          message(glue::glue("[omitida] tesela {tesela} no existe (océano)"))
          if (file.exists(destino)) unlink(destino)  # residuo de download.file
          return(NULL)
        }
        stop(e)
      }
    )
  })
  archivos <- unlist(Filter(Negate(is.null), archivos))
  if (length(archivos) == 0) {
    stop("Ninguna tesela WorldCover disponible para el bbox solicitado.",
         call. = FALSE)
  }
  archivos
}

# Footprint elíptico de cada detección: ejes scan (E-O) x track (N-S) en km,
# construido en CRTM05 (métrico) escalando un círculo unitario.
# nQuadSegs = 8 (33 vértices por elipse): a escala nacional hay 10⁵-10⁶
# detecciones y el default (~120 vértices) cuadruplicaría la memoria sin
# ganar precisión frente a un ráster de 10 m.
footprints_detecciones <- function(puntos) {
  centros <- sf::st_transform(puntos, CRS_CRTM05)
  geoms <- sf::st_geometry(centros)
  circulos <- sf::st_buffer(geoms, dist = 1, nQuadSegs = 8)
  elipses <- mapply(function(circulo, centro, scan_km, track_km) {
    (circulo - centro) * diag(c(scan_km, track_km) * 1000 / 2) + centro
  }, circulos, geoms, centros$scan, centros$track, SIMPLIFY = FALSE)
  sf::st_set_geometry(
    centros,
    sf::st_sfc(elipses, crs = sf::st_crs(centros))
  )
}

# Identificador de cada polígono para los joins de cobertura: la columna
# id_deteccion si existe (detecciones, vía a_sf_puntos()); el número de fila
# en su defecto (píxeles de área quemada). NUNCA se asume que id == fila.
ids_de_poligonos <- function(poligonos) {
  if ("id_deteccion" %in% names(poligonos)) {
    poligonos$id_deteccion
  } else {
    seq_len(nrow(poligonos))
  }
}

# Núcleo compartido: fracción de cada clase de cobertura dentro de cada
# polígono (footprint elíptico de detección o píxel de área quemada).
# exact_extract lee el VRT por bloques; se procesa por lotes para acotar el
# data frame ancho intermedio con cientos de miles de polígonos.
fracciones_cobertura <- function(poligonos, archivos_worldcover,
                                 tamano_lote = 50000L) {
  capa <- if (length(archivos_worldcover) > 1) {
    terra::vrt(archivos_worldcover)
  } else {
    terra::rast(archivos_worldcover)
  }
  ids <- ids_de_poligonos(poligonos)
  poligonos <- sf::st_transform(poligonos, sf::st_crs(capa))

  lotes <- split(seq_len(nrow(poligonos)),
                 ceiling(seq_len(nrow(poligonos)) / tamano_lote))
  purrr::map(lotes, function(filas) {
    exactextractr::exact_extract(
      capa, poligonos[filas, ], fun = "frac", progress = FALSE
    ) |>
      dplyr::mutate(id_deteccion = ids[filas]) |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("frac_"),
        names_to = "clase_valor", names_prefix = "frac_",
        values_to = "fraccion"
      ) |>
      dplyr::filter(fraccion > 0)
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(clase = CLASES_WORLDCOVER[clase_valor])
}

# Fracción de cada clase dentro del footprint de cada detección. Retorna un
# data frame sin geometría, una fila por detección y clase presente.
extraer_cobertura <- function(puntos, archivos_worldcover) {
  fracciones_cobertura(footprints_detecciones(puntos), archivos_worldcover) |>
    dplyr::left_join(
      tibble::tibble(id_deteccion = puntos$id_deteccion,
                     acq_date = puntos$acq_date, frp = puntos$frp),
      by = "id_deteccion"
    ) |>
    dplyr::relocate(id_deteccion, acq_date, frp)
}

# Fracción de cada clase dentro de cada píxel de área quemada (el píxel
# MCD64A1 de 500 m YA es el footprint: no hay elipse que construir).
extraer_cobertura_quemas <- function(quemas, archivos_worldcover) {
  if (nrow(quemas) == 0) {
    return(tibble::tibble(id_deteccion = integer(), fecha = as.Date(character()),
                          area_ha = numeric(), clase_valor = character(),
                          fraccion = numeric(), clase = character()))
  }
  fracciones_cobertura(quemas, archivos_worldcover) |>
    dplyr::left_join(
      tibble::tibble(id_deteccion = seq_len(nrow(quemas)),
                     fecha = quemas$fecha, area_ha = quemas$area_ha),
      by = "id_deteccion"
    ) |>
    dplyr::relocate(id_deteccion, fecha, area_ha)
}

# Clase dominante (mayor fracción del footprint o píxel) por fila de entrada.
# any_of(): arrastra los metadatos presentes según la fuente (acq_date/frp en
# detecciones; fecha/area_ha en área quemada).
clase_dominante <- function(cobertura) {
  cobertura |>
    dplyr::slice_max(fraccion, n = 1, by = id_deteccion, with_ties = FALSE) |>
    dplyr::select(dplyr::any_of(c("id_deteccion", "acq_date", "frp",
                                  "fecha", "area_ha")), clase, fraccion)
}

# Resumen por clase: detecciones donde la clase domina el footprint y
# fracción promedio del footprint que ocupa (sobre todas las detecciones).
resumen_cobertura <- function(cobertura) {
  dominantes <- clase_dominante(cobertura) |>
    dplyr::count(clase, name = "detecciones_dominante")
  n_detecciones <- dplyr::n_distinct(cobertura$id_deteccion)
  cobertura |>
    dplyr::summarise(
      fraccion_promedio = sum(fraccion) / n_detecciones,
      .by = clase
    ) |>
    dplyr::left_join(dominantes, by = "clase") |>
    dplyr::mutate(
      detecciones_dominante = dplyr::coalesce(detecciones_dominante, 0L)
    ) |>
    dplyr::arrange(dplyr::desc(fraccion_promedio))
}

# --- Contraste con las capas nacionales del SINAC ---------------------------
# WorldCover clasifica como "Pastizal" buena parte de la vegetación herbácea
# inundable (p. ej. las marismas del Tempisque). Se contrasta cada footprint
# contra el Registro Nacional de Humedales para verificar si esas detecciones
# ocurren en humedal registrado.
#
# Criterio: mismo footprint elíptico del análisis WorldCover (no el punto), y
# se considera "en humedal" si el footprint interseca algún polígono del
# registro; se reporta además la fracción del footprint cubierta por humedal.
#
# Limitación: las capas nacionales también son fotos fijas (registro de
# humedales sin fecha uniforme) frente a 2001-2026.
# Núcleo del cruce: solo usa la geometría y el id de `poligonos` (footprints
# elípticos o píxeles de quema, cualquier CRS proyectable). A escala nacional
# los footprints son cientos de miles y los humedales miles de polígonos:
# la intersección geométrica (costosa) se calcula solo para los footprints
# que el índice espacial marca como candidatos.
cruce_humedales <- function(poligonos, humedales) {
  ids <- ids_de_poligonos(poligonos)
  poligonos <- a_crtm05(poligonos)
  humedales <- sf::st_make_valid(
    a_crtm05(humedales)[, c("nom_hum", "tipo_hum", "clase_hum")]
  )
  geoms <- sf::st_make_valid(
    sf::st_sf(id_deteccion = ids, geometry = sf::st_geometry(poligonos))
  )

  candidatos <- lengths(sf::st_intersects(geoms, humedales)) > 0
  interseccion <- sf::st_intersection(geoms[candidatos, ], humedales)
  areas <- as.numeric(sf::st_area(geoms))

  resumen <- interseccion |>
    dplyr::mutate(area_humedal = as.numeric(sf::st_area(interseccion))) |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      # Clase del humedal que más área aporta al footprint
      clase_hum = clase_hum[which.max(area_humedal)],
      tipo_hum  = tipo_hum[which.max(area_humedal)],
      nom_hum   = nom_hum[which.max(area_humedal)],
      area_humedal = sum(area_humedal),
      .by = id_deteccion
    ) |>
    dplyr::mutate(
      fraccion_humedal = pmin(area_humedal / areas[match(id_deteccion, ids)], 1)
    )

  data.frame(id_deteccion = ids) |>
    dplyr::left_join(resumen, by = "id_deteccion") |>
    dplyr::mutate(
      en_humedal = !is.na(clase_hum),
      fraccion_humedal = tidyr::replace_na(fraccion_humedal, 0)
    )
}

cruzar_con_humedales <- function(puntos, humedales) {
  cruce_humedales(footprints_detecciones(puntos), humedales)
}

# Para el área quemada el polígono del píxel ya es el footprint.
cruzar_quemas_con_humedales <- function(quemas, humedales) {
  cruce_humedales(quemas, humedales)
}

# Contraste WorldCover vs. humedales registrados: por clase dominante de
# WorldCover, cuántas detecciones caen en humedal y con qué cobertura.
contraste_humedales <- function(cobertura, humedales_detecciones) {
  clase_dominante(cobertura) |>
    dplyr::left_join(humedales_detecciones, by = "id_deteccion") |>
    dplyr::summarise(
      detecciones = dplyr::n(),
      # `pct` antes de crear la columna `en_humedal`: summarise() permite
      # referirse a columnas recién creadas y el conteo enmascararía al lógico.
      pct_en_humedal = round(100 * mean(en_humedal), 1),
      en_humedal = sum(en_humedal),
      fraccion_humedal_promedio = round(mean(fraccion_humedal), 3),
      .by = clase
    ) |>
    dplyr::relocate(en_humedal, .after = detecciones) |>
    dplyr::arrange(dplyr::desc(detecciones))
}

# Hectáreas quemadas por clase dominante del píxel: análogo de
# contraste_humedales() para el área quemada, ponderado por area_ha en lugar
# de contar filas.
contraste_quemas_por_clase <- function(cobertura_quemas, humedales_quemas) {
  clase_dominante(cobertura_quemas) |>
    dplyr::left_join(humedales_quemas, by = "id_deteccion") |>
    dplyr::summarise(
      hectareas_quemadas = round(sum(area_ha)),
      pct_quemado_en_humedal = round(100 * sum(area_ha[en_humedal]) /
                                       sum(area_ha), 1),
      .by = clase
    )
}

# Contraste combinado detecciones + área quemada, por clase de WorldCover.
# full_join: una clase que solo domina en quemas (o solo en detecciones) no
# debe perderse; los porcentajes quedan NA donde la fuente no aporta filas.
contraste_completo <- function(cobertura, humedales_detecciones,
                               cobertura_quemas, humedales_quemas) {
  contraste_humedales(cobertura, humedales_detecciones) |>
    dplyr::full_join(
      contraste_quemas_por_clase(cobertura_quemas, humedales_quemas),
      by = "clase"
    ) |>
    dplyr::mutate(dplyr::across(c(detecciones, en_humedal, hectareas_quemadas),
                                \(x) tidyr::replace_na(x, 0))) |>
    dplyr::arrange(dplyr::desc(detecciones))
}

# Tabla CSV del contraste (target con format = "file").
tabla_contraste_csv <- function(cobertura, humedales_detecciones,
                                cobertura_quemas, humedales_quemas, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  contraste_completo(cobertura, humedales_detecciones,
                     cobertura_quemas, humedales_quemas) |>
    readr::write_csv(dest)
  dest
}

# Tabla CSV del resumen por clase (target con format = "file").
tabla_cobertura_csv <- function(cobertura, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  resumen_cobertura(cobertura) |>
    dplyr::mutate(fraccion_promedio = round(fraccion_promedio, 3)) |>
    readr::write_csv(dest)
  dest
}

# Color de la condición "en humedal": el mismo teal de la capa de humedales
# del mapa interactivo, para lectura consistente entre productos. El de
# "fuera de humedal" es COLOR_DETECCIONES, referido dentro de la función
# porque tar_source() carga este archivo antes que visualizacion.R.
COLOR_EN_HUMEDAL <- "#0096a0"

# Detecciones por clase de cobertura dominante, divididas según caigan o no
# en un humedal registrado. El humedal no es un tipo de vegetación que compita
# con "pastizal" o "bosque" (las capas del SINAC no cubren todo el territorio,
# ver README): es una condición del sitio, y por eso se representa como
# partición de cada barra y no como una clase más.
conteo_cobertura_humedal <- function(cobertura, humedales_detecciones) {
  clase_dominante(cobertura) |>
    dplyr::left_join(humedales_detecciones, by = "id_deteccion") |>
    dplyr::count(clase, en_humedal, name = "detecciones") |>
    dplyr::mutate(
      condicion = factor(
        ifelse(en_humedal, "En humedal registrado", "Fuera de humedal"),
        levels = c("En humedal registrado", "Fuera de humedal")
      )
    )
}

# Gráfico de barras apiladas: detecciones por clase de cobertura dominante,
# segmentadas por condición de humedal.
crear_grafico_cobertura <- function(cobertura, humedales_detecciones, fuente,
                                    interactivo = FALSE) {
  conteos <- conteo_cobertura_humedal(cobertura, humedales_detecciones)
  orden <- conteos |>
    dplyr::summarise(total = sum(detecciones), .by = clase) |>
    dplyr::arrange(total)
  datos <- conteos |>
    dplyr::mutate(
      clase = factor(clase, levels = orden$clase),
      etiqueta = paste0(
        "Clase: ", clase,
        "<br>", condicion,
        "<br>Detecciones: ", detecciones
      )
    )
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = detecciones, y = clase,
                                           fill = condicion, text = etiqueta)) +
    # El separador blanco entre segmentos evita que se lean como una sola barra.
    # reverse = TRUE: el segmento "en humedal" arranca en cero, en el mismo
    # orden en que aparece en la leyenda.
    ggplot2::geom_col(width = 0.7, linewidth = 0.7, color = "white",
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(
      values = c("En humedal registrado" = COLOR_EN_HUMEDAL,
                 "Fuera de humedal" = COLOR_DETECCIONES),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "Detecciones", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey92"),
      legend.position = "top"
    )
  if (interactivo) {
    plotly::ggplotly(p, tooltip = "text") |>
      configurar_plotly(
        "Detecciones por cobertura de la tierra y condición de humedal",
        paste("Clase dominante en el footprint — WorldCover 2021;",
              "humedales: Registro Nacional (SINAC)"),
        fuente = fuente,
        # El eje x lleva rótulo ("Detecciones"), por eso la fuente baja más
        margen_superior = 130, margen_inferior = 110,
        desplazamiento_fuente = -78
      ) |>
      plotly::layout(legend = list(orientation = "h", x = 0,
                                   y = 1.02, yanchor = "bottom"))
  } else {
    p + ggplot2::labs(
      title = "Detecciones por cobertura de la tierra y condición de humedal",
      subtitle = paste("Clase dominante en el footprint — WorldCover 2021;",
                       "humedales: Registro Nacional (SINAC)"),
      caption = fuente
    ) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }
}

# PNG del gráfico de cobertura (target con format = "file").
grafico_cobertura <- function(cobertura, humedales_detecciones, dest, fuente) {
  p <- crear_grafico_cobertura(cobertura, humedales_detecciones, fuente,
                               interactivo = FALSE)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 8, height = 4.5, dpi = 200)
  dest
}

# Hectáreas quemadas por clase de cobertura dominante del píxel, divididas
# por condición de humedal (gemelo de conteo_cobertura_humedal, ponderado
# por area_ha).
hectareas_cobertura_humedal <- function(cobertura_quemas, humedales_quemas) {
  clase_dominante(cobertura_quemas) |>
    dplyr::left_join(humedales_quemas, by = "id_deteccion") |>
    dplyr::summarise(hectareas = sum(area_ha), .by = c(clase, en_humedal)) |>
    dplyr::mutate(
      condicion = factor(
        ifelse(en_humedal, "En humedal registrado", "Fuera de humedal"),
        levels = c("En humedal registrado", "Fuera de humedal")
      )
    )
}

# Gemelo del gráfico de cobertura para el área quemada: hectáreas en lugar de
# detecciones. "Fuera de humedal" usa COLOR_AREA_QUEMADA (morado): identifica
# a MCD64A1 en todo el reporte y evita leer hectáreas como si fueran
# detecciones (naranja); el teal de "en humedal" se mantiene para lectura
# consistente de la condición. COLOR_AREA_QUEMADA se refiere dentro de la
# función (tar_source() carga cobertura.R después de area_quemada.R, pero el
# patrón del archivo es no depender del orden de carga).
crear_grafico_cobertura_quemas <- function(cobertura_quemas, humedales_quemas,
                                           fuente, interactivo = FALSE) {
  hectareas <- hectareas_cobertura_humedal(cobertura_quemas, humedales_quemas)
  orden <- hectareas |>
    dplyr::summarise(total = sum(hectareas), .by = clase) |>
    dplyr::arrange(total)
  datos <- hectareas |>
    dplyr::mutate(
      clase = factor(clase, levels = orden$clase),
      etiqueta = paste0(
        "Clase: ", clase,
        "<br>", condicion,
        "<br>Área quemada: ", round(hectareas), " ha"
      )
    )
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = hectareas, y = clase,
                                           fill = condicion, text = etiqueta)) +
    ggplot2::geom_col(width = 0.7, linewidth = 0.7, color = "white",
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(
      values = c("En humedal registrado" = COLOR_EN_HUMEDAL,
                 "Fuera de humedal" = COLOR_AREA_QUEMADA),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::labs(x = "Hectáreas quemadas", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey92"),
      legend.position = "top"
    )
  titulo <- "Área quemada por cobertura de la tierra y condición de humedal"
  subtitulo <- paste("Clase dominante en el píxel de 500 m — WorldCover 2021;",
                     "humedales: Registro Nacional (SINAC)")
  if (interactivo) {
    plotly::ggplotly(p, tooltip = "text") |>
      configurar_plotly(
        titulo, subtitulo, fuente = fuente,
        margen_superior = 130, margen_inferior = 110,
        desplazamiento_fuente = -78
      ) |>
      plotly::layout(legend = list(orientation = "h", x = 0,
                                   y = 1.02, yanchor = "bottom"))
  } else {
    p + ggplot2::labs(title = titulo, subtitle = subtitulo, caption = fuente) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }
}

# PNG del gráfico de cobertura del área quemada (target con format = "file").
grafico_cobertura_quemas <- function(cobertura_quemas, humedales_quemas, dest,
                                     fuente) {
  p <- crear_grafico_cobertura_quemas(cobertura_quemas, humedales_quemas,
                                      fuente, interactivo = FALSE)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 8, height = 4.5, dpi = 200)
  dest
}
