# Año de fuego e índices anuales de la temporada de fuego.
#
# Implementa la sección «Año de fuego e índices anuales» del README, que es el
# contrato: las definiciones se escribieron antes que este código. Primer
# índice: longitud de la temporada (LON), con sus componentes INI y FIN, los
# primeros días del año de fuego en que la suma acumulada de detecciones
# alcanza el 10 % y el 90 % del total anual. Los parámetros viven en
# R/constantes.R (MES_INICIO_ANIO_FUEGO, TEMPORADA_*).
#
# Todo opera sobre detecciones de VEGETACIÓN (filtrar_vegetacion) y sobre una
# serie diaria con ceros explícitos construida desde `rangos`, no desde las
# fechas con detecciones: un día sin detecciones es un cero informativo.

# --- Año de fuego -----------------------------------------------------------

# Año de fuego de una fecha: el año calendario en que termina el periodo
# septiembre-agosto que la contiene (2023-09-01 y 2024-05-01 son ambos 2024).
anio_fuego <- function(fecha) {
  lubridate::year(fecha) +
    as.integer(lubridate::month(fecha) >= MES_INICIO_ANIO_FUEGO)
}

# Primer y último día de un año de fuego. Vectorizados y tolerantes a NA (un
# INI ausente en un año sin detecciones no debe abortar el cálculo).
inicio_anio_fuego <- function(anio) {
  out <- rep(as.Date(NA), length(anio))
  ok <- !is.na(anio)
  out[ok] <- as.Date(sprintf("%d-%02d-01", anio[ok] - 1L,
                             MES_INICIO_ANIO_FUEGO))
  out
}

fin_anio_fuego <- function(anio) {
  inicio_anio_fuego(anio + 1L) - 1
}

# Día dentro del año de fuego (1 = 1 de septiembre; 365 o 366 = 31 de agosto).
dia_anio_fuego <- function(fecha) {
  as.integer(fecha - inicio_anio_fuego(anio_fuego(fecha))) + 1L
}

# --- Detecciones incluidas ---------------------------------------------------

# Detecciones de vegetación: `type` 0 o ausente. La cola NRT de FIRMS no trae
# el campo, y una plataforma sin procesamiento estándar puede no traer la
# columna: en ambos casos "ausente" cuenta como vegetación (ver README).
es_vegetacion <- function(puntos) {
  if (!"type" %in% names(puntos)) return(rep(TRUE, nrow(puntos)))
  is.na(puntos$type) | puntos$type == 0L
}

filtrar_vegetacion <- function(puntos) {
  puntos[es_vegetacion(puntos), , drop = FALSE]
}

# Detecciones por año de fuego y tipo de fuente, en formato ancho con todas
# las categorías siempre presentes (un año sin volcanes muestra 0, no una
# columna que desaparece). `sin_tipo` es la cola NRT.
resumen_tipos <- function(puntos) {
  categorias <- c("vegetacion", "sin_tipo", "volcan_activo",
                  "fuente_estatica", "mar")
  df <- sf::st_drop_geometry(puntos)
  if (!"type" %in% names(df)) df$type <- NA_integer_
  ancho <- df |>
    dplyr::mutate(
      anio_fuego = anio_fuego(acq_date),
      categoria = dplyr::case_when(
        is.na(type) ~ "sin_tipo",
        type == 0L  ~ "vegetacion",
        type == 1L  ~ "volcan_activo",
        type == 2L  ~ "fuente_estatica",
        type == 3L  ~ "mar"
      )
    ) |>
    dplyr::count(anio_fuego, categoria) |>
    tidyr::pivot_wider(names_from = categoria, values_from = n,
                       values_fill = 0L)
  for (col in setdiff(categorias, names(ancho))) ancho[[col]] <- 0L
  ancho |>
    dplyr::mutate(excluidas = volcan_activo + fuente_estatica + mar) |>
    dplyr::select(anio_fuego, dplyr::all_of(categorias), excluidas) |>
    dplyr::arrange(anio_fuego)
}

# --- Serie diaria ------------------------------------------------------------

# Una fila por día observado por la plataforma (de `rangos`), con el número de
# detecciones (0 en los días sin ninguna), el nivel de procesamiento del día
# (SP o NRT) y su posición en el año de fuego.
serie_diaria <- function(puntos, rangos) {
  conteos <- puntos |>
    sf::st_drop_geometry() |>
    dplyr::count(acq_date, name = "detecciones")
  dias <- data.frame(
    fecha = seq(min(rangos$inicio), max(rangos$fin), by = "day")
  )
  dias$nivel <- NA_character_
  for (i in seq_len(nrow(rangos))) {
    en_rango <- dias$fecha >= rangos$inicio[i] & dias$fecha <= rangos$fin[i]
    dias$nivel[en_rango] <- rangos$nivel[i]
  }
  dias |>
    dplyr::left_join(conteos, by = c("fecha" = "acq_date")) |>
    dplyr::mutate(
      detecciones = tidyr::replace_na(detecciones, 0L),
      anio_fuego  = anio_fuego(fecha),
      dia         = dia_anio_fuego(fecha)
    )
}

# --- Índices ------------------------------------------------------------------

# Primer día en que la suma acumulada de detecciones alcanza `fraccion` del
# total; NA si no hubo detecciones.
primer_dia_fraccion <- function(fecha, detecciones, fraccion) {
  total <- sum(detecciones)
  if (total == 0) return(as.Date(NA))
  fecha[which(cumsum(detecciones) >= fraccion * total)[1]]
}

# Tabla de índices por año de fuego: DTOT, INI, FIN (día y fecha) y LON, con
# tres marcas que advierten sobre la lectura de la temporalidad:
#   parcial      el año de fuego no está completo en el periodo observado
#                (2001 empieza en enero; el año en curso no ha terminado)
#   provisional  algún día del año proviene del tiempo casi real
#   pocas_detecciones  DTOT < TEMPORADA_MIN_DETECCIONES
indices_temporada <- function(diaria, rangos,
                              fraccion_ini = TEMPORADA_FRACCION_INI,
                              fraccion_fin = TEMPORADA_FRACCION_FIN,
                              minimo = TEMPORADA_MIN_DETECCIONES) {
  observado <- c(min(rangos$inicio), max(rangos$fin))
  diaria |>
    dplyr::arrange(fecha) |>
    dplyr::summarise(
      dtot        = sum(detecciones),
      ini_fecha   = primer_dia_fraccion(fecha, detecciones, fraccion_ini),
      fin_fecha   = primer_dia_fraccion(fecha, detecciones, fraccion_fin),
      provisional = any(nivel == "NRT"),
      .by = anio_fuego
    ) |>
    dplyr::mutate(
      ini_dia = dia_anio_fuego(ini_fecha),
      fin_dia = dia_anio_fuego(fin_fecha),
      lon     = fin_dia - ini_dia + 1L,
      parcial = inicio_anio_fuego(anio_fuego) < observado[1] |
        fin_anio_fuego(anio_fuego) > observado[2],
      pocas_detecciones = dtot < minimo
    ) |>
    dplyr::select(anio_fuego, dtot, ini_dia, ini_fecha, fin_dia, fin_fecha,
                  lon, parcial, provisional, pocas_detecciones) |>
    dplyr::arrange(anio_fuego)
}

# Años cuya temporalidad puede interpretarse sin reservas.
temporada_confiable <- function(indices) {
  !(indices$parcial | indices$provisional | indices$pocas_detecciones) &
    !is.na(indices$lon)
}

# Nota en prosa por año, para la tabla del reporte ("" si no hay reservas).
notas_temporada <- function(indices) {
  notas <- mapply(function(parcial, provisional, pocas) {
    paste(c(if (parcial) "año parcial",
            if (provisional) "provisional",
            if (pocas) paste0("< ", TEMPORADA_MIN_DETECCIONES, " detecciones")),
          collapse = "; ")
  }, indices$parcial, indices$provisional, indices$pocas_detecciones)
  unname(notas)
}

# --- Tablas -------------------------------------------------------------------

tabla_temporada_csv <- function(indices, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(indices, dest)
  dest
}

tabla_tipos_csv <- function(tipos, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(tipos, dest)
  dest
}

# Widget DT de la tabla de temporada (para el reporte Quarto).
crear_tabla_temporada <- function(indices, etiqueta_fuente) {
  datos <- indices |>
    dplyr::mutate(
      inicio = ifelse(is.na(ini_fecha), "", fecha_es(ini_fecha, con_anio = FALSE)),
      fin    = ifelse(is.na(fin_fecha), "", fecha_es(fin_fecha, con_anio = FALSE)),
      nota   = notas_temporada(indices)
    ) |>
    dplyr::select(anio_fuego, dtot, inicio, fin, lon, nota)
  DT::datatable(
    datos,
    colnames = c("Año de fuego", "Detecciones", "Inicio (10 %)", "Fin (90 %)",
                 "Longitud (días)", "Nota"),
    caption = paste0("Temporada de fuego por año de fuego (setiembre–agosto) — ",
                     AREA_NOMBRE, ", ", etiqueta_fuente),
    options = list(pageLength = 30, dom = "t"),
    rownames = FALSE
  )
}

# Widget DT de las detecciones por tipo de fuente y año de fuego.
crear_tabla_tipos <- function(tipos, etiqueta_fuente) {
  DT::datatable(
    tipos,
    colnames = c("Año de fuego", TIPOS_FIRMS[["0"]], "Sin tipo (NRT)",
                 TIPOS_FIRMS[["1"]], TIPOS_FIRMS[["2"]], TIPOS_FIRMS[["3"]],
                 "Excluidas"),
    caption = paste0("Detecciones por tipo de fuente (columna `type` de FIRMS) — ",
                     AREA_NOMBRE, ", ", etiqueta_fuente),
    options = list(pageLength = 30, dom = "t"),
    rownames = FALSE
  )
}

# --- Cifras para la prosa del reporte --------------------------------------

# Resumen de los años confiables, con fechas ya en prosa española. Los años
# se devuelven como enteros para que el reporte los interpole tal cual.
ayudantes_temporada <- function(indices) {
  ok <- indices[temporada_confiable(indices), ]
  ref <- inicio_anio_fuego(2002L)   # año de referencia no bisiesto
  fecha_ref <- function(dia) fecha_es(ref + dia - 1L, con_anio = FALSE)
  list(
    n_confiables = nrow(ok),
    anios_confiables = range(ok$anio_fuego),
    n_reservas = sum(!temporada_confiable(indices)),
    lon_media = num_es(mean(ok$lon), 0),
    lon_min = min(ok$lon), anio_lon_min = ok$anio_fuego[which.min(ok$lon)],
    lon_max = max(ok$lon), anio_lon_max = ok$anio_fuego[which.max(ok$lon)],
    ini_mediana = fecha_ref(stats::median(ok$ini_dia)),
    fin_mediana = fecha_ref(stats::median(ok$fin_dia)),
    ini_mas_temprano = fecha_es(ok$ini_fecha[which.min(ok$ini_dia)]),
    ini_mas_tardio = fecha_es(ok$ini_fecha[which.max(ok$ini_dia)]),
    fin_mas_temprano = fecha_es(ok$fin_fecha[which.min(ok$fin_dia)]),
    fin_mas_tardio = fecha_es(ok$fin_fecha[which.max(ok$fin_dia)])
  )
}

# --- Figura -------------------------------------------------------------------

# Banda de meses (inicio, fin inclusive) proyectada al año de fuego de
# referencia 2001-09-01..2002-08-31 (sin bisiesto): los meses desde
# septiembre caen en 2001 y los demás en 2002.
banda_referencia <- function(meses) {
  anio <- function(m) if (m >= MES_INICIO_ANIO_FUEGO) 2001L else 2002L
  xmin <- as.Date(sprintf("%d-%02d-01", anio(meses[["inicio"]]), meses[["inicio"]]))
  xmax <- lubridate::ceiling_date(
    as.Date(sprintf("%d-%02d-01", anio(meses[["fin"]]), meses[["fin"]])),
    "month") - 1
  c(xmin = xmin, xmax = xmax)
}

# Un segmento por año de fuego, de INI a FIN, sobre las temporadas del SINAC
# y del IMN como bandas de referencia. Los años con reservas (parciales,
# provisionales o con pocas detecciones) van en gris.
grafico_temporada <- function(indices, dest, etiqueta_fuente, fuente) {
  ref <- inicio_anio_fuego(2002L)
  datos <- indices |>
    dplyr::filter(!is.na(lon)) |>
    dplyr::mutate(
      x0 = ref + ini_dia - 1L,
      x1 = ref + fin_dia - 1L,
      lectura = ifelse(temporada_confiable(indices)[!is.na(indices$lon)],
                       "Año completo", "Año parcial, provisional o con pocas detecciones")
    )
  sinac <- banda_referencia(TEMPORADA_SINAC)
  imn   <- banda_referencia(EPOCA_SECA_IMN)
  bandas <- data.frame(
    banda = factor(c("Temporada de incendios del SINAC (enero–mayo)",
                     "Época seca del Pacífico según el IMN (diciembre–abril)")),
    xmin = c(sinac[["xmin"]], imn[["xmin"]]),
    xmax = c(sinac[["xmax"]], imn[["xmax"]])
  )
  limites <- c(min(c(datos$x0, bandas$xmin)) - 10,
               max(c(datos$x1, bandas$xmax)) + 25)
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = bandas,
                       ggplot2::aes(xmin = xmin, xmax = xmax,
                                    ymin = -Inf, ymax = Inf, fill = banda),
                       alpha = 0.18) +
    ggplot2::geom_segment(data = datos,
                          ggplot2::aes(x = x0, xend = x1,
                                       y = anio_fuego, yend = anio_fuego,
                                       color = lectura),
                          linewidth = 2.2, lineend = "round") +
    ggplot2::geom_text(data = datos,
                       ggplot2::aes(x = x1 + 4, y = anio_fuego, label = lon),
                       hjust = 0, size = 3, color = "grey30") +
    ggplot2::scale_fill_manual(values = c("#fdd49e", "#a6bddb"), name = NULL) +
    ggplot2::scale_color_manual(values = c("Año completo" = COLOR_DETECCIONES,
                                           "Año parcial, provisional o con pocas detecciones" = "grey65"),
                                name = NULL) +
    ggplot2::scale_x_date(limits = limites, date_breaks = "1 month",
                          labels = function(x) MESES_ES[lubridate::month(x)],
                          expand = ggplot2::expansion(0)) +
    ggplot2::scale_y_reverse(breaks = datos$anio_fuego) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1, nrow = 2),
                    color = ggplot2::guide_legend(order = 2, nrow = 2)) +
    ggplot2::labs(
      title = "Longitud de la temporada de fuego por año",
      subtitle = paste0("Del 10 % al 90 % de las detecciones acumuladas de cada ",
                        "año de fuego (setiembre–agosto); la cifra es la ",
                        "longitud en días\n", AREA_NOMBRE, ", ", etiqueta_fuente),
      x = NULL, y = "Año de fuego", caption = fuente
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey92"),
      panel.grid.major.x = ggplot2::element_line(color = "grey92"),
      legend.position = "bottom",
      legend.box = "vertical",
      plot.title = ggplot2::element_text(face = "bold")
    )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 10,
                  height = 2.5 + 0.28 * nrow(datos), dpi = 200)
  dest
}

# --- Ráster consolidado ------------------------------------------------------
# Ver README, «Ráster consolidado de LON». Temporada climatológica por celda:
# las detecciones de todos los años del periodo de referencia se agrupan por
# celda y su distribución en días del año de fuego da INI, FIN y LON con las
# mismas fracciones. No es el promedio de los LON anuales.

# Años de fuego del periodo de referencia: los completos y no provisionales.
anios_referencia <- function(indices) {
  indices$anio_fuego[!indices$parcial & !indices$provisional]
}

# Índices consolidados por celda. `celdas` es la tabla (id_deteccion,
# celda_id) de asignar_celda(); `anios`, los años de fuego incluidos. Las
# celdas con menos de `minimo` detecciones conservan dtot pero llevan NA en
# los índices; las celdas de la grilla sin detección alguna no aparecen.
indices_consolidados <- function(puntos, celdas, anios,
                                 minimo = RASTER_MIN_DETECCIONES,
                                 fraccion_ini = TEMPORADA_FRACCION_INI,
                                 fraccion_fin = TEMPORADA_FRACCION_FIN) {
  df <- puntos |>
    sf::st_drop_geometry() |>
    dplyr::select(id_deteccion, acq_date) |>
    dplyr::inner_join(celdas, by = "id_deteccion") |>
    dplyr::mutate(anio_fuego = anio_fuego(acq_date),
                  dia = dia_anio_fuego(acq_date)) |>
    dplyr::filter(anio_fuego %in% anios)
  primer_dia <- function(dia, fraccion) {
    conteo <- table(dia)
    dias <- as.integer(names(conteo))
    dias[which(cumsum(conteo) >= fraccion * sum(conteo))[1]]
  }
  df |>
    dplyr::summarise(
      dtot = dplyr::n(),
      ini_dia = primer_dia(dia, fraccion_ini),
      fin_dia = primer_dia(dia, fraccion_fin),
      .by = celda_id
    ) |>
    dplyr::mutate(
      valida = dtot >= minimo,
      ini_dia = ifelse(valida, ini_dia, NA_integer_),
      fin_dia = ifelse(valida, fin_dia, NA_integer_),
      lon = fin_dia - ini_dia + 1L,
      anio_inicio = min(anios), anio_fin = max(anios)
    ) |>
    dplyr::arrange(celda_id)
}

tabla_temporada_celdas_csv <- function(consolidado, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(consolidado, dest)
  dest
}

# GeoTIFF con una capa por índice sobre la extensión de la grilla de
# análisis. Los metadatos llevan plataforma, periodo y umbral para que dos
# rásteres no se comparen sin saber qué hay detrás.
raster_consolidado <- function(consolidado, grilla, dest, plataforma,
                               res = GRILLA_RES_ANALISIS,
                               minimo = RASTER_MIN_DETECCIONES) {
  b <- sf::st_bbox(grilla)
  plantilla <- terra::rast(
    xmin = b[["xmin"]], xmax = b[["xmax"]], ymin = b[["ymin"]],
    ymax = b[["ymax"]], resolution = res, crs = CRS_WGS84
  )
  capas <- c("ini_dia", "fin_dia", "lon", "dtot")
  datos <- grilla |>
    sf::st_drop_geometry() |>
    dplyr::inner_join(consolidado, by = "celda_id")
  celda <- terra::cellFromXY(plantilla,
                             cbind(datos$lon_sw + res / 2, datos$lat_sw + res / 2))
  r <- terra::rast(replicate(length(capas), plantilla, simplify = FALSE))
  names(r) <- capas
  for (capa in capas) {
    r[[capa]][celda] <- datos[[capa]]
  }
  terra::metags(r) <- c(
    plataforma = plataforma,
    periodo = paste0(min(consolidado$anio_inicio), "-", max(consolidado$anio_fin)),
    umbral_detecciones = as.character(minimo),
    # Sin el signo "=" en los valores: terra descarta TODAS las etiquetas si
    # alguna lo contiene (verificado con terra 1.9-11).
    definicion = "INI/FIN: dia del anio de fuego (1: 1 set) en que la suma acumulada alcanza 10 %/90 %; LON: FIN - INI + 1"
  )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(r, dest, overwrite = TRUE, datatype = "INT4S",
                     NAflag = -9999)
  dest
}

# Celdas de la grilla con sus índices, en CRTM05, listas para dibujar: las
# celdas sin detección alguna no se incluyen; las que no alcanzan el umbral
# van con índices NA (gris en los mapas).
celdas_temporada_sf <- function(consolidado, grilla) {
  # La unión se hace sobre la tabla sin geometría y esta se vuelve a pegar
  # con st_sf: el método de dplyr para sf solo existe si el paquete está
  # cargado, y en los qmd no lo está.
  datos <- dplyr::inner_join(sf::st_drop_geometry(grilla), consolidado,
                             by = "celda_id")
  geometria <- sf::st_geometry(grilla)[match(datos$celda_id, grilla$celda_id)]
  a_crtm05(sf::st_sf(datos, geometry = geometria))
}

# Cifras para la prosa del reporte.
ayudantes_temporada_celdas <- function(consolidado) {
  ok <- consolidado[consolidado$valida, ]
  ref <- inicio_anio_fuego(2002L)
  fecha_ref <- function(dia) fecha_es(ref + dia - 1L, con_anio = FALSE)
  list(
    n_con_fuego = nrow(consolidado),
    n_validas = nrow(ok),
    periodo = paste0(min(consolidado$anio_inicio), "–", max(consolidado$anio_fin)),
    lon_min = min(ok$lon), lon_max = max(ok$lon),
    lon_mediana = stats::median(ok$lon),
    ini_min = fecha_ref(min(ok$ini_dia)), ini_max = fecha_ref(max(ok$ini_dia)),
    fin_min = fecha_ref(min(ok$fin_dia)), fin_max = fecha_ref(max(ok$fin_dia))
  )
}

# Mapa estático de un índice consolidado por celda sobre el límite nacional.
# `variable`: "lon" (días), "ini_dia" o "fin_dia" (rotulados como fechas).
grafico_temporada_celdas <- function(consolidado, grilla, area, dest,
                                     variable, etiqueta_fuente, fuente,
                                     minimo = RASTER_MIN_DETECCIONES) {
  celdas <- celdas_temporada_sf(consolidado, grilla)
  ref <- inicio_anio_fuego(2002L)
  rotulo <- c(lon = "Longitud (días)", ini_dia = "Inicio (10 %)",
              fin_dia = "Fin (90 %)")[[variable]]
  titulo <- c(lon = "Longitud de la temporada de fuego por celda",
              ini_dia = "Inicio de la temporada de fuego por celda",
              fin_dia = "Fin de la temporada de fuego por celda")[[variable]]
  etiquetas_escala <- if (variable == "lon") ggplot2::waiver() else {
    function(x) format(ref + x - 1L, "%d %b") |>
      (\(s) paste(sub(" .*", "", s), MESES_ES[lubridate::month(ref + x - 1L)]))()
  }
  periodo <- paste0(min(consolidado$anio_inicio), "–", max(consolidado$anio_fin))
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = celdas, ggplot2::aes(fill = .data[[variable]]),
                     color = "white", linewidth = 0.15) +
    ggplot2::geom_sf(data = area, fill = NA, color = "grey30",
                     linewidth = 0.4) +
    # LON: la escala se acota en RASTER_LON_TOPE para que las pocas celdas
    # sin estación definida (fuego todo el año, LON cercano a 300) no
    # aplasten el gradiente de 60 a 120 días que domina el Pacífico.
    ggplot2::scale_fill_viridis_c(
      option = if (variable == "lon") "inferno" else "viridis",
      direction = if (variable == "lon") -1 else 1,
      na.value = "grey85", name = rotulo,
      limits = if (variable == "lon") c(0, RASTER_LON_TOPE) else NULL,
      oob = scales::oob_squish,
      labels = if (variable == "lon") {
        function(x) ifelse(x >= RASTER_LON_TOPE, paste0("\u2265 ", x), x)
      } else etiquetas_escala
    ) +
    ggplot2::labs(
      title = titulo,
      subtitle = paste0("Detecciones de vegetación de los años de fuego ",
                        periodo, "; celdas de 0,1° con menos de ", minimo,
                        " detecciones en gris\n", AREA_NOMBRE, ", ",
                        etiqueta_fuente),
      caption = fuente
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "grey92", linewidth = 0.3),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "right"
    )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 9, height = 7, dpi = 200)
  dest
}
