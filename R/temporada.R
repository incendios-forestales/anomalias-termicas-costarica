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

# --- Concentración diaria (README, «Segundo índice») ------------------------

# Número mínimo de días que, ordenados de mayor a menor, acumulan `fraccion`
# del total; NA sin detecciones.
n50 <- function(detecciones, fraccion = CONCENTRACION_FRACCION) {
  total <- sum(detecciones)
  if (total == 0) return(NA_integer_)
  orden <- sort(detecciones, decreasing = TRUE)
  which(cumsum(orden) >= fraccion * total)[1]
}

# Porcentaje del total en los `top` días con más detecciones; NA sin
# detecciones.
c10 <- function(detecciones, top = CONCENTRACION_DIAS_TOP) {
  total <- sum(detecciones)
  if (total == 0) return(NA_real_)
  round(100 * sum(head(sort(detecciones, decreasing = TRUE), top)) / total, 1)
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
      df          = sum(detecciones > 0),
      ini_fecha   = primer_dia_fraccion(fecha, detecciones, fraccion_ini),
      fin_fecha   = primer_dia_fraccion(fecha, detecciones, fraccion_fin),
      n50         = n50(detecciones),
      c10         = c10(detecciones),
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
    dplyr::select(anio_fuego, dtot, df, ini_dia, ini_fecha, fin_dia, fin_fecha,
                  lon, n50, c10, parcial, provisional, pocas_detecciones) |>
    dplyr::arrange(anio_fuego)
}

# --- Intensidad (README, «Cuarto índice») -----------------------------------
# Por año de fuego, sobre las detecciones de vegetación: mediana y percentil
# 95 de la FRP (MW) y los controles de mezcla de satélites (fracción de Aqua)
# y de hora (fracción nocturna). NA en los años sin detecciones.
indices_intensidad <- function(puntos) {
  puntos |>
    sf::st_drop_geometry() |>
    dplyr::mutate(anio_fuego = anio_fuego(acq_date)) |>
    dplyr::summarise(
      frpi  = round(stats::median(frp, na.rm = TRUE), 1),
      frp95 = round(stats::quantile(frp, 0.95, na.rm = TRUE, names = FALSE), 1),
      aq    = round(mean(satellite == "Aqua"), 3),
      noc   = round(mean(daynight == "N"), 3),
      .by = anio_fuego
    ) |>
    dplyr::arrange(anio_fuego)
}

# Agrega la intensidad a la tabla anual de índices (NA donde no hubo
# detecciones). Las columnas de control van al final, antes de las marcas.
unir_intensidad <- function(indices, intensidad) {
  indices |>
    dplyr::left_join(intensidad, by = "anio_fuego") |>
    dplyr::relocate(frpi, frp95, aq, noc, .before = parcial)
}

# --- Días extremos (README, «Quinto índice») --------------------------------

# Umbral P95: cuantil empírico de las detecciones diarias en los días de
# fuego (al menos una detección) de los años de fuego del periodo base.
umbral_p95 <- function(diaria, anios, prob = EXTREMOS_PERCENTIL) {
  base <- diaria[diaria$anio_fuego %in% anios & diaria$detecciones > 0, ]
  if (nrow(base) == 0) stop("Sin días de fuego en el periodo base.", call. = FALSE)
  unname(stats::quantile(base$detecciones, prob))
}

# Por año de fuego: días que superan el umbral, detecciones acumuladas en
# ellos y fracción del total anual (NA sin detecciones).
indices_extremos <- function(diaria, p95) {
  diaria |>
    dplyr::summarise(
      nd95 = sum(detecciones > p95),
      d95p = sum(detecciones[detecciones > p95]),
      d95ptot = ifelse(sum(detecciones) > 0,
                       round(100 * d95p / sum(detecciones), 1), NA_real_),
      .by = anio_fuego
    ) |>
    dplyr::mutate(p95 = p95) |>
    dplyr::arrange(anio_fuego)
}

# Agrega los días extremos a la tabla anual y marca los años no comparables
# con el umbral: los que casi no tienen detecciones de Aqua (2001 y 2002).
unir_extremos <- function(indices, extremos, aq_min = EXTREMOS_AQ_MIN) {
  indices |>
    dplyr::left_join(extremos, by = "anio_fuego") |>
    dplyr::mutate(no_comparable = is.na(aq) | aq < aq_min) |>
    dplyr::relocate(nd95, d95p, d95ptot, p95, .before = parcial)
}

# Años cuya temporalidad puede interpretarse sin reservas.
temporada_confiable <- function(indices) {
  !(indices$parcial | indices$provisional | indices$pocas_detecciones) &
    !is.na(indices$lon)
}

# Nota en prosa por año, para la tabla del reporte ("" si no hay reservas).
notas_temporada <- function(indices) {
  no_comp <- if ("no_comparable" %in% names(indices)) indices$no_comparable else
    rep(FALSE, nrow(indices))
  notas <- mapply(function(parcial, provisional, pocas, no_comp) {
    paste(c(if (parcial) "año parcial",
            if (provisional) "provisional",
            if (pocas) paste0("< ", TEMPORADA_MIN_DETECCIONES, " detecciones"),
            if (no_comp) "sin Aqua: días extremos no comparables"),
          collapse = "; ")
  }, indices$parcial, indices$provisional, indices$pocas_detecciones, no_comp)
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
    dplyr::select(anio_fuego, dtot, inicio, fin, lon, df, n50, c10, frpi,
                  frp95, nd95, d95ptot, nota)
  DT::datatable(
    datos,
    colnames = c("Año de fuego", "Detecciones", "Inicio (10 %)", "Fin (90 %)",
                 "Longitud (días)", "Días de fuego", "N50 (días)",
                 "C10 (%)", "FRP mediana (MW)", "FRP p95 (MW)",
                 "Días extremos", "% en días extremos", "Nota"),
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
# `fuera` es el porcentaje de detecciones fuera de los meses de referencia
# (dic-may): por encima de `fuera_max` la celda es bimodal o sin estación
# definida y sus INI, FIN y LON se anulan (ver README, «Celdas bimodales»).
indices_consolidados <- function(puntos, celdas, anios,
                                 minimo = RASTER_MIN_DETECCIONES,
                                 minimo_concentracion = RASTER_MIN_DETECCIONES_CONCENTRACION,
                                 fuera_max = RASTER_FUERA_MAX_PCT,
                                 meses_referencia = TEMPORADA_REFERENCIA_MESES,
                                 fraccion_ini = TEMPORADA_FRACCION_INI,
                                 fraccion_fin = TEMPORADA_FRACCION_FIN) {
  df <- puntos |>
    sf::st_drop_geometry() |>
    dplyr::select(id_deteccion, acq_date) |>
    dplyr::inner_join(celdas, by = "id_deteccion") |>
    dplyr::mutate(anio_fuego = anio_fuego(acq_date),
                  dia = dia_anio_fuego(acq_date),
                  en_temporada = lubridate::month(acq_date) %in% meses_referencia) |>
    dplyr::filter(anio_fuego %in% anios)
  primer_dia <- function(dia, fraccion) {
    conteo <- table(dia)
    dias <- as.integer(names(conteo))
    dias[which(cumsum(conteo) >= fraccion * sum(conteo))[1]]
  }
  df |>
    dplyr::summarise(
      dtot = dplyr::n(),
      fuera = round(100 * mean(!en_temporada), 1),
      ini_dia = primer_dia(dia, fraccion_ini),
      fin_dia = primer_dia(dia, fraccion_fin),
      # Concentración sobre las fechas reales agrupadas: días de fuego de la
      # celda y días que reúnen la mitad de sus detecciones.
      df = dplyr::n_distinct(acq_date),
      n50 = n50(as.integer(table(acq_date))),
      .by = celda_id
    ) |>
    dplyr::mutate(
      valida = dtot >= minimo,
      sin_estacion = valida & fuera > fuera_max,
      con_indices = valida & !sin_estacion,
      ini_dia = ifelse(con_indices, ini_dia, NA_integer_),
      fin_dia = ifelse(con_indices, fin_dia, NA_integer_),
      lon = fin_dia - ini_dia + 1L,
      valida_n50f = dtot >= minimo_concentracion,
      n50f = ifelse(valida_n50f, round(n50 / df, 3), NA_real_),
      anio_inicio = min(anios), anio_fin = max(anios)
    ) |>
    dplyr::select(celda_id, dtot, fuera, ini_dia, fin_dia, lon, df, n50f,
                  valida, sin_estacion, valida_n50f, anio_inicio, anio_fin) |>
    dplyr::arrange(celda_id)
}

# --- Frecuencia y densidad (README, «Tercer índice») ------------------------
# Para TODAS las celdas de la grilla (el cero es dato), sobre los años de
# fuego del periodo base: superficie terrestre, años con fuego, FREC (fracción
# de años con fuego) y DENS (detecciones por km² de tierra y año). Celdas con
# menos de `min_area` km² de tierra quedan en NA. Sobre el mismo periodo base
# van la intensidad FRPI (mediana de FRP) y el control AQ (fracción de Aqua)
# por celda, con NA por debajo de `minimo` detecciones (README, «Cuarto
# índice»).
indices_frecuencia <- function(puntos, celdas, grilla, anios,
                               min_area = RASTER_MIN_AREA_KM2,
                               minimo = RASTER_MIN_DETECCIONES) {
  # Se fijan antes de entrar a la tabla: dentro de mutate(), `anios` pasa a
  # ser la columna de años con fuego y taparía al vector del periodo base.
  n_anios <- length(anios)
  base_ini <- min(anios)
  base_fin <- max(anios)
  por_celda <- puntos |>
    sf::st_drop_geometry() |>
    dplyr::select(id_deteccion, acq_date, frp, satellite) |>
    dplyr::inner_join(celdas, by = "id_deteccion") |>
    dplyr::mutate(anio_fuego = anio_fuego(acq_date)) |>
    dplyr::filter(anio_fuego %in% anios) |>
    dplyr::summarise(dtot_base = dplyr::n(),
                     anios = dplyr::n_distinct(anio_fuego),
                     frpi = round(stats::median(frp, na.rm = TRUE), 1),
                     aq = round(mean(satellite == "Aqua"), 3),
                     .by = celda_id)
  grilla |>
    sf::st_drop_geometry() |>
    dplyr::select(celda_id, area_km2) |>
    dplyr::left_join(por_celda, by = "celda_id") |>
    dplyr::mutate(
      dtot_base = tidyr::replace_na(dtot_base, 0L),
      anios = tidyr::replace_na(anios, 0L),
      area_ok = area_km2 >= min_area,
      frec = ifelse(area_ok, round(anios / n_anios, 3), NA_real_),
      dens = ifelse(area_ok, round(dtot_base / area_km2 / n_anios, 4),
                    NA_real_),
      valida_base = dtot_base >= minimo,
      frpi = ifelse(valida_base, frpi, NA_real_),
      aq = ifelse(valida_base, aq, NA_real_),
      base_inicio = base_ini, base_fin = base_fin
    ) |>
    dplyr::select(celda_id, area_km2, dtot_base, anios, frec, dens, frpi, aq,
                  valida_base, base_inicio, base_fin) |>
    dplyr::arrange(celda_id)
}

# Une el consolidado de temporada (solo celdas con detecciones) con la
# frecuencia y densidad (todas las celdas): la tabla resultante cubre la
# grilla completa, con ceros en dtot y NA en los índices de temporada de las
# celdas sin fuego.
unir_consolidados <- function(consolidado, frecuencia, anios) {
  # Antes de entrar a la tabla: `anios` es también una columna de
  # `frecuencia` (años con fuego por celda) y taparía al vector.
  ref_ini <- min(anios)
  ref_fin <- max(anios)
  frecuencia |>
    dplyr::left_join(consolidado, by = "celda_id") |>
    dplyr::mutate(
      dtot = tidyr::replace_na(dtot, 0L),
      valida = tidyr::replace_na(valida, FALSE),
      sin_estacion = tidyr::replace_na(sin_estacion, FALSE),
      valida_n50f = tidyr::replace_na(valida_n50f, FALSE),
      valida_base = tidyr::replace_na(valida_base, FALSE),
      anio_inicio = ref_ini, anio_fin = ref_fin
    ) |>
    dplyr::select(celda_id, area_km2, dtot, fuera, ini_dia, fin_dia, lon, df,
                  n50f, dtot_base, anios, frec, dens, frpi, aq, valida,
                  sin_estacion, valida_n50f, valida_base, anio_inicio,
                  anio_fin, base_inicio, base_fin) |>
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
                               minimo = RASTER_MIN_DETECCIONES,
                               fuera_max = RASTER_FUERA_MAX_PCT) {
  b <- sf::st_bbox(grilla)
  plantilla <- terra::rast(
    xmin = b[["xmin"]], xmax = b[["xmax"]], ymin = b[["ymin"]],
    ymax = b[["ymax"]], resolution = res, crs = CRS_WGS84
  )
  capas <- c("ini_dia", "fin_dia", "lon", "fuera", "n50f", "dtot", "frec", "dens",
             "frpi", "aq")
  datos <- grilla |>
    sf::st_drop_geometry() |>
    dplyr::inner_join(consolidado, by = "celda_id")
  celda <- terra::cellFromXY(plantilla,
                             cbind(datos$lon_sw + res / 2, datos$lat_sw + res / 2))
  r <- terra::rast(replicate(length(capas), plantilla, simplify = FALSE))
  names(r) <- capas
  datos$fuera <- round(datos$fuera)
  for (capa in capas) {
    r[[capa]][celda] <- datos[[capa]]
  }
  terra::metags(r) <- c(
    plataforma = plataforma,
    periodo = paste0(min(consolidado$anio_inicio), "-", max(consolidado$anio_fin)),
    periodo_base_frec_dens = paste0(min(consolidado$base_inicio), "-",
                                    max(consolidado$base_fin)),
    umbral_area_km2 = as.character(RASTER_MIN_AREA_KM2),
    umbral_detecciones = as.character(minimo),
    umbral_fuera_pct = as.character(fuera_max),
    umbral_detecciones_n50f = as.character(RASTER_MIN_DETECCIONES_CONCENTRACION),
    # Sin el signo "=" en los valores: terra descarta TODAS las etiquetas si
    # alguna lo contiene (verificado con terra 1.9-11).
    definicion = paste0("INI/FIN: dia del anio de fuego (1: 1 set) en que la suma ",
                        "acumulada alcanza 10 %/90 %; LON: FIN - INI + 1; FUERA: % de ",
                        "detecciones fuera de dic-may (celdas por encima del umbral ",
                        "quedan sin INI/FIN/LON: sin estacion definida); N50F: ",
                        "proporcion de los dias de fuego de la celda que reunen la ",
                        "mitad de sus detecciones (0,5 repartido, hacia 0 en oleadas); ",
                        "FREC: fraccion de anios del periodo base con fuego; DENS: ",
                        "detecciones por km2 de tierra y anio del periodo base; ",
                        "FRPI: mediana de la FRP (MW) en el periodo base; AQ: ",
                        "fraccion de detecciones de Aqua en el periodo base")
  )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  # FLT4S por N50F (0-0,5); las demás capas son enteras y caben igual.
  terra::writeRaster(r, dest, overwrite = TRUE, datatype = "FLT4S",
                     NAflag = -9999)
  dest
}

# Celdas de la grilla con sus índices, en CRTM05, listas para dibujar: las
# celdas sin detección alguna no se incluyen; las que no alcanzan el umbral
# van con índices NA (gris en los mapas).
celdas_temporada_sf <- function(consolidado, grilla, solo_con_fuego = FALSE) {
  if (solo_con_fuego) consolidado <- consolidado[consolidado$dtot > 0, ]
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
  ok <- consolidado[!is.na(consolidado$lon), ]
  bim <- consolidado[consolidado$sin_estacion, ]
  ref <- inicio_anio_fuego(2002L)
  fecha_ref <- function(dia) fecha_es(ref + dia - 1L, con_anio = FALSE)
  list(
    n_con_fuego = nrow(consolidado),
    n_validas = sum(consolidado$valida),
    n_con_indices = nrow(ok),
    n_sin_estacion = nrow(bim),
    fuera_sin_estacion = if (nrow(bim) > 0) paste0(num_es(min(bim$fuera), 0), " y ",
                                                   num_es(max(bim$fuera), 0)) else "",
    fuera_mediana_unimodal = num_es(stats::median(ok$fuera), 0),
    n_celdas_grilla = nrow(consolidado),
    n_sin_fuego_base = sum(consolidado$dtot_base == 0 & !is.na(consolidado$frec)),
    n_frec_alta = sum(consolidado$frec >= 0.75, na.rm = TRUE),
    n_frec_baja = sum(consolidado$frec > 0 & consolidado$frec < 0.25, na.rm = TRUE),
    periodo_base = paste0(min(consolidado$base_inicio), "–", max(consolidado$base_fin)),
    dens_max = num_es(max(consolidado$dens, na.rm = TRUE), 2),
    dens_mediana_con_fuego = num_es(stats::median(consolidado$dens[consolidado$dtot_base > 0], na.rm = TRUE), 3),
    n_validas_base = sum(consolidado$valida_base),
    frpi_mediana = num_es(stats::median(consolidado$frpi, na.rm = TRUE), 1),
    frpi_min = num_es(min(consolidado$frpi, na.rm = TRUE), 1),
    frpi_max = num_es(max(consolidado$frpi, na.rm = TRUE), 1),
    aq_mediana = num_es(100 * stats::median(consolidado$aq, na.rm = TRUE), 0),
    aq_min = num_es(100 * min(consolidado$aq, na.rm = TRUE), 0),
    aq_max = num_es(100 * max(consolidado$aq, na.rm = TRUE), 0),
    n_validas_n50f = sum(consolidado$valida_n50f),
    n50f_min = num_es(min(consolidado$n50f, na.rm = TRUE), 2),
    n50f_max = num_es(max(consolidado$n50f, na.rm = TRUE), 2),
    n50f_mediana = num_es(stats::median(consolidado$n50f, na.rm = TRUE), 2),
    periodo = paste0(min(consolidado$anio_inicio), "–", max(consolidado$anio_fin)),
    lon_min = min(ok$lon), lon_max = max(ok$lon),
    lon_mediana = stats::median(ok$lon),
    ini_min = fecha_ref(min(ok$ini_dia)), ini_max = fecha_ref(max(ok$ini_dia)),
    fin_min = fecha_ref(min(ok$fin_dia)), fin_max = fecha_ref(max(ok$fin_dia))
  )
}

# Trama diagonal (tres líneas por celda) para marcar celdas en un mapa sin
# depender de paquetes de patrones: líneas sf en el CRS de las celdas.
trama_celdas <- function(celdas) {
  if (nrow(celdas) == 0) {
    return(sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(celdas))))
  }
  lineas <- lapply(seq_len(nrow(celdas)), function(i) {
    b <- sf::st_bbox(celdas[i, ])
    w <- b[["xmax"]] - b[["xmin"]]; h <- b[["ymax"]] - b[["ymin"]]
    sf::st_multilinestring(list(
      rbind(c(b[["xmin"]], b[["ymin"]]), c(b[["xmax"]], b[["ymax"]])),
      rbind(c(b[["xmin"]], b[["ymin"]] + h / 2), c(b[["xmax"]] - w / 2, b[["ymax"]])),
      rbind(c(b[["xmin"]] + w / 2, b[["ymin"]]), c(b[["xmax"]], b[["ymax"]] - h / 2))
    ))
  })
  sf::st_sf(geometry = sf::st_sfc(lineas, crs = sf::st_crs(celdas)))
}

# Mapa estático de un índice consolidado por celda sobre el límite nacional.
# `variable`: "lon" (días), "ini_dia" o "fin_dia" (rotulados como fechas).
grafico_temporada_celdas <- function(consolidado, grilla, area, dest,
                                     variable, etiqueta_fuente, fuente,
                                     minimo = RASTER_MIN_DETECCIONES) {
  # Los índices de temporada se dibujan solo en las celdas con fuego (las
  # demás quedan en blanco); frecuencia y densidad cubren toda la grilla,
  # porque en ellas el cero es dato.
  # Frecuencia y densidad cubren toda la grilla; intensidad y control de
  # satélite usan el periodo base pero solo en celdas con fuego.
  todas_las_celdas <- variable %in% c("frec", "dens")
  usa_base <- variable %in% c("frec", "dens", "frpi", "aq")
  celdas <- celdas_temporada_sf(consolidado, grilla,
                                solo_con_fuego = !todas_las_celdas)
  trama <- trama_celdas(celdas[celdas$sin_estacion, ])
  ref <- inicio_anio_fuego(2002L)
  rotulo <- c(lon = "Longitud (días)", ini_dia = "Inicio (10 %)",
              fin_dia = "Fin (90 %)",
              n50f = "N50F\n(0,5 repartido;\nhacia 0, en oleadas)",
              frec = "Fracción de años\ncon fuego",
              dens = "Detecciones por\nkm² y año",
              frpi = "FRP mediana\n(MW)",
              aq = "Fracción de\ndetecciones\nde Aqua (tarde)")[[variable]]
  titulo <- c(lon = "Longitud de la temporada de fuego por celda",
              ini_dia = "Inicio de la temporada de fuego por celda",
              fin_dia = "Fin de la temporada de fuego por celda",
              n50f = "Concentración diaria del fuego por celda",
              frec = "Frecuencia del fuego por celda",
              dens = "Densidad del fuego por celda",
              frpi = "Intensidad del fuego por celda",
              aq = "Ciclo diurno del fuego por celda")[[variable]]
  es_fecha <- variable %in% c("ini_dia", "fin_dia")
  etiquetas_escala <- if (!es_fecha) ggplot2::waiver() else {
    function(x) format(ref + x - 1L, "%d %b") |>
      (\(s) paste(sub(" .*", "", s), MESES_ES[lubridate::month(ref + x - 1L)]))()
  }
  periodo <- if (usa_base) {
    paste0(min(consolidado$base_inicio), "–", max(consolidado$base_fin),
           ", el periodo base")
  } else {
    paste0(min(consolidado$anio_inicio), "–", max(consolidado$anio_fin))
  }
  nota_umbral <- if (todas_las_celdas) {
    paste0("celdas con menos de ", RASTER_MIN_AREA_KM2, " km² de tierra en gris")
  } else if (usa_base) {
    paste0("celdas con menos de ", minimo,
           " detecciones en el periodo base en gris")
  } else {
    paste0("celdas de 0,1° con menos de ",
           if (variable == "n50f") RASTER_MIN_DETECCIONES_CONCENTRACION else minimo,
           " detecciones en gris")
  }
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = celdas, ggplot2::aes(fill = .data[[variable]]),
                     color = "white", linewidth = 0.15) +
    ggplot2::geom_sf(data = celdas[celdas$sin_estacion, ], fill = "#deebf7",
                     color = "white", linewidth = 0.15) +
    ggplot2::geom_sf(data = trama, color = "grey35", linewidth = 0.35) +
    ggplot2::geom_sf(data = area, fill = NA, color = "grey30",
                     linewidth = 0.4) +
    # LON: la escala se acota en RASTER_LON_TOPE para que las pocas celdas
    # sin estación definida (fuego todo el año, LON cercano a 300) no
    # aplasten el gradiente de 60 a 120 días que domina el Pacífico.
    ggplot2::scale_fill_viridis_c(
      option = c(lon = "inferno", ini_dia = "viridis", fin_dia = "viridis",
                 n50f = "mako", frec = "viridis", dens = "rocket",
                 frpi = "magma", aq = "cividis")[[variable]],
      direction = if (variable %in% c("lon", "dens", "frpi")) -1 else 1,
      trans = if (variable %in% c("dens", "frpi")) "sqrt" else "identity",
      na.value = "grey85", name = rotulo,
      limits = if (variable == "lon") c(0, RASTER_LON_TOPE) else NULL,
      breaks = if (variable == "lon") seq(0, RASTER_LON_TOPE, by = 60) else ggplot2::waiver(),
      oob = scales::oob_squish,
      labels = if (variable == "lon") {
        function(x) ifelse(x >= RASTER_LON_TOPE, paste0("\u2265 ", x), x)
      } else etiquetas_escala
    ) +
    ggplot2::labs(
      title = titulo,
      subtitle = paste0("Detecciones de vegetación de los años de fuego ",
                        periodo, "\n", toupper(substr(nota_umbral, 1, 1)),
                        substr(nota_umbral, 2, nchar(nota_umbral)),
                        "\nCon trama, sin estación definida (más de ",
                        RASTER_FUERA_MAX_PCT,
                        " % de las detecciones fuera de diciembre a mayo)\n",
                        AREA_NOMBRE, ", ", etiqueta_fuente),
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


# --- Figura y cifras de la concentración diaria ------------------------------

ayudantes_concentracion <- function(indices) {
  ok <- indices[temporada_confiable(indices), ]
  list(
    n50_media = num_es(mean(ok$n50), 0),
    n50_min = min(ok$n50), anio_n50_min = ok$anio_fuego[which.min(ok$n50)],
    n50_max = max(ok$n50), anio_n50_max = ok$anio_fuego[which.max(ok$n50)],
    c10_media = num_es(mean(ok$c10), 0),
    c10_min = num_es(min(ok$c10), 0), anio_c10_min = ok$anio_fuego[which.min(ok$c10)],
    c10_max = num_es(max(ok$c10), 0), anio_c10_max = ok$anio_fuego[which.max(ok$c10)],
    df_media = num_es(mean(ok$df), 0)
  )
}

# Barras por año de fuego de dos índices en paneles apilados; los años con
# reservas en gris. `variables` es un vector con nombre: c(columna = rótulo).
grafico_barras_anuales <- function(indices, variables, titulo, subtitulo, dest,
                                   etiqueta_fuente, fuente, decimales = 0,
                                   confiable = temporada_confiable(indices),
                                   rotulo_reserva = "Año parcial, provisional o con pocas detecciones") {
  columnas <- names(variables)
  # Un año sin detecciones (el recién iniciado) no tiene nada que mostrar,
  # aunque un índice de conteo dé 0 en lugar de NA.
  con_dato <- !is.na(indices[[columnas[1]]]) & indices$dtot > 0
  datos <- indices[con_dato, ] |>
    dplyr::mutate(
      lectura = ifelse(confiable[con_dato], "Año completo", rotulo_reserva)
    ) |>
    dplyr::select(anio_fuego, lectura, dplyr::all_of(columnas)) |>
    tidyr::pivot_longer(dplyr::all_of(columnas), names_to = "indice",
                        values_to = "valor") |>
    dplyr::mutate(indice = factor(indice, levels = columnas,
                                  labels = unname(variables)))
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = anio_fuego, y = valor, fill = lectura)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::geom_text(ggplot2::aes(label = num_es(valor, decimales)),
                       vjust = -0.4, size = 2.8, color = "grey30") +
    ggplot2::facet_wrap(~indice, ncol = 1, scales = "free_y") +
    ggplot2::scale_fill_manual(values = stats::setNames(c(COLOR_DETECCIONES, "grey65"),
                                                        c("Año completo", rotulo_reserva)),
                               name = NULL) +
    ggplot2::scale_x_continuous(breaks = datos$anio_fuego) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(
      title = titulo,
      subtitle = paste0(subtitulo, " — ", AREA_NOMBRE, ", ", etiqueta_fuente),
      x = "Año de fuego", y = NULL, caption = fuente
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 8),
      plot.title = ggplot2::element_text(face = "bold")
    )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 10, height = 7, dpi = 200)
  dest
}

grafico_concentracion <- function(indices, dest, etiqueta_fuente, fuente) {
  grafico_barras_anuales(
    indices,
    c(n50 = "N50: días que reúnen la mitad de las detecciones",
      c10 = "C10: % de las detecciones en los 10 días más activos"),
    titulo = "Concentración diaria del fuego por año",
    subtitulo = paste0("Días del año de fuego (setiembre–agosto) ordenados de ",
                       "mayor a menor número de detecciones"),
    dest, etiqueta_fuente, fuente
  )
}

# Días extremos por año: ND95 y D95pTOT; los años sin Aqua también en gris.
grafico_extremos <- function(indices, dest, etiqueta_fuente, fuente) {
  p95 <- unique(stats::na.omit(indices$p95))[1]
  grafico_barras_anuales(
    indices,
    c(nd95 = paste0("ND95: días con más de ", p95, " detecciones (percentil 95 del periodo base)"),
      d95ptot = "D95pTOT: % de las detecciones del año ocurridas en esos días"),
    titulo = "Días extremos de fuego por año",
    subtitulo = paste0("Umbral P95 = ", p95, " detecciones por día, percentil 95 de los ",
                       "días de fuego del periodo base"),
    dest, etiqueta_fuente, fuente,
    confiable = temporada_confiable(indices) & !indices$no_comparable,
    rotulo_reserva = "Año parcial, provisional, con pocas detecciones o sin Aqua"
  )
}

# Intensidad anual: FRP mediana y percentil 95 (MW) por año de fuego.
grafico_intensidad <- function(indices, dest, etiqueta_fuente, fuente) {
  grafico_barras_anuales(
    indices,
    c(frpi = "FRPI: FRP mediana de las detecciones (MW)",
      frp95 = "FRP95: percentil 95 de la FRP (MW)"),
    titulo = "Intensidad del fuego por año",
    subtitulo = paste0("Potencia radiativa de las detecciones de vegetación ",
                       "de cada año de fuego (setiembre–agosto)"),
    dest, etiqueta_fuente, fuente, decimales = 1
  )
}

# --- Estilo de QGIS para el GeoTIFF consolidado ------------------------------
# QGIS abre un ráster de más de tres bandas como «color multibanda» con las
# tres primeras, lo que oculta que hay seis índices. Un archivo .qml con el
# mismo nombre que el ráster se aplica solo al abrirlo: pseudocolor
# monobanda sobre LON, con la misma rampa y el mismo tope que el mapa
# estático. Solo simbología (styleCategories), para que QGIS lo acepte con
# cualquier versión 3.x.
escribir_estilo_qml <- function(dest, banda = 3L, minimo = 0, maximo = RASTER_LON_TOPE,
                                titulo = "Longitud de la temporada (días)") {
  cortes <- seq(minimo, maximo, length.out = 7)
  colores <- rev(viridisLite::inferno(length(cortes)))
  items <- sprintf('      <item alpha="255" value="%s" color="%s" label="%s"/>',
                   cortes, substr(colores, 1, 7),
                   ifelse(cortes >= maximo, paste0("≥ ", maximo), round(cortes)))
  xml <- c(
    "<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>",
    '<qgis version="3.34" styleCategories="Symbology">',
    "  <pipe>",
    "    <provider>",
    '      <resampling enabled="false" zoomedInResamplingMethod="nearestNeighbour" zoomedOutResamplingMethod="nearestNeighbour" maxOversampling="2"/>',
    "    </provider>",
    sprintf('    <rasterrenderer type="singlebandpseudocolor" band="%d" opacity="1" alphaBand="-1" nodataColor="" classificationMin="%s" classificationMax="%s">',
            banda, minimo, maximo),
    "      <rasterTransparency/>",
    "      <minMaxOrigin>",
    "        <limits>None</limits>",
    "        <extent>WholeRaster</extent>",
    "        <statAccuracy>Estimated</statAccuracy>",
    "        <cumulativeCutLower>0.02</cumulativeCutLower>",
    "        <cumulativeCutUpper>0.98</cumulativeCutUpper>",
    "        <stdDevFactor>2</stdDevFactor>",
    "      </minMaxOrigin>",
    "      <rastershader>",
    sprintf('        <colorrampshader colorRampType="INTERPOLATED" classificationMode="1" clip="0" minimumValue="%s" maximumValue="%s" labelPrecision="0">',
            minimo, maximo),
    items,
    sprintf('          <rampLegendSettings minimumLabel="" maximumLabel="" prefix="" suffix="" direction="0" orientation="2" useContinuousLegend="1"><numericFormat id="basic"><Option type="Map"><Option name="decimals" type="int" value="0"/></Option></numericFormat></rampLegendSettings>'),
    "        </colorrampshader>",
    "      </rastershader>",
    "    </rasterrenderer>",
    '    <brightnesscontrast brightness="0" contrast="0" gamma="1"/>',
    '    <huesaturation saturation="0" grayscaleMode="0" colorizeOn="0" colorizeRed="255" colorizeGreen="128" colorizeBlue="128" colorizeStrength="100" invertColors="0"/>',
    '    <rasterresampler maxOversampling="2"/>',
    "    <resamplingStage>resamplingFilter</resamplingStage>",
    "  </pipe>",
    "  <blendMode>0</blendMode>",
    sprintf("  <!-- %s: banda %d del GeoTIFF consolidado; ver README -->", titulo, banda),
    "</qgis>"
  )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  writeLines(xml, dest, useBytes = FALSE)
  dest
}

ayudantes_intensidad <- function(indices) {
  ok <- indices[temporada_confiable(indices), ]
  list(
    frpi_media = num_es(mean(ok$frpi), 1),
    frpi_min = num_es(min(ok$frpi), 1), anio_frpi_min = ok$anio_fuego[which.min(ok$frpi)],
    frpi_max = num_es(max(ok$frpi), 1), anio_frpi_max = ok$anio_fuego[which.max(ok$frpi)],
    frp95_media = num_es(mean(ok$frp95), 0),
    frp95_max = num_es(max(ok$frp95), 0), anio_frp95_max = ok$anio_fuego[which.max(ok$frp95)],
    aq_media = num_es(100 * mean(ok$aq[ok$aq > 0]), 0),
    noc_media = num_es(100 * mean(ok$noc), 0)
  )
}

ayudantes_extremos <- function(indices) {
  ok <- indices[temporada_confiable(indices) & !indices$no_comparable, ]
  list(
    p95 = unique(stats::na.omit(indices$p95))[1],
    n_anios = nrow(ok),
    nd95_media = num_es(mean(ok$nd95), 1),
    nd95_max = max(ok$nd95), anio_nd95_max = ok$anio_fuego[which.max(ok$nd95)],
    anios_sin_extremos = paste(ok$anio_fuego[ok$nd95 == 0], collapse = ", "),
    n_sin_extremos = sum(ok$nd95 == 0),
    d95ptot_media = num_es(mean(ok$d95ptot), 0),
    d95ptot_max = num_es(max(ok$d95ptot), 0), anio_d95ptot_max = ok$anio_fuego[which.max(ok$d95ptot)]
  )
}
