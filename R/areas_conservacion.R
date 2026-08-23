# Desagregación espacial por área de conservación del SINAC.
#
# A escala nacional interesa no solo cuándo ocurre el fuego sino dónde. La
# unidad de desagregación es el área de conservación (AC): la división
# administrativa con que el SINAC gestiona el territorio (y el fuego), servida
# por el mismo geoserver que las demás capas del proyecto
# (PNE:areas_conservacion, 10 unidades terrestres).
#
# Las detecciones se asignan por punto y los píxeles de área quemada por
# CENTROIDE: un píxel de 500 m puede cruzar el límite entre dos AC y partirlo
# geométricamente repartiría hectáreas con una precisión que el producto no
# tiene. Las detecciones que no caen en ninguna AC (desajustes de borde entre
# la capa del SINAC y el límite IGN 1:5000, típicamente en la línea de costa)
# se agrupan como "Sin asignar" en vez de perderse en silencio.

ETIQUETA_SIN_AC <- "Sin asignar"

# Normaliza la capa de AC a las dos columnas que usa el módulo.
preparar_ac <- function(ac) {
  a_crtm05(ac) |>
    sf::st_make_valid() |>
    dplyr::select(siglas_ac, nombre_ac)
}

# Asigna a cada detección su AC (columnas siglas_ac / nombre_ac).
asignar_ac <- function(puntos, ac) {
  sf::st_join(a_crtm05(puntos), preparar_ac(ac), join = sf::st_intersects,
              left = TRUE) |>
    dplyr::mutate(
      siglas_ac = tidyr::replace_na(siglas_ac, ETIQUETA_SIN_AC),
      nombre_ac = tidyr::replace_na(nombre_ac, ETIQUETA_SIN_AC)
    )
}

# Detecciones por AC y año (data frame largo, sin geometría). Base de la
# tabla CSV y del gráfico.
detecciones_por_ac <- function(puntos, ac) {
  asignar_ac(puntos, ac) |>
    sf::st_drop_geometry() |>
    dplyr::count(siglas_ac, nombre_ac, anio, name = "detecciones")
}

# Hectáreas quemadas por AC y año, asignando cada píxel por su centroide.
quemas_por_ac <- function(quemas, ac) {
  if (nrow(quemas) == 0) {
    return(tibble::tibble(siglas_ac = character(), nombre_ac = character(),
                          anio = integer(), hectareas = numeric()))
  }
  centroides <- sf::st_set_geometry(quemas,
                                    sf::st_centroid(sf::st_geometry(quemas)))
  asignar_ac(centroides, ac) |>
    sf::st_drop_geometry() |>
    dplyr::summarise(hectareas = sum(area_ha),
                     .by = c(siglas_ac, nombre_ac, anio))
}

# Resumen total por AC: detecciones, porcentaje del total y hectáreas
# quemadas del período. Une por siglas: una AC sin quemas (o sin detecciones)
# no se pierde.
resumen_por_ac <- function(detecciones_ac, quemas_ac) {
  totales <- detecciones_ac |>
    dplyr::summarise(detecciones = sum(detecciones),
                     .by = c(siglas_ac, nombre_ac))
  hectareas <- quemas_ac |>
    dplyr::summarise(hectareas = round(sum(hectareas)),
                     .by = c(siglas_ac, nombre_ac))
  totales |>
    dplyr::full_join(hectareas, by = c("siglas_ac", "nombre_ac")) |>
    dplyr::mutate(
      detecciones = tidyr::replace_na(detecciones, 0L),
      hectareas = tidyr::replace_na(hectareas, 0),
      pct_detecciones = round(100 * detecciones / sum(detecciones), 1)
    ) |>
    dplyr::relocate(pct_detecciones, .after = detecciones) |>
    dplyr::arrange(dplyr::desc(detecciones))
}

# Gráfico de barras: detecciones por AC en todo el período, rotuladas por
# siglas (el nombre completo va en el tooltip de la versión interactiva).
crear_grafico_ac <- function(detecciones_ac, quemas_ac, fuente,
                             interactivo = FALSE) {
  resumen <- resumen_por_ac(detecciones_ac, quemas_ac) |>
    dplyr::mutate(
      siglas_ac = factor(siglas_ac, levels = rev(siglas_ac)),
      etiqueta = paste0(
        nombre_ac,
        "<br>Detecciones: ", num_es(detecciones, 0),
        " (", num_es(pct_detecciones), " %)",
        "<br>Área quemada: ", num_es(hectareas, 0), " ha"
      )
    )
  p <- ggplot2::ggplot(resumen,
                       ggplot2::aes(x = detecciones, y = siglas_ac,
                                    text = etiqueta)) +
    ggplot2::geom_col(fill = COLOR_DETECCIONES, width = 0.7) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)),
                                labels = \(x) num_es(x, 0)) +
    ggplot2::labs(x = "Detecciones", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "grey92")
    )
  titulo <- "Detecciones por área de conservación"
  subtitulo <- "Total del período — límites administrativos del SINAC"
  if (interactivo) {
    plotly::ggplotly(p, tooltip = "text") |>
      configurar_plotly(
        titulo, subtitulo, fuente = fuente,
        margen_superior = 130, margen_inferior = 110,
        desplazamiento_fuente = -78
      )
  } else {
    p + ggplot2::labs(title = titulo, subtitle = subtitulo, caption = fuente) +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  }
}

# PNG del gráfico por AC (target con format = "file").
grafico_ac <- function(detecciones_ac, quemas_ac, dest, fuente) {
  p <- crear_grafico_ac(detecciones_ac, quemas_ac, fuente, interactivo = FALSE)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 8, height = 4.5, dpi = 200)
  dest
}

# Tabla CSV de detecciones por AC y año (target con format = "file").
tabla_ac_csv <- function(detecciones_ac, quemas_ac, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  detecciones_ac |>
    dplyr::full_join(quemas_ac,
                     by = c("siglas_ac", "nombre_ac", "anio")) |>
    dplyr::mutate(
      detecciones = tidyr::replace_na(detecciones, 0L),
      hectareas = round(tidyr::replace_na(hectareas, 0))
    ) |>
    dplyr::arrange(siglas_ac, anio) |>
    readr::write_csv(dest)
  dest
}

# Widget DT del resumen por AC (para incrustar en el reporte Quarto).
crear_tabla_ac <- function(detecciones_ac, quemas_ac, etiqueta_fuente,
                           etiqueta_ba) {
  DT::datatable(
    resumen_por_ac(detecciones_ac, quemas_ac),
    colnames = c("Siglas", "Área de conservación", "Detecciones",
                 "% del total", "Hectáreas quemadas"),
    caption = paste0("Detecciones (", etiqueta_fuente, ") y área quemada (",
                     etiqueta_ba, ") por área de conservación del SINAC, ",
                     "total del período"),
    options = list(pageLength = 15, dom = "t"),
    rownames = FALSE
  ) |>
    formato_dt_es("pct_detecciones", decimales = 1)
}
