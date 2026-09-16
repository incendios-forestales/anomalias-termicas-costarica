# Mapa de fuentes estáticas: dónde están y qué firma tienen las detecciones
# de tipo 1 (volcán activo) y 2 (otra fuente estática) que la suite excluye.
#
# Implementa la subsección «Mapa de fuentes estáticas» del README, que es el
# contrato: métricas por celda de la grilla común sobre TODAS las detecciones
# estáticas del registro de la plataforma, una firma por reglas y un catálogo
# de localidades. Todo por plataforma; MODIS y VIIRS ven fuentes distintas y
# esa diferencia es un resultado.

FUENTES_MIN_DETECCIONES   <- 10L    # N_EST para dibujar una celda como fuente
FUENTES_MIN_CATALOGO      <- 20L    # N_EST para llevar localidad en el catálogo
FUENTES_DIURNA_REFLEJO    <- 0.9    # fracción diurna mínima del reflejo urbano
FUENTES_MANANA_REFLEJO    <- 0.7    # fracción de Terra mínima (MODIS)
FUENTES_DIURNA_NOCTURNA   <- 0.5    # fracción diurna máxima de la fuente nocturna
FUENTES_ANIOS_PERSISTENTE <- 3L     # años con detecciones para «persistente»

FIRMAS_FUENTES <- c("Cráter volcánico", "Fuente térmica nocturna persistente",
                    "Reflejo urbano diurno (probable)", "Sin clasificar")

# Catálogo de localidades de referencia: geocodificación inversa del
# centroide de las detecciones estáticas con Nominatim (OpenStreetMap), hecha
# el 2026-09-15 para las celdas con N_EST >= 20 en alguna plataforma. La
# localidad dice dónde buscar la fuente, no cuál es.
CATALOGO_FUENTES_ESTATICAS <- tibble::tribble(
  ~celda_id,      ~localidad,
  "c1045_m08475", "La Fortuna, San Carlos (volcán Arenal)",
  "c0995_m08475", "Los Ángeles de Barranca, Puntarenas",
  "c0985_m08375", "Casco urbano de Turrialba",
  "c1005_m08355", "Siquirres",
  "c1055_m08545", "Liberia",
  "c0995_m08305", "Limón",
  "c1035_m08515", "Cañas",
  "c1015_m08385", "Guápiles, Pococí",
  "c0995_m08425", "La Ribera y La Uruca, entre Belén y San José",
  "c0995_m08415", "Ulloa, Heredia",
  "c1005_m08455", "San Ramón",
  "c0975_m08395", "Aguacaliente de Cartago y Llanos de Santa Lucía, Paraíso",
  "c0935_m08375", "San Isidro de El General",
  "c1025_m08565", "Santa Cruz, Guanacaste",
  "c1005_m08555", "Nicoya",
  "c0995_m08385", "Santa Cruz de Turrialba (cráter del Turrialba)",
  "c1015_m08425", "San Juan de Poás (cráter del Poás)",
  "c1015_m08525", "Colorado de Abangares"
)
CATALOGO_FUENTES_FECHA <- as.Date("2026-09-15")

# --- Métricas por celda ------------------------------------------------------------

# Una fila por celda con alguna detección de tipo 1 o 2. `puntos` es el
# registro completo de la plataforma (todos los tipos), porque PCT_EST se
# calcula sobre todas las detecciones de la celda. La confianza solo se
# resume cuando es numérica (MODIS); en VIIRS es categórica y queda NA.
metricas_fuentes_estaticas <- function(puntos, grilla, satelite_control) {
  if (!"type" %in% names(puntos)) puntos$type <- NA_integer_
  celdas <- asignar_celda(puntos, grilla)
  d <- puntos |>
    sf::st_drop_geometry() |>
    dplyr::inner_join(celdas, by = "id_deteccion")
  totales <- dplyr::count(d, celda_id, name = "n_total")
  est <- d[!is.na(d$type) & d$type %in% c(1L, 2L), ]
  conf <- suppressWarnings(as.numeric(est$confidence))
  est$conf_num <- if (all(is.na(conf))) NA_real_ else conf
  est |>
    dplyr::mutate(anio_fuego = anio_fuego(acq_date)) |>
    dplyr::summarise(
      n_est = dplyr::n(),
      n_volcan = sum(type == 1L),
      n_fija = sum(type == 2L),
      anios_est = dplyr::n_distinct(anio_fuego),
      primer_anio = min(anio_fuego), ultimo_anio = max(anio_fuego),
      diurna = round(mean(daynight == "D"), 2),
      manana = if (is.na(satelite_control)) NA_real_ else
        round(mean(satellite != satelite_control), 2),
      frp_est = round(stats::median(frp, na.rm = TRUE), 1),
      conf_est = if (all(is.na(conf_num))) NA_real_ else
        round(stats::median(conf_num, na.rm = TRUE)),
      lat = round(mean(latitude), 4), lon = round(mean(longitude), 4),
      disp_m = round(sqrt(stats::var(latitude) +
                            (stats::sd(longitude) * cos(mean(latitude) * pi / 180))^2) * 111000),
      .by = celda_id
    ) |>
    dplyr::mutate(disp_m = tidyr::replace_na(disp_m, 0)) |>
    dplyr::inner_join(totales, by = "celda_id") |>
    dplyr::mutate(pct_est = round(100 * n_est / n_total, 1)) |>
    dplyr::relocate(n_total, pct_est, .after = n_fija) |>
    firmar_fuentes(satelite_control) |>
    dplyr::left_join(CATALOGO_FUENTES_ESTATICAS, by = "celda_id") |>
    dplyr::arrange(dplyr::desc(n_est))
}

# Firma por reglas (README, «Firma»), en orden: cráter volcánico, fuente
# térmica nocturna persistente, reflejo urbano diurno (probable), sin
# clasificar. La condición de Terra solo aplica donde hay satélite de control.
firmar_fuentes <- function(metricas, satelite_control,
                           minimo = FUENTES_MIN_DETECCIONES) {
  con_manana <- !is.na(satelite_control)
  metricas |>
    dplyr::mutate(
      es_fuente = n_est >= minimo,
      firma = dplyr::case_when(
        n_volcan > n_fija ~ FIRMAS_FUENTES[1],
        diurna <= FUENTES_DIURNA_NOCTURNA & anios_est >= FUENTES_ANIOS_PERSISTENTE ~ FIRMAS_FUENTES[2],
        diurna >= FUENTES_DIURNA_REFLEJO &
          (!con_manana | manana >= FUENTES_MANANA_REFLEJO) ~ FIRMAS_FUENTES[3],
        TRUE ~ FIRMAS_FUENTES[4]
      ),
      firma = factor(firma, levels = FIRMAS_FUENTES)
    )
}

# --- Tablas -------------------------------------------------------------------------

# Widget DT de las celdas fuente (N_EST >= 10).
crear_tabla_fuentes <- function(metricas, etiqueta_fuente) {
  datos <- metricas |>
    dplyr::filter(es_fuente) |>
    dplyr::transmute(
      celda_id, localidad = tidyr::replace_na(localidad, ""),
      firma = as.character(firma), n_volcan, n_fija,
      pct_est = num_es(pct_est, 0),
      anios = paste0(anios_est, " (", primer_anio, "–", ultimo_anio, ")"),
      diurna = num_es(100 * diurna, 0),
      manana = ifelse(is.na(manana), "", num_es(100 * manana, 0)),
      frp_est = num_es(frp_est, 1),
      conf_est = ifelse(is.na(conf_est), "", as.character(conf_est)),
      coordenadas = paste0(num_es(lat, 4), ", ", num_es(lon, 4)),
      disp_m
    )
  DT::datatable(
    datos,
    colnames = c("Celda", "Localidad de referencia", "Firma", "Volcán (tipo 1)",
                 "Fija (tipo 2)", "% de la celda", "Años (primero–último)",
                 "% diurnas", "% de Terra", "FRP mediana (MW)", "Confianza mediana",
                 "Centroide (lat, lon)", "Dispersión (m)"),
    caption = paste0("Celdas con al menos ", FUENTES_MIN_DETECCIONES,
                     " detecciones de fuente estática — ", AREA_NOMBRE, ", ",
                     etiqueta_fuente),
    options = list(pageLength = 25, dom = "t", scrollX = TRUE),
    rownames = FALSE
  )
}

# --- Mapa -----------------------------------------------------------------------------

# Celdas fuente coloreadas por firma con el número de detecciones; las celdas
# con menos de FUENTES_MIN_DETECCIONES estáticas, como puntos grises.
mapa_fuentes_estaticas <- function(metricas, grilla, area, dest, etiqueta_fuente,
                                   fuente) {
  datos <- dplyr::inner_join(sf::st_drop_geometry(grilla), metricas, by = "celda_id")
  geometria <- sf::st_geometry(grilla)[match(datos$celda_id, grilla$celda_id)]
  celdas <- a_crtm05(sf::st_sf(datos, geometry = geometria))
  fuentes <- celdas[celdas$es_fuente, ]
  menores <- celdas[!celdas$es_fuente, ]
  centros <- sf::st_sf(sf::st_drop_geometry(fuentes),
                       geometry = sf::st_centroid(sf::st_geometry(fuentes)))
  colores <- stats::setNames(c("#7b3294", "#d7301f", "#f4a582", "grey60"), FIRMAS_FUENTES)
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = area, fill = "grey97", color = "grey30", linewidth = 0.4) +
    ggplot2::geom_sf(data = fuentes, ggplot2::aes(fill = firma), color = "white",
                     linewidth = 0.3) +
    ggplot2::geom_sf(data = sf::st_centroid(sf::st_geometry(menores)), color = "grey55",
                     size = 0.9) +
    ggplot2::geom_sf_text(data = centros, ggplot2::aes(label = n_est), size = 2.6,
                          color = "grey10", fontface = "bold") +
    ggplot2::scale_fill_manual(values = colores, name = "Firma", drop = FALSE) +
    ggplot2::labs(
      title = "Fuentes estáticas: dónde están las detecciones que la suite excluye",
      subtitle = paste0("Detecciones de tipo 1 (volcán) y 2 (fuente estática) de todo el ",
                        "registro, por celda de 0,1° y con su número\n",
                        "Celdas con menos de ", FUENTES_MIN_DETECCIONES,
                        " como puntos grises — ", AREA_NOMBRE, ", ", etiqueta_fuente),
      caption = fuente, x = NULL, y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "grey92", linewidth = 0.3),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2))
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 9, height = 7.5, dpi = 200)
  dest
}

# --- Cifras para la prosa ---------------------------------------------------------------

ayudantes_fuentes <- function(metricas) {
  f <- metricas[metricas$es_fuente, ]
  por_firma <- function(nombre) f[as.character(f$firma) == nombre, ]
  lista <- function(x) if (nrow(x) == 0) "" else
    paste(ifelse(is.na(x$localidad), x$celda_id, x$localidad), collapse = "; ")
  volcan <- por_firma(FIRMAS_FUENTES[1]); noct <- por_firma(FIRMAS_FUENTES[2])
  refl <- por_firma(FIRMAS_FUENTES[3]); sinc <- por_firma(FIRMAS_FUENTES[4])
  list(
    n_celdas = nrow(metricas), n_fuentes = nrow(f),
    n_est_total = sum(metricas$n_est),
    pct_en_fuentes = num_es(100 * sum(f$n_est) / sum(metricas$n_est), 0),
    n_volcan = nrow(volcan), volcanes = lista(volcan),
    n_nocturna = nrow(noct), nocturnas = lista(noct),
    n_reflejo = nrow(refl), reflejos = lista(refl),
    n_sin = nrow(sinc), sin_clasificar = lista(sinc),
    mayor = if (nrow(f)) ifelse(is.na(f$localidad[1]), f$celda_id[1], f$localidad[1]) else "",
    mayor_n = if (nrow(f)) f$n_est[1] else 0L,
    mayor_firma = if (nrow(f)) tolower(as.character(f$firma[1])) else "",
    reflejo_diurna = if (nrow(refl)) num_es(100 * stats::median(refl$diurna), 0) else "",
    reflejo_manana = if (nrow(refl) && !all(is.na(refl$manana))) num_es(100 * stats::median(refl$manana, na.rm = TRUE), 0) else "",
    reflejo_frp = if (nrow(refl)) num_es(stats::median(refl$frp_est), 1) else "",
    nocturna_diurna = if (nrow(noct)) num_es(100 * stats::median(noct$diurna), 0) else ""
  )
}
