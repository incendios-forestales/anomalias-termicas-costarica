# Desagregación de los índices de temporada por área de conservación (AC).
#
# Implementa la subsección «Desagregación por área de conservación» del
# README, que es el contrato. Reutiliza las funciones de R/temporada.R sobre
# las detecciones de cada AC: la serie diaria y los índices anuales por AC
# salen de serie_diaria() e indices_temporada() con un umbral propio, y el
# consolidado por AC sale de indices_consolidados() e indices_frecuencia()
# tomando cada AC como una «celda» con su superficie terrestre, para que
# ninguna definición ni umbral difiera entre las dos escalas. Todo es por
# plataforma; aquí nunca se juntan detecciones de dos plataformas.

# Umbrales por AC y año de fuego (README, «Índices anuales por AC»).
AC_MIN_DETECCIONES_TEMPORADA <- 100L   # INI, FIN, LON, N50, C10, FRP95
AC_MIN_DETECCIONES_FRP       <- 30L    # FRPI, AQ, NOC
AC_MIN_DIAS_BASE_P95         <- 100L   # días de fuego del AC en el periodo base

# --- Áreas y asignación ---------------------------------------------------------

# Capa de AC en CRTM05 con la superficie terrestre de cada una (intersección
# con el límite nacional), en km². Es la «grilla» del consolidado por AC.
superficie_ac <- function(ac, pais) {
  acp <- preparar_ac(ac)
  tierra <- sf::st_union(a_crtm05(pais))
  acp$area_km2 <- vapply(seq_len(nrow(acp)), function(i) {
    round(as.numeric(sf::st_area(sf::st_intersection(sf::st_geometry(acp)[i],
                                                     tierra))) / 1e6, 1)
  }, numeric(1))
  acp
}

# Tabla (id_deteccion, siglas_ac, nombre_ac) de las detecciones; la llave es
# id_deteccion, como en el resto del proyecto.
asignar_ac_ids <- function(puntos, ac) {
  asignar_ac(puntos, ac) |>
    sf::st_drop_geometry() |>
    dplyr::select(id_deteccion, siglas_ac, nombre_ac)
}

# --- Índices anuales por AC ------------------------------------------------------

# Una fila por AC y año de fuego con los índices de la suite nacional
# calculados sobre las detecciones del AC, los umbrales del README y la
# anomalía estandarizada de DTOT sobre el periodo base. Las detecciones «Sin
# asignar» no reciben índices.
indices_temporada_ac <- function(puntos, ac_ids, rangos, anios_base,
                                 satelite_control,
                                 minimo_temporada = AC_MIN_DETECCIONES_TEMPORADA,
                                 minimo_frp = AC_MIN_DETECCIONES_FRP,
                                 minimo_dias_p95 = AC_MIN_DIAS_BASE_P95) {
  df <- puntos |>
    sf::st_drop_geometry() |>
    dplyr::inner_join(ac_ids, by = "id_deteccion") |>
    dplyr::filter(siglas_ac != ETIQUETA_SIN_AC)
  purrr::map(split(df, df$siglas_ac), function(d) {
    diaria <- serie_diaria(d, rangos)
    anual <- indices_temporada(diaria, rangos, minimo = minimo_temporada)
    intensidad <- indices_intensidad(d, satelite_control)
    dias_base <- sum(diaria$anio_fuego %in% anios_base & diaria$detecciones > 0)
    extremos <- if (dias_base >= minimo_dias_p95) {
      indices_extremos(diaria, umbral_p95(diaria, anios_base))
    } else {
      tibble::tibble(anio_fuego = anual$anio_fuego, nd95 = NA_integer_,
                     d95p = NA_integer_, d95ptot = NA_real_, p95 = NA_real_)
    }
    tabla <- unir_extremos(unir_intensidad(anual, intensidad), extremos,
                           satelite_control)
    # Umbrales: bajo el mínimo, el índice no existe (NA), no un valor ruidoso.
    pocas <- tabla$dtot < minimo_temporada
    for (col in c("ini_dia", "ini_fecha", "fin_dia", "fin_fecha", "lon",
                  "n50", "c10", "frp95")) {
      tabla[[col]][pocas] <- NA
    }
    pocas_frp <- tabla$dtot < minimo_frp
    for (col in c("frpi", "aq", "noc")) tabla[[col]][pocas_frp] <- NA
    base <- tabla$dtot[tabla$anio_fuego %in% anios_base]
    z <- if (length(base) < 2 || stats::sd(base) == 0) NA_real_ else
      round((tabla$dtot - mean(base)) / stats::sd(base), 2)
    tabla |>
      dplyr::mutate(siglas_ac = d$siglas_ac[1], nombre_ac = d$nombre_ac[1],
                    .before = 1) |>
      dplyr::mutate(z_dtot = z, dias_base_p95 = dias_base, .after = p95)
  }) |>
    purrr::list_rbind() |>
    dplyr::arrange(siglas_ac, anio_fuego)
}

# --- Consolidado por AC ------------------------------------------------------------

# Consolidado por AC con las mismas funciones que el ráster por celda: cada
# AC entra como una celda (celda_id = siglas) con su superficie terrestre.
# Sin FREC: todas las AC arden todos los años.
consolidar_ac <- function(puntos, ac_ids, areas, anios_referencia, anios_base,
                          satelite_control) {
  celdas <- ac_ids |>
    dplyr::filter(siglas_ac != ETIQUETA_SIN_AC) |>
    dplyr::transmute(id_deteccion, celda_id = siglas_ac)
  grilla <- areas |>
    dplyr::transmute(celda_id = siglas_ac, area_km2)
  consolidado <- indices_consolidados(puntos, celdas, anios_referencia)
  frecuencia <- indices_frecuencia(puntos, celdas, grilla, anios_base,
                                   satelite_control)
  nombres <- sf::st_drop_geometry(areas)[, c("siglas_ac", "nombre_ac")]
  unir_consolidados(consolidado, frecuencia, anios_referencia) |>
    dplyr::rename(siglas_ac = celda_id) |>
    dplyr::select(-frec, -anios) |>
    dplyr::left_join(nombres, by = "siglas_ac") |>
    dplyr::relocate(nombre_ac, .after = siglas_ac) |>
    dplyr::arrange(dplyr::desc(dtot))
}

# --- Tablas ------------------------------------------------------------------------

# Widget DT del consolidado por AC (para el reporte Quarto).
crear_tabla_ac_consolidado <- function(consolidado, etiqueta_fuente,
                                       con_aq = TRUE) {
  ref <- inicio_anio_fuego(2002L)
  fecha_ref <- function(dia) ifelse(is.na(dia), "",
                                    fecha_es(ref + dia - 1L, con_anio = FALSE))
  datos <- consolidado |>
    dplyr::transmute(
      siglas_ac, nombre_ac, area_km2 = round(area_km2), dtot,
      inicio = fecha_ref(ini_dia), fin = fecha_ref(fin_dia), lon,
      fuera = num_es(fuera, 0),
      n50f = ifelse(is.na(n50f), "", num_es(n50f, 2)),
      dens = ifelse(is.na(dens), "", num_es(dens, 3)),
      frpi = ifelse(is.na(frpi), "", num_es(frpi, 1)),
      aq = ifelse(is.na(aq), "", num_es(100 * aq, 0)),
      nota = ifelse(sin_estacion, "sin estación definida",
                    ifelse(!valida, "bajo el umbral", ""))
    )
  nombres <- c("AC", "Nombre", "km² de tierra", "Detecciones", "Inicio (10 %)",
               "Fin (90 %)", "Longitud (días)", "Fuera de dic–may (%)", "N50F",
               "Det./km²/año (base)", "FRP mediana (MW)", "De Aqua (%)", "Nota")
  if (!con_aq) {
    datos$aq <- NULL
    nombres <- nombres[nombres != "De Aqua (%)"]
  }
  DT::datatable(
    datos, colnames = nombres,
    caption = paste0("Temporada consolidada por área de conservación — ",
                     AREA_NOMBRE, ", ", etiqueta_fuente),
    options = list(pageLength = 12, dom = "t", scrollX = TRUE),
    rownames = FALSE
  )
}

# --- Figuras -------------------------------------------------------------------------

# Orden de las AC en las figuras: por detecciones acumuladas, de más a menos.
orden_ac <- function(tabla) {
  tabla |>
    dplyr::summarise(total = sum(dtot), .by = siglas_ac) |>
    dplyr::arrange(total) |>
    dplyr::pull(siglas_ac)
}

# Mosaico AC × año de fuego de un índice anual: "lon" (días, escala de los
# mapas por celda) o "z_dtot" (anomalía estandarizada, divergente). Las
# casillas sin índice van en gris; los años parciales o provisionales llevan
# el valor en cursiva gris.
grafico_mosaico_ac <- function(tabla, variable, dest, etiqueta_fuente, fuente,
                               tope_lon = RASTER_LON_TOPE, tope_z = 2.5) {
  datos <- tabla |>
    dplyr::filter(dtot > 0 | !parcial) |>
    dplyr::mutate(
      siglas_ac = factor(siglas_ac, levels = orden_ac(tabla)),
      valor = .data[[variable]],
      reserva = parcial | provisional,
      rotulo = ifelse(is.na(valor), "", num_es(valor, if (variable == "lon") 0 else 1))
    )
  es_lon <- variable == "lon"
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = anio_fuego, y = siglas_ac, fill = valor)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = rotulo,
                                    color = ifelse(reserva, "reserva", "ok"),
                                    fontface = ifelse(reserva, "italic", "plain")),
                       size = 2.6, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c(ok = "grey15", reserva = "grey55")) +
    ggplot2::scale_x_continuous(breaks = unique(datos$anio_fuego), expand = ggplot2::expansion(0)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(0)) +
    ggplot2::labs(
      x = "Año de fuego", y = NULL, caption = fuente,
      title = if (es_lon) "Longitud de la temporada de fuego por área de conservación y año" else
        "Anomalía del número de detecciones por área de conservación y año",
      subtitle = paste0(
        if (es_lon) paste0("Del 10 % al 90 % de las detecciones del AC en cada año de fuego; ",
                           "en gris, años con menos de ", AC_MIN_DETECCIONES_TEMPORADA,
                           " detecciones") else
          "(DTOT − media del periodo base del AC) / desviación típica; positivo, más fuego que lo habitual en esa AC",
        "\nEn cursiva gris, años parciales o provisionales — ", AREA_NOMBRE, ", ",
        etiqueta_fuente)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 8),
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold")
    )
  p <- if (es_lon) {
    p + ggplot2::scale_fill_viridis_c(
      option = "inferno", direction = -1, na.value = "grey88",
      limits = c(0, tope_lon), oob = scales::oob_squish, name = "Longitud (días)",
      labels = function(x) ifelse(x >= tope_lon, paste0("≥ ", x), x))
  } else {
    p + ggplot2::scale_fill_distiller(
      palette = "RdBu", direction = -1, limits = c(-tope_z, tope_z),
      oob = scales::oob_squish, na.value = "grey88", name = "Anomalía\n(desv. típicas)")
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 11, height = 4.5, dpi = 200)
  dest
}

# Coropleta de un índice consolidado por AC ("lon", "ini_dia" o "dens"), con
# las siglas en el centroide. Las AC sin estación definida van con asterisco.
mapa_ac <- function(consolidado, areas, area_pais, dest, variable,
                    etiqueta_fuente, fuente) {
  datos <- dplyr::inner_join(sf::st_drop_geometry(areas)[, "siglas_ac", drop = FALSE],
                             consolidado, by = "siglas_ac")
  geometria <- sf::st_geometry(areas)[match(datos$siglas_ac, areas$siglas_ac)]
  celdas <- sf::st_sf(datos, geometry = geometria) |>
    dplyr::mutate(rotulo = paste0(siglas_ac, ifelse(sin_estacion, "*", "")))
  centros <- sf::st_point_on_surface(sf::st_geometry(celdas))
  rotulos <- sf::st_sf(rotulo = celdas$rotulo, geometry = centros)
  ref <- inicio_anio_fuego(2002L)
  usa_base <- variable == "dens"
  periodo <- if (usa_base) {
    paste0(min(consolidado$base_inicio), "–", max(consolidado$base_fin), ", el periodo base")
  } else paste0(min(consolidado$anio_inicio), "–", max(consolidado$anio_fin))
  titulo <- c(lon = "Longitud de la temporada de fuego por área de conservación",
              ini_dia = "Inicio de la temporada de fuego por área de conservación",
              dens = "Densidad del fuego por área de conservación")[[variable]]
  rotulo <- c(lon = "Longitud (días)", ini_dia = "Inicio (10 %)",
              dens = "Detecciones por\nkm² y año")[[variable]]
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = celdas, ggplot2::aes(fill = .data[[variable]]),
                     color = "white", linewidth = 0.4) +
    ggplot2::geom_sf(data = area_pais, fill = NA, color = "grey30", linewidth = 0.3) +
    ggplot2::geom_sf_text(data = rotulos, ggplot2::aes(label = rotulo), size = 3,
                          color = "grey10", fontface = "bold") +
    ggplot2::scale_fill_viridis_c(
      option = c(lon = "inferno", ini_dia = "viridis", dens = "rocket")[[variable]],
      direction = if (variable %in% c("lon", "dens")) -1 else 1,
      trans = if (variable == "dens") "sqrt" else "identity",
      na.value = "grey85", name = rotulo,
      limits = if (variable == "lon") c(0, RASTER_LON_TOPE) else NULL,
      oob = scales::oob_squish,
      labels = if (variable == "ini_dia") {
        function(x) paste(sub(" .*", "", format(ref + x - 1L, "%d")),
                          MESES_ES[lubridate::month(ref + x - 1L)])
      } else if (variable == "lon") {
        function(x) ifelse(x >= RASTER_LON_TOPE, paste0("≥ ", x), x)
      } else ggplot2::waiver()
    ) +
    ggplot2::labs(
      title = titulo,
      subtitle = paste0("Detecciones de vegetación de los años de fuego ", periodo,
                        "; mismas definiciones y umbrales que el ráster por celda\n",
                        "Con asterisco, AC sin estación definida (más de ", RASTER_FUERA_MAX_PCT,
                        " % de las detecciones fuera de diciembre a mayo)\n",
                        AREA_NOMBRE, ", ", etiqueta_fuente),
      # Los límites de las AC son del SINAC; `fuente` es el pie de FIRMS.
      caption = paste0(fuente, " y SINAC"), x = NULL, y = NULL
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

# --- Cifras para la prosa ------------------------------------------------------------

ayudantes_temporada_ac <- function(tabla, consolidado) {
  ok <- tabla[!tabla$parcial & !tabla$provisional, ]
  con_lon <- ok[!is.na(ok$lon), ]
  ref <- inicio_anio_fuego(2002L)
  fecha_ref <- function(dia) fecha_es(ref + dia - 1L, con_anio = FALSE)
  por_ac <- con_lon |>
    dplyr::summarise(n = dplyr::n(), lon_media = mean(lon), .by = c(siglas_ac, nombre_ac))
  cons_ok <- consolidado[!is.na(consolidado$lon), ]
  sin_est <- consolidado[consolidado$sin_estacion, ]
  z_ok <- ok[!is.na(ok$z_dtot), ]
  anio_z <- z_ok |>
    dplyr::summarise(n_pos = sum(z_dtot > 1), n_neg = sum(z_dtot < -1), .by = anio_fuego)
  list(
    n_ac = dplyr::n_distinct(tabla$siglas_ac),
    n_ac_anios = nrow(ok),
    n_ac_anios_con_lon = nrow(con_lon),
    pct_con_lon = num_es(100 * nrow(con_lon) / nrow(ok), 0),
    ac_siempre = paste(por_ac$siglas_ac[por_ac$n == max(por_ac$n)], collapse = ", "),
    n_anios_ok = dplyr::n_distinct(ok$anio_fuego),
    ac_nunca = paste(setdiff(unique(tabla$siglas_ac), por_ac$siglas_ac), collapse = ", "),
    lon_ac_max = por_ac$siglas_ac[which.max(por_ac$lon_media)],
    lon_ac_max_valor = num_es(max(por_ac$lon_media), 0),
    lon_ac_min = por_ac$siglas_ac[which.min(por_ac$lon_media)],
    lon_ac_min_valor = num_es(min(por_ac$lon_media), 0),
    anio_mas_pos = anio_z$anio_fuego[which.max(anio_z$n_pos)],
    n_mas_pos = max(anio_z$n_pos),
    anio_mas_neg = anio_z$anio_fuego[which.max(anio_z$n_neg)],
    n_mas_neg = max(anio_z$n_neg),
    cons_lon_max = cons_ok$siglas_ac[which.max(cons_ok$lon)],
    cons_lon_max_valor = max(cons_ok$lon),
    cons_lon_min = cons_ok$siglas_ac[which.min(cons_ok$lon)],
    cons_lon_min_valor = min(cons_ok$lon),
    cons_ini_min = cons_ok$siglas_ac[which.min(cons_ok$ini_dia)],
    cons_ini_min_fecha = fecha_ref(min(cons_ok$ini_dia)),
    cons_ini_max = cons_ok$siglas_ac[which.max(cons_ok$ini_dia)],
    cons_ini_max_fecha = fecha_ref(max(cons_ok$ini_dia)),
    cons_sin_estacion = paste(sin_est$siglas_ac, collapse = " y "),
    n_sin_estacion = nrow(sin_est),
    dens_max = consolidado$siglas_ac[which.max(consolidado$dens)],
    dens_max_valor = num_es(max(consolidado$dens, na.rm = TRUE), 3),
    dens_min = consolidado$siglas_ac[which.min(consolidado$dens)],
    dens_min_valor = num_es(min(consolidado$dens, na.rm = TRUE), 3),
    razon_dens = num_es(max(consolidado$dens, na.rm = TRUE) / min(consolidado$dens, na.rm = TRUE), 0)
  )
}
