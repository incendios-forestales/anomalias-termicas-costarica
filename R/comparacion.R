# Comparación entre plataformas en el periodo de traslape.
#
# Implementa la subsección «Comparación entre plataformas en el traslape» del
# README, que es el contrato. Es el único módulo en que dos plataformas
# aparecen en un mismo producto, y lo hacen como dos columnas o dos mapas
# lado a lado: aquí NUNCA se juntan detecciones de dos plataformas. Todo se
# calcula por par ordenado (A, B) y las diferencias son siempre B − A.
#
# Tres productos: índices anuales lado a lado (los comparables en nivel),
# anomalías estandarizadas de los índices de conteo, y consolidados por celda
# recalculados sobre los años del traslape con clase de acuerdo y diferencias.

# Colores de las dos plataformas de un par: A cálido (la serie de referencia,
# el mismo naranja de las detecciones), B frío. Función y no constante porque
# tar_source() carga este archivo antes que constantes.R.
colores_par <- function() c(a = COLOR_DETECCIONES, b = "#2c7fb8")

# Índices comparables en nivel (fracciones y fechas) y dependientes del conteo
# (README, «Extensión a las plataformas VIIRS»).
INDICES_NIVEL  <- c("ini_dia", "fin_dia", "lon", "n50", "c10")
INDICES_CONTEO <- c("dtot", "nd95", "d95ptot")

# Diferencia de INI que se considera acuerdo entre plataformas (días).
COMPARACION_ACUERDO_INI_DIAS <- 15L

# Clases de acuerdo por celda, con su código en el ráster.
CLASES_ACUERDO <- c(ambas = 1L, solo_b = 2L, solo_a = 3L, ninguna = 4L,
                    discordante = 5L)

# --- Pares ---------------------------------------------------------------------

# Pares ordenados (A, B) de plataformas con periodo base, en el orden de
# PLATAFORMAS: A es la que aparece antes en la tabla (la serie más larga).
# Con menos de dos plataformas no hay pares.
pares_comparacion <- function() {
  con <- plataformas_con_indices()
  if (nrow(con) < 2) {
    return(tibble::tibble(par = character(), a = character(), b = character()))
  }
  idx <- utils::combn(nrow(con), 2)
  tibble::tibble(a = con$clave[idx[1, ]], b = con$clave[idx[2, ]]) |>
    dplyr::mutate(par = paste0(a, "_", b), .before = 1)
}

# Años de fuego completos y no provisionales en ambas plataformas.
anios_traslape <- function(temporada_a, temporada_b) {
  intersect(anios_referencia(temporada_a), anios_referencia(temporada_b))
}

# --- Producto 1 y 2: tabla anual lado a lado -----------------------------------

# Anomalía estandarizada: (x − media) / desviación típica; NA si no hay
# dispersión (una serie constante no tiene años extremos).
anomalia_z <- function(x) {
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  round((x - mean(x, na.rm = TRUE)) / s, 2)
}

# Una fila por año del traslape: cada índice con sufijo _a y _b, la
# diferencia B − A de los comparables en nivel (dif_*) y la anomalía
# estandarizada por plataforma de los de conteo (z_*_a, z_*_b).
comparar_anual <- function(temporada_a, temporada_b, anios) {
  if (length(anios) == 0) stop("Sin años de traslape entre las plataformas.",
                               call. = FALSE)
  cols <- c("anio_fuego", INDICES_NIVEL, INDICES_CONTEO)
  a <- temporada_a[temporada_a$anio_fuego %in% anios, cols]
  b <- temporada_b[temporada_b$anio_fuego %in% anios, cols]
  names(a)[-1] <- paste0(names(a)[-1], "_a")
  names(b)[-1] <- paste0(names(b)[-1], "_b")
  tabla <- dplyr::inner_join(a, b, by = "anio_fuego") |>
    dplyr::arrange(anio_fuego)
  for (v in INDICES_NIVEL) {
    tabla[[paste0("dif_", v)]] <- tabla[[paste0(v, "_b")]] - tabla[[paste0(v, "_a")]]
  }
  for (v in INDICES_CONTEO) {
    tabla[[paste0("z_", v, "_a")]] <- anomalia_z(tabla[[paste0(v, "_a")]])
    tabla[[paste0("z_", v, "_b")]] <- anomalia_z(tabla[[paste0(v, "_b")]])
  }
  tabla
}

# Correlación de Spearman entre dos series, NA con menos de tres pares.
rho_spearman <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  if (sum(ok) < 3) return(NA_real_)
  round(stats::cor(x[ok], y[ok], method = "spearman"), 2)
}

# Resumen por índice de la tabla anual: mediana y rango de la diferencia
# (índices en nivel), correlación de Spearman entre las dos series, fracción
# de años con anomalía del mismo signo y años del máximo y del mínimo de
# cada plataforma (índices de conteo).
resumen_comparacion_anual <- function(tabla) {
  fila <- function(v, tipo) {
    xa <- tabla[[paste0(v, "_a")]]
    xb <- tabla[[paste0(v, "_b")]]
    dif <- if (tipo == "nivel") tabla[[paste0("dif_", v)]] else NA_real_
    za <- if (tipo == "conteo") tabla[[paste0("z_", v, "_a")]] else NA_real_
    zb <- if (tipo == "conteo") tabla[[paste0("z_", v, "_b")]] else NA_real_
    signo_ok <- !is.na(za) & !is.na(zb) & za != 0 & zb != 0
    tibble::tibble(
      indice = v, tipo = tipo, n_anios = sum(!is.na(xa) & !is.na(xb)),
      mediana_dif = if (tipo == "nivel") stats::median(dif, na.rm = TRUE) else NA_real_,
      min_dif = if (tipo == "nivel") min(dif, na.rm = TRUE) else NA_real_,
      max_dif = if (tipo == "nivel") max(dif, na.rm = TRUE) else NA_real_,
      rho = rho_spearman(xa, xb),
      mismo_signo = if (tipo == "conteo" && any(signo_ok)) {
        round(mean(sign(za[signo_ok]) == sign(zb[signo_ok])), 2)
      } else NA_real_,
      anio_max_a = tabla$anio_fuego[which.max(xa)],
      anio_max_b = tabla$anio_fuego[which.max(xb)],
      anio_min_a = tabla$anio_fuego[which.min(xa)],
      anio_min_b = tabla$anio_fuego[which.min(xb)]
    )
  }
  dplyr::bind_rows(
    purrr::map(INDICES_NIVEL, fila, tipo = "nivel"),
    purrr::map(INDICES_CONTEO, fila, tipo = "conteo")
  )
}

# --- Producto 3: consolidados por celda en el traslape -------------------------

# Une los consolidados de las dos plataformas (salidas de unir_consolidados()
# sobre los mismos años) celda a celda, conserva las celdas con fuego en
# alguna y clasifica el acuerdo (README, «Producto 3»). Las diferencias solo
# existen donde ambas tienen índices.
comparar_celdas <- function(consolidado_a, consolidado_b) {
  cols <- c("celda_id", "area_km2", "dtot", "ini_dia", "fin_dia", "lon",
            "valida", "sin_estacion", "dens", "anio_inicio", "anio_fin")
  a <- consolidado_a[, cols]
  b <- consolidado_b[, setdiff(cols, c("area_km2", "anio_inicio", "anio_fin"))]
  names(a)[-(1:2)] <- paste0(names(a)[-(1:2)], "_a")
  names(b)[-1] <- paste0(names(b)[-1], "_b")
  dplyr::inner_join(a, b, by = "celda_id") |>
    dplyr::filter(dtot_a > 0 | dtot_b > 0) |>
    dplyr::mutate(
      indices_a = !is.na(lon_a),
      indices_b = !is.na(lon_b),
      acuerdo = dplyr::case_when(
        indices_a & indices_b ~ "ambas",
        (indices_a & sin_estacion_b) | (indices_b & sin_estacion_a) ~ "discordante",
        indices_b ~ "solo_b",
        indices_a ~ "solo_a",
        TRUE ~ "ninguna"
      ),
      acuerdo_codigo = unname(CLASES_ACUERDO[acuerdo]),
      dif_ini = ifelse(acuerdo == "ambas", ini_dia_b - ini_dia_a, NA_integer_),
      dif_fin = ifelse(acuerdo == "ambas", fin_dia_b - fin_dia_a, NA_integer_),
      dif_lon = ifelse(acuerdo == "ambas", lon_b - lon_a, NA_integer_)
    ) |>
    dplyr::select(celda_id, area_km2, anio_inicio_a, anio_fin_a, dtot_a, dtot_b,
                  ini_dia_a, ini_dia_b, fin_dia_a, fin_dia_b, lon_a, lon_b,
                  dens_a, dens_b, valida_a, valida_b, sin_estacion_a,
                  sin_estacion_b, acuerdo, acuerdo_codigo, dif_ini, dif_fin,
                  dif_lon) |>
    dplyr::rename(anio_inicio = anio_inicio_a, anio_fin = anio_fin_a) |>
    dplyr::arrange(celda_id)
}

# Cifras del producto por celda: celdas por clase, mediana y rango
# intercuartílico de las diferencias, fracción de celdas en acuerdo de INI y
# correlación de Spearman entre las densidades de ambas plataformas.
resumen_comparacion_celdas <- function(tabla,
                                       acuerdo_ini = COMPARACION_ACUERDO_INI_DIAS) {
  ambas <- tabla[tabla$acuerdo == "ambas", ]
  cuartiles <- function(x) {
    q <- stats::quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
    list(q1 = q[1], mediana = q[2], q3 = q[3])
  }
  con_dens <- !is.na(tabla$dens_a) & !is.na(tabla$dens_b)
  list(
    n_con_fuego = nrow(tabla),
    n_clases = as.list(table(factor(tabla$acuerdo, levels = names(CLASES_ACUERDO)))),
    n_ambas = nrow(ambas),
    dif_ini = cuartiles(ambas$dif_ini),
    dif_fin = cuartiles(ambas$dif_fin),
    dif_lon = cuartiles(ambas$dif_lon),
    pct_acuerdo_ini = if (nrow(ambas)) {
      round(100 * mean(abs(ambas$dif_ini) <= acuerdo_ini), 0)
    } else NA_real_,
    pct_b_antes = if (nrow(ambas)) round(100 * mean(ambas$dif_ini < 0), 0) else NA_real_,
    rho_dens = rho_spearman(tabla$dens_a[con_dens], tabla$dens_b[con_dens]),
    n_dens = sum(con_dens)
  )
}

# --- Tablas --------------------------------------------------------------------

tabla_comparacion_csv <- function(tabla, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(tabla, dest)
  dest
}

# Rótulo corto de un par para títulos: "MODIS → VIIRS S-NPP".
rotulo_par <- function(etiquetas_a, etiquetas_b) {
  paste0(etiquetas_a$corta, " → ", etiquetas_b$corta)
}

# Widget DT de la tabla anual lado a lado (índices en nivel).
crear_tabla_comparacion_anual <- function(tabla, etiquetas_a, etiquetas_b) {
  ref <- inicio_anio_fuego(2002L)
  fecha_ref <- function(dia) ifelse(is.na(dia), "",
                                    fecha_es(ref + dia - 1L, con_anio = FALSE))
  datos <- tabla |>
    dplyr::transmute(
      anio_fuego,
      ini_a = fecha_ref(ini_dia_a), ini_b = fecha_ref(ini_dia_b),
      dif_ini = dif_ini_dia,
      fin_a = fecha_ref(fin_dia_a), fin_b = fecha_ref(fin_dia_b),
      dif_fin = dif_fin_dia,
      lon_a, lon_b, dif_lon, n50_a, n50_b, c10_a, c10_b
    )
  a <- etiquetas_a$corta; b <- etiquetas_b$corta
  DT::datatable(
    datos,
    colnames = c("Año de fuego", paste("Inicio", a), paste("Inicio", b), "ΔINI (días)",
                 paste("Fin", a), paste("Fin", b), "ΔFIN (días)",
                 paste("LON", a), paste("LON", b), "ΔLON (días)",
                 paste("N50", a), paste("N50", b), paste("C10 %", a), paste("C10 %", b)),
    caption = paste0("Índices anuales lado a lado en el traslape — ", AREA_NOMBRE,
                     ", ", rotulo_par(etiquetas_a, etiquetas_b), " (Δ = B − A)"),
    options = list(pageLength = 30, dom = "t", scrollX = TRUE),
    rownames = FALSE
  )
}

# Widget DT del resumen por índice de un par.
crear_tabla_resumen_comparacion <- function(resumen, etiquetas_a, etiquetas_b) {
  nombres <- c(ini_dia = "INI", fin_dia = "FIN", lon = "LON", n50 = "N50",
               c10 = "C10", dtot = "DTOT", nd95 = "ND95", d95ptot = "D95pTOT")
  datos <- resumen |>
    dplyr::transmute(
      indice = nombres[indice],
      tipo = ifelse(tipo == "nivel", "en nivel", "anomalía estandarizada"),
      n_anios,
      mediana_dif = ifelse(is.na(mediana_dif), "", num_es(mediana_dif, 1)),
      rango_dif = ifelse(is.na(min_dif), "",
                         paste0(num_es(min_dif, 1), " a ", num_es(max_dif, 1))),
      rho = ifelse(is.na(rho), "", num_es(rho, 2)),
      mismo_signo = ifelse(is.na(mismo_signo), "", num_es(100 * mismo_signo, 0)),
      max_a = anio_max_a, max_b = anio_max_b
    )
  DT::datatable(
    datos,
    colnames = c("Índice", "Comparación", "Años", "Mediana de B − A",
                 "Rango de B − A", "ρ de Spearman", "% años con el mismo signo",
                 paste("Año máximo", etiquetas_a$corta),
                 paste("Año máximo", etiquetas_b$corta)),
    caption = paste0("Resumen de la comparación anual — ",
                     rotulo_par(etiquetas_a, etiquetas_b)),
    options = list(pageLength = 10, dom = "t"),
    rownames = FALSE
  )
}

# --- Ráster --------------------------------------------------------------------

# GeoTIFF por par con la clase de acuerdo (códigos de CLASES_ACUERDO) y las
# diferencias B − A de INI, FIN y LON en días. Metadatos sin «=» (terra).
escribir_raster_comparacion <- function(tabla, grilla, dest, etiquetas_a,
                                        etiquetas_b, anios,
                                        res = GRILLA_RES_ANALISIS) {
  b <- sf::st_bbox(grilla)
  plantilla <- terra::rast(
    xmin = b[["xmin"]], xmax = b[["xmax"]], ymin = b[["ymin"]],
    ymax = b[["ymax"]], resolution = res, crs = CRS_WGS84
  )
  capas <- c("acuerdo", "dif_ini", "dif_fin", "dif_lon")
  datos <- grilla |>
    sf::st_drop_geometry() |>
    dplyr::inner_join(tabla, by = "celda_id")
  celda <- terra::cellFromXY(plantilla,
                             cbind(datos$lon_sw + res / 2, datos$lat_sw + res / 2))
  r <- terra::rast(replicate(length(capas), plantilla, simplify = FALSE))
  names(r) <- capas
  r[["acuerdo"]][celda] <- datos$acuerdo_codigo
  for (capa in capas[-1]) r[[capa]][celda] <- datos[[capa]]
  terra::metags(r) <- c(
    par = rotulo_par(etiquetas_a, etiquetas_b),
    plataforma_a = etiquetas_a$corta,
    plataforma_b = etiquetas_b$corta,
    traslape = paste0(min(anios), "-", max(anios)),
    umbral_detecciones = as.character(RASTER_MIN_DETECCIONES),
    codigos_acuerdo = paste0("1 indices en ambas; 2 solo en B; 3 solo en A; ",
                             "4 en ninguna; 5 estacion discordante"),
    definicion = paste0("dif_ini, dif_fin, dif_lon: B menos A en dias, solo ",
                        "en las celdas con indices en ambas; consolidados ",
                        "recalculados sobre los anios del traslape con los ",
                        "mismos umbrales que los rasteres por plataforma")
  )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(r, dest, overwrite = TRUE, datatype = "INT2S",
                     NAflag = -9999)
  dest
}

# --- Figuras --------------------------------------------------------------------

# Tema común de las figuras anuales del módulo.
tema_comparacion <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      plot.title = ggplot2::element_text(face = "bold")
    )
}

# Formato largo de la tabla anual para un conjunto de columnas base: una fila
# por año, índice y plataforma.
comparacion_larga <- function(tabla, columnas, sufijos = c("a", "b"),
                              rotulos, etiquetas_a, etiquetas_b) {
  purrr::map(columnas, function(v) {
    purrr::map(sufijos, function(s) {
      tibble::tibble(anio_fuego = tabla$anio_fuego, indice = rotulos[[v]],
                     plataforma = if (s == "a") etiquetas_a$corta else etiquetas_b$corta,
                     lado = s, valor = tabla[[paste0(v, "_", s)]])
    }) |> purrr::list_rbind()
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      indice = factor(indice, levels = unname(rotulos[columnas])),
      plataforma = factor(plataforma, levels = c(etiquetas_a$corta, etiquetas_b$corta))
    )
}

# Índices comparables en nivel, año a año, con una línea por plataforma.
grafico_comparacion_anual <- function(tabla, dest, etiquetas_a, etiquetas_b, anios) {
  rotulos <- c(ini_dia = "INI: inicio (día del año de fuego; 123 = 1 de enero)",
               lon = "LON: longitud de la temporada (días)",
               n50 = "N50: días que reúnen la mitad de las detecciones",
               c10 = "C10: % de las detecciones en los 10 días más activos")
  datos <- comparacion_larga(tabla, names(rotulos), rotulos = rotulos,
                             etiquetas_a = etiquetas_a, etiquetas_b = etiquetas_b)
  colores <- stats::setNames(unname(colores_par()), c(etiquetas_a$corta, etiquetas_b$corta))
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = anio_fuego, y = valor,
                                           color = plataforma, group = plataforma)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~indice, ncol = 1, scales = "free_y") +
    ggplot2::scale_color_manual(values = colores, name = NULL) +
    ggplot2::scale_x_continuous(breaks = tabla$anio_fuego) +
    ggplot2::labs(
      title = paste0("Índices de temporada lado a lado: ",
                     rotulo_par(etiquetas_a, etiquetas_b)),
      subtitle = paste0("Años de fuego del traslape ", min(anios), "–", max(anios),
                        "; índices por fracciones, comparables en nivel — ",
                        AREA_NOMBRE),
      x = "Año de fuego", y = NULL,
      caption = "Datos: NASA FIRMS (MODIS y VIIRS)"
    ) +
    tema_comparacion()
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 10, height = 9, dpi = 200)
  dest
}

# Anomalías estandarizadas de los índices de conteo, barras por año y
# plataforma.
grafico_anomalias <- function(tabla, dest, etiquetas_a, etiquetas_b, anios) {
  rotulos <- c(dtot = "DTOT: detecciones del año",
               nd95 = "ND95: días extremos",
               d95ptot = "D95pTOT: % de las detecciones en días extremos")
  z <- tabla
  for (v in names(rotulos)) {
    z[[paste0(v, "_a")]] <- tabla[[paste0("z_", v, "_a")]]
    z[[paste0(v, "_b")]] <- tabla[[paste0("z_", v, "_b")]]
  }
  datos <- comparacion_larga(z, names(rotulos), rotulos = rotulos,
                             etiquetas_a = etiquetas_a, etiquetas_b = etiquetas_b)
  colores <- stats::setNames(unname(colores_par()), c(etiquetas_a$corta, etiquetas_b$corta))
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = anio_fuego, y = valor, fill = plataforma)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.7) +
    ggplot2::facet_wrap(~indice, ncol = 1) +
    ggplot2::scale_fill_manual(values = colores, name = NULL) +
    ggplot2::scale_x_continuous(breaks = tabla$anio_fuego) +
    ggplot2::labs(
      title = paste0("Anomalías estandarizadas de los índices de conteo: ",
                     rotulo_par(etiquetas_a, etiquetas_b)),
      subtitle = paste0("(valor − media) / desviación típica del traslape ",
                        min(anios), "–", max(anios), ", por plataforma — ",
                        AREA_NOMBRE),
      x = "Año de fuego", y = "Anomalía estandarizada (desviaciones típicas)",
      caption = "Datos: NASA FIRMS (MODIS y VIIRS)"
    ) +
    tema_comparacion()
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 10, height = 8, dpi = 200)
  dest
}

# Celdas de la tabla de comparación con geometría, en CRTM05, para dibujar.
celdas_comparacion_sf <- function(tabla, grilla) {
  datos <- dplyr::inner_join(sf::st_drop_geometry(grilla), tabla, by = "celda_id")
  geometria <- sf::st_geometry(grilla)[match(datos$celda_id, grilla$celda_id)]
  a_crtm05(sf::st_sf(datos, geometry = geometria))
}

# Rótulos de las clases de acuerdo con los nombres de las plataformas.
rotulos_acuerdo <- function(etiquetas_a, etiquetas_b) {
  c(ambas = "Índices en ambas",
    solo_b = paste0("Solo en ", etiquetas_b$corta),
    solo_a = paste0("Solo en ", etiquetas_a$corta),
    ninguna = "En ninguna (bajo el umbral en las dos)",
    discordante = "Estación discordante (sin estación definida en una sola)")
}

# Mapa categórico del acuerdo entre plataformas por celda.
mapa_acuerdo <- function(tabla, grilla, area, dest, etiquetas_a, etiquetas_b, anios) {
  rot <- rotulos_acuerdo(etiquetas_a, etiquetas_b)
  celdas <- celdas_comparacion_sf(tabla, grilla) |>
    dplyr::mutate(clase = factor(rot[acuerdo], levels = unname(rot)))
  colores <- stats::setNames(c("#1a9850", colores_par()[["b"]], colores_par()[["a"]],
                               "grey80", "#984ea3"), unname(rot))
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = celdas, ggplot2::aes(fill = clase),
                     color = "white", linewidth = 0.15) +
    ggplot2::geom_sf(data = area, fill = NA, color = "grey30", linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = colores, name = NULL, drop = FALSE) +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::labs(
      title = paste0("Acuerdo entre plataformas por celda: ",
                     rotulo_par(etiquetas_a, etiquetas_b)),
      subtitle = paste0("Consolidados de ambas sobre los años de fuego del traslape ",
                        min(anios), "–", max(anios), "; umbral de ",
                        RASTER_MIN_DETECCIONES, " detecciones por celda\n",
                        AREA_NOMBRE),
      caption = "Datos: NASA FIRMS (MODIS y VIIRS)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "grey92", linewidth = 0.3),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 9, height = 8, dpi = 200)
  dest
}

# Mapa divergente de una diferencia por celda (dif_ini, dif_fin o dif_lon):
# rojo, B antes o más corta; azul, B después o más larga. Escala simétrica
# acotada en `tope` días; celdas sin diferencia (no están en ambas) en gris.
mapa_diferencia <- function(tabla, grilla, area, dest, variable, etiquetas_a,
                            etiquetas_b, anios, tope = 60L) {
  celdas <- celdas_comparacion_sf(tabla, grilla)
  titulo <- c(dif_ini = "Diferencia de inicio de la temporada por celda",
              dif_fin = "Diferencia de fin de la temporada por celda",
              dif_lon = "Diferencia de longitud de la temporada por celda")[[variable]]
  rotulo <- c(dif_ini = "ΔINI (días)", dif_fin = "ΔFIN (días)",
              dif_lon = "ΔLON (días)")[[variable]]
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = celdas, ggplot2::aes(fill = .data[[variable]]),
                     color = "white", linewidth = 0.15) +
    ggplot2::geom_sf(data = area, fill = NA, color = "grey30", linewidth = 0.4) +
    ggplot2::scale_fill_distiller(palette = "RdBu", direction = 1,
                                  limits = c(-tope, tope), oob = scales::oob_squish,
                                  na.value = "grey85", name = rotulo,
                                  labels = function(x) ifelse(abs(x) >= tope,
                                                              paste0(ifelse(x > 0, "≥ ", "≤ "), x), x)) +
    ggplot2::labs(
      title = paste0(titulo, ": ", rotulo_par(etiquetas_a, etiquetas_b)),
      subtitle = paste0(etiquetas_b$corta, " menos ", etiquetas_a$corta,
                        " en las celdas con índices en ambas (las demás en gris); ",
                        "traslape ", min(anios), "–", max(anios), "\n",
                        "Negativo: ", etiquetas_b$corta,
                        if (variable == "dif_lon") " más corta" else " antes",
                        " — ", AREA_NOMBRE),
      caption = "Datos: NASA FIRMS (MODIS y VIIRS)"
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

# --- Cifras para la prosa ------------------------------------------------------

ayudantes_comparacion <- function(tabla_anual, resumen_anual, resumen_celdas,
                                  anios) {
  fila <- function(v) resumen_anual[resumen_anual$indice == v, ]
  d <- function(v, campo, dec = 0) num_es(fila(v)[[campo]], dec)
  list(
    traslape = paste0(min(anios), "–", max(anios)),
    n_anios = length(anios),
    ini_mediana_dif = d("ini_dia", "mediana_dif"),
    ini_min_dif = d("ini_dia", "min_dif"), ini_max_dif = d("ini_dia", "max_dif"),
    ini_rho = d("ini_dia", "rho", 2),
    fin_mediana_dif = d("fin_dia", "mediana_dif"), fin_rho = d("fin_dia", "rho", 2),
    lon_mediana_dif = d("lon", "mediana_dif"),
    lon_min_dif = d("lon", "min_dif"), lon_max_dif = d("lon", "max_dif"),
    lon_rho = d("lon", "rho", 2),
    n50_mediana_dif = d("n50", "mediana_dif"), n50_rho = d("n50", "rho", 2),
    c10_mediana_dif = d("c10", "mediana_dif", 1), c10_rho = d("c10", "rho", 2),
    dtot_rho = d("dtot", "rho", 2),
    dtot_mismo_signo = num_es(100 * fila("dtot")$mismo_signo, 0),
    dtot_max_a = fila("dtot")$anio_max_a, dtot_max_b = fila("dtot")$anio_max_b,
    dtot_min_a = fila("dtot")$anio_min_a, dtot_min_b = fila("dtot")$anio_min_b,
    nd95_rho = d("nd95", "rho", 2),
    nd95_mismo_signo = num_es(100 * fila("nd95")$mismo_signo, 0),
    nd95_max_a = fila("nd95")$anio_max_a, nd95_max_b = fila("nd95")$anio_max_b,
    d95ptot_rho = d("d95ptot", "rho", 2),
    n_con_fuego = resumen_celdas$n_con_fuego,
    n_ambas = resumen_celdas$n_ambas,
    n_solo_b = resumen_celdas$n_clases$solo_b,
    n_solo_a = resumen_celdas$n_clases$solo_a,
    n_ninguna = resumen_celdas$n_clases$ninguna,
    n_discordante = resumen_celdas$n_clases$discordante,
    dif_ini_mediana = num_es(resumen_celdas$dif_ini$mediana, 0),
    dif_ini_q1 = num_es(resumen_celdas$dif_ini$q1, 0),
    dif_ini_q3 = num_es(resumen_celdas$dif_ini$q3, 0),
    dif_fin_mediana = num_es(resumen_celdas$dif_fin$mediana, 0),
    dif_lon_mediana = num_es(resumen_celdas$dif_lon$mediana, 0),
    dif_lon_q1 = num_es(resumen_celdas$dif_lon$q1, 0),
    dif_lon_q3 = num_es(resumen_celdas$dif_lon$q3, 0),
    pct_acuerdo_ini = resumen_celdas$pct_acuerdo_ini,
    pct_b_antes = resumen_celdas$pct_b_antes,
    rho_dens = num_es(resumen_celdas$rho_dens, 2),
    n_dens = resumen_celdas$n_dens
  )
}
