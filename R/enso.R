# Fase ENSO (ONI del CPC de la NOAA) contra los índices anuales de temporada.
#
# Implementa la subsección «Fase ENSO y los índices anuales» del README, que
# es el contrato: una sola ventana fijada a priori (el trimestre DJF que cae
# dentro del año de fuego) y la fase por la regla oficial del CPC (cinco
# trimestres móviles consecutivos con |ONI| ≥ 0,5). El producto describe la
# asociación por plataforma; no ajusta modelos ni predice, y la fase no entra
# en ningún índice de la suite.

ONI_URL          <- "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"
ONI_UMBRAL       <- 0.5    # °C, umbral del CPC
ONI_TRIMESTRES   <- 5L     # trimestres consecutivos para declarar episodio
ONI_DIAS_VIGENCIA <- 30L   # el CPC actualiza el ONI una vez al mes
ONI_TRIMESTRE_VENTANA <- "DJF"

# Trimestres móviles del CPC y el mes calendario central de cada uno.
ONI_MES_CENTRAL <- c(DJF = 1L, JFM = 2L, FMA = 3L, MAM = 4L, AMJ = 5L, MJJ = 6L,
                     JJA = 7L, JAS = 8L, ASO = 9L, SON = 10L, OND = 11L, NDJ = 12L)

FASES_ENSO <- c("El Niño", "neutra", "La Niña")

# --- Descarga -----------------------------------------------------------------------

# Descarga oni.ascii.txt a data/raw/oni/ si no existe o si tiene más de
# ONI_DIAS_VIGENCIA días; deja la fecha de descarga junto al archivo. Es la
# única serie no satelital del proyecto. Devuelve la ruta del archivo.
descargar_oni <- function(dir = "data/raw/oni", url = ONI_URL,
                          vigencia = ONI_DIAS_VIGENCIA) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  destino <- file.path(dir, "oni.ascii.txt")
  fecha <- file.path(dir, "oni_descarga.txt")
  vigente <- file.exists(destino) && file.exists(fecha) &&
    as.integer(Sys.Date() - as.Date(readLines(fecha, n = 1))) <= vigencia
  if (!vigente) {
    temporal <- tempfile(fileext = ".txt")
    httr2::request(url) |> httr2::req_perform(path = temporal)
    lineas <- readLines(temporal)
    if (length(lineas) < 100 || !grepl("^\\s*SEAS\\s+YR", lineas[1])) {
      stop("El archivo ONI descargado no tiene el formato esperado.", call. = FALSE)
    }
    file.copy(temporal, destino, overwrite = TRUE)
    writeLines(as.character(Sys.Date()), fecha)
  }
  destino
}

# --- Lectura y fases ------------------------------------------------------------------

# Serie ONI: una fila por trimestre móvil con su año, el mes central, la
# temperatura total y la anomalía (el ONI propiamente dicho).
leer_oni <- function(archivo) {
  utils::read.table(archivo, header = TRUE, col.names = c("trimestre", "anio", "total", "oni")) |>
    tibble::as_tibble() |>
    dplyr::mutate(anio = as.integer(anio),
                  mes_central = unname(ONI_MES_CENTRAL[trimestre])) |>
    dplyr::arrange(anio, mes_central)
}

# Fase del CPC por trimestre: un trimestre es de El Niño (La Niña) si
# pertenece a una racha de al menos ONI_TRIMESTRES consecutivos con ONI por
# encima de +0,5 (por debajo de −0,5); si no, neutra.
fases_oni <- function(oni, umbral = ONI_UMBRAL, minimo = ONI_TRIMESTRES) {
  en_racha <- function(condicion) {
    r <- rle(condicion)
    rep(r$values & r$lengths >= minimo, r$lengths)
  }
  oni |>
    dplyr::arrange(anio, mes_central) |>
    dplyr::mutate(
      fase = dplyr::case_when(
        en_racha(oni >= umbral) ~ "El Niño",
        en_racha(oni <= -umbral) ~ "La Niña",
        TRUE ~ "neutra"
      ),
      fase = factor(fase, levels = FASES_ENSO)
    )
}

# ONI y fase del trimestre ventana por año de fuego: DJF del año Y (dic Y−1 a
# feb Y) cae dentro del año de fuego Y.
oni_por_anio_fuego <- function(oni_con_fase, trimestre = ONI_TRIMESTRE_VENTANA) {
  oni_con_fase |>
    dplyr::filter(.data$trimestre == .env$trimestre) |>
    dplyr::transmute(anio_fuego = anio, oni_djf = oni, fase)
}

# --- Tabla por año y resumen --------------------------------------------------------

# Anomalía estandarizada respecto de la media y la desviación típica de los
# años del periodo base (no de toda la serie).
anomalia_base <- function(x, anios, anios_base) {
  base <- x[anios %in% anios_base]
  base <- base[!is.na(base)]
  if (length(base) < 2 || stats::sd(base) == 0) return(rep(NA_real_, length(x)))
  round((x - mean(base)) / stats::sd(base), 2)
}

INDICES_ENSO <- c("dtot", "ini_dia", "fin_dia", "lon", "n50", "c10", "frpi",
                  "nd95", "d95ptot")

# Años completos y no provisionales de la plataforma con su ONI, su fase y
# los índices anuales; los de conteo también como anomalía sobre el periodo
# base (z_dtot, z_nd95, z_d95ptot).
unir_enso <- function(temporada, oni_anio_fuego, anios_base) {
  ok <- temporada[!temporada$parcial & !temporada$provisional, ]
  tabla <- ok |>
    dplyr::select(anio_fuego, dplyr::all_of(INDICES_ENSO), no_comparable) |>
    dplyr::inner_join(oni_anio_fuego, by = "anio_fuego") |>
    dplyr::relocate(oni_djf, fase, .after = anio_fuego) |>
    dplyr::arrange(anio_fuego)
  for (v in c("dtot", "nd95", "d95ptot")) {
    x <- tabla[[v]]
    # Los conteos de los años no comparables (en MODIS, 2002 con solo Terra)
    # dependen del instrumental, no del fuego: sin anomalía.
    x[tabla$no_comparable] <- NA
    tabla[[paste0("z_", v)]] <- anomalia_base(x, tabla$anio_fuego, anios_base)
  }
  tabla
}

# Una fila por índice: años y mediana por fase, y ρ de Spearman con su
# valor p entre ONI_DJF y el índice (los de conteo, como anomalía).
resumen_enso <- function(tabla) {
  variables <- c(lon = "lon", ini_dia = "ini_dia", fin_dia = "fin_dia",
                 n50 = "n50", c10 = "c10", frpi = "frpi",
                 dtot = "z_dtot", nd95 = "z_nd95", d95ptot = "z_d95ptot")
  purrr::imap(variables, function(col, nombre) {
    x <- tabla[[col]]
    ok <- !is.na(x) & !is.na(tabla$oni_djf)
    por_fase <- purrr::map(FASES_ENSO, function(f) {
      sel <- ok & tabla$fase == f
      c(n = sum(sel), mediana = if (any(sel)) stats::median(x[sel]) else NA_real_)
    })
    test <- if (sum(ok) >= 5) {
      suppressWarnings(stats::cor.test(tabla$oni_djf[ok], x[ok], method = "spearman",
                                       exact = FALSE))
    } else NULL
    tibble::tibble(
      indice = nombre, columna = col, n = sum(ok),
      n_nino = por_fase[[1]][["n"]], mediana_nino = por_fase[[1]][["mediana"]],
      n_neutra = por_fase[[2]][["n"]], mediana_neutra = por_fase[[2]][["mediana"]],
      n_nina = por_fase[[3]][["n"]], mediana_nina = por_fase[[3]][["mediana"]],
      rho = if (is.null(test)) NA_real_ else round(unname(test$estimate), 2),
      p = if (is.null(test)) NA_real_ else round(test$p.value, 3)
    )
  }) |>
    purrr::list_rbind()
}

# --- Tablas ----------------------------------------------------------------------------

# Widget DT del resumen por fase.
crear_tabla_resumen_enso <- function(resumen, etiqueta_fuente) {
  nombres <- c(dtot = "DTOT (anomalía)", ini_dia = "INI (día)", fin_dia = "FIN (día)",
               lon = "LON (días)", n50 = "N50 (días)", c10 = "C10 (%)",
               frpi = "FRPI (MW)", nd95 = "ND95 (anomalía)", d95ptot = "D95pTOT (anomalía)")
  f <- function(x) ifelse(is.na(x), "", num_es(x, 1))
  datos <- resumen |>
    dplyr::transmute(
      indice = nombres[indice],
      nino = paste0(f(mediana_nino), " (", n_nino, ")"),
      neutra = paste0(f(mediana_neutra), " (", n_neutra, ")"),
      nina = paste0(f(mediana_nina), " (", n_nina, ")"),
      rho = ifelse(is.na(rho), "", num_es(rho, 2)),
      p = ifelse(is.na(p), "", num_es(p, 3))
    )
  DT::datatable(
    datos,
    colnames = c("Índice", "El Niño: mediana (años)", "Neutra: mediana (años)",
                 "La Niña: mediana (años)", "ρ de Spearman con ONI_DJF", "valor p"),
    caption = paste0("Índices anuales según la fase ENSO del trimestre DJF — ",
                     AREA_NOMBRE, ", ", etiqueta_fuente),
    options = list(pageLength = 10, dom = "t"),
    rownames = FALSE
  )
}

# --- Figura -----------------------------------------------------------------------------

# Dispersión de ONI_DJF contra LON, INI y la anomalía de DTOT, un panel por
# índice, cada año rotulado y coloreado por fase.
grafico_enso <- function(tabla, dest, etiqueta_fuente, fuente) {
  rotulos <- c(lon = "LON: longitud de la temporada (días)",
               ini_dia = "INI: inicio (día del año de fuego; 123 = 1 de enero)",
               z_dtot = "DTOT: anomalía estandarizada sobre el periodo base")
  datos <- purrr::imap(rotulos, function(r, v) {
    tibble::tibble(anio_fuego = tabla$anio_fuego, oni_djf = tabla$oni_djf,
                   fase = tabla$fase, indice = r, valor = tabla[[v]])
  }) |>
    purrr::list_rbind() |>
    dplyr::filter(!is.na(valor)) |>
    dplyr::mutate(indice = factor(indice, levels = unname(rotulos)))
  colores <- stats::setNames(c("#d7301f", "grey55", "#2171b5"), FASES_ENSO)
  p <- ggplot2::ggplot(datos, ggplot2::aes(x = oni_djf, y = valor)) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = -ONI_UMBRAL, ymin = -Inf, ymax = Inf,
                      fill = "#2171b5", alpha = 0.06) +
    ggplot2::annotate("rect", xmin = ONI_UMBRAL, xmax = Inf, ymin = -Inf, ymax = Inf,
                      fill = "#d7301f", alpha = 0.06) +
    ggplot2::geom_vline(xintercept = 0, color = "grey70", linewidth = 0.3) +
    ggplot2::geom_point(ggplot2::aes(color = fase), size = 2.6) +
    ggplot2::geom_text(ggplot2::aes(label = anio_fuego, color = fase), size = 2.6,
                       vjust = -0.9, show.legend = FALSE) +
    ggplot2::facet_wrap(~indice, ncol = 1, scales = "free_y") +
    ggplot2::scale_color_manual(values = colores, name = "Fase del CPC en DJF", drop = FALSE) +
    ggplot2::labs(
      title = "Índices anuales de temporada según el ENSO",
      subtitle = paste0("ONI del trimestre diciembre–febrero que precede a la temporada; ",
                        "años completos y no provisionales\nBandas: umbral de ±",
                        num_es(ONI_UMBRAL, 1), " °C del CPC — ", AREA_NOMBRE, ", ",
                        etiqueta_fuente),
      x = "ONI del trimestre DJF (°C)", y = NULL,
      caption = paste0(fuente, "; ONI: NOAA CPC")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      plot.title = ggplot2::element_text(face = "bold")
    )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(dest, p, width = 9, height = 10, dpi = 200)
  dest
}

# --- Cifras para la prosa ---------------------------------------------------------------

NOMBRES_INDICES_ENSO <- c(dtot = "DTOT", ini_dia = "INI", fin_dia = "FIN", lon = "LON",
                          n50 = "N50", c10 = "C10", frpi = "FRPI", nd95 = "ND95",
                          d95ptot = "D95pTOT")

# Valor p en prosa: dos decimales, o "< 0,01" por debajo.
p_es <- function(p) ifelse(is.na(p), "", ifelse(p < 0.01, "< 0,01", paste0("= ", num_es(p, 2))))

ayudantes_enso <- function(tabla, resumen) {
  fila <- function(v) resumen[resumen$indice == v, ]
  anios_fase <- function(f) paste(tabla$anio_fuego[tabla$fase == f], collapse = ", ")
  lon <- fila("lon"); ini <- fila("ini_dia"); dtot <- fila("dtot"); nd95 <- fila("nd95")
  ref <- inicio_anio_fuego(2002L)
  fecha_ref <- function(d) if (is.na(d)) "" else fecha_es(ref + round(d) - 1L, con_anio = FALSE)
  list(
    n_anios = nrow(tabla),
    n_nino = sum(tabla$fase == "El Niño"), anios_nino = anios_fase("El Niño"),
    n_nina = sum(tabla$fase == "La Niña"), anios_nina = anios_fase("La Niña"),
    n_neutra = sum(tabla$fase == "neutra"),
    oni_max = num_es(max(tabla$oni_djf), 1), anio_oni_max = tabla$anio_fuego[which.max(tabla$oni_djf)],
    oni_min = num_es(min(tabla$oni_djf), 1), anio_oni_min = tabla$anio_fuego[which.min(tabla$oni_djf)],
    lon_nino = num_es(lon$mediana_nino, 0), lon_neutra = num_es(lon$mediana_neutra, 0),
    lon_nina = num_es(lon$mediana_nina, 0), lon_rho = num_es(lon$rho, 2), lon_p = p_es(lon$p),
    ini_nino = fecha_ref(ini$mediana_nino), ini_neutra = fecha_ref(ini$mediana_neutra),
    ini_nina = fecha_ref(ini$mediana_nina), ini_rho = num_es(ini$rho, 2), ini_p = p_es(ini$p),
    dtot_nino = num_es(dtot$mediana_nino, 1), dtot_neutra = num_es(dtot$mediana_neutra, 1),
    dtot_nina = num_es(dtot$mediana_nina, 1), dtot_rho = num_es(dtot$rho, 2), dtot_p = p_es(dtot$p),
    nd95_rho = num_es(nd95$rho, 2), nd95_p = p_es(nd95$p),
    indice_rho_max = NOMBRES_INDICES_ENSO[[resumen$indice[which.max(abs(resumen$rho))]]],
    rho_max = num_es(resumen$rho[which.max(abs(resumen$rho))], 2),
    n_p05 = sum(resumen$p < 0.05, na.rm = TRUE),
    indices_p05 = paste(NOMBRES_INDICES_ENSO[resumen$indice[!is.na(resumen$p) & resumen$p < 0.05]], collapse = ", ")
  )
}
