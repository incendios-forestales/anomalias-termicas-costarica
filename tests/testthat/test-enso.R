# Pruebas del módulo R/enso.R con una serie ONI sintética.

# Serie ONI de 2019 a 2021 (36 trimestres) con un episodio Niño de seis
# trimestres (JJA 2019 a NDJ 2019), una racha positiva corta de tres separada
# de la anterior (FMA a AMJ 2020) y un episodio Niña de cinco trimestres
# (ASO 2020 a DJF 2021).
oni_prueba <- local({
  trimestres <- names(ONI_MES_CENTRAL)
  anio <- rep(2019:2021, each = 12)
  oni <- rep(0, 36)
  oni[7:12] <- c(0.5, 0.8, 1.0, 1.2, 0.9, 0.6)      # JJA-NDJ 2019: Niño
  oni[15:17] <- c(0.7, 0.6, 0.5)                    # FMA-AMJ 2020: corta, no cuenta
  oni[21:25] <- c(-0.6, -0.9, -1.1, -0.8, -0.5)     # ASO 2020-DJF 2021: Niña
  tibble::tibble(trimestre = rep(trimestres, 3), anio = anio, total = 26 + oni,
                 oni = oni, mes_central = rep(unname(ONI_MES_CENTRAL), 3))
})

test_that("fases_oni aplica la regla de cinco trimestres del CPC", {
  f <- fases_oni(oni_prueba)
  expect_equal(as.character(f$fase[7:12]), rep("El Niño", 6))
  expect_equal(as.character(f$fase[15:17]), rep("neutra", 3))   # tres no bastan
  expect_equal(as.character(f$fase[21:25]), rep("La Niña", 5))
  expect_equal(as.character(f$fase[1]), "neutra")
  expect_equal(levels(f$fase), FASES_ENSO)
})

test_that("oni_por_anio_fuego toma el DJF que cae dentro del año de fuego", {
  af <- oni_por_anio_fuego(fases_oni(oni_prueba))
  expect_equal(af$anio_fuego, 2019:2021)
  expect_equal(af$oni_djf, c(0, 0, -0.5))
  expect_equal(as.character(af$fase), c("neutra", "neutra", "La Niña"))
})

test_that("leer_oni lee el formato del CPC", {
  archivo <- tempfile(fileext = ".txt")
  writeLines(c(" SEAS  YR   TOTAL   ANOM", "  DJF 1950  25.01  -1.32",
               "  JFM 1950  25.36  -1.20"), archivo)
  o <- leer_oni(archivo)
  expect_equal(names(o), c("trimestre", "anio", "total", "oni", "mes_central"))
  expect_equal(o$oni, c(-1.32, -1.20))
  expect_equal(o$mes_central, c(1L, 2L))
})

test_that("unir_enso y resumen_enso describen la asociación por fase", {
  af <- tibble::tibble(anio_fuego = 2010:2019,
                       oni_djf = c(1.5, -1.2, 0.1, 2.0, -0.8, 0.3, 1.1, -1.5, 0.0, 0.9),
                       fase = factor(c("El Niño", "La Niña", "neutra", "El Niño", "La Niña",
                                       "neutra", "El Niño", "La Niña", "neutra", "El Niño"),
                                     levels = FASES_ENSO))
  # LON crece con el ONI; DTOT también; los demás índices constantes.
  temporada <- tibble::tibble(
    anio_fuego = 2010:2019, dtot = as.integer(1000 + 200 * af$oni_djf),
    ini_dia = 150L, fin_dia = 230L, lon = as.integer(80 + 10 * af$oni_djf),
    n50 = 20L, c10 = 30, frpi = 15, nd95 = 3L, d95ptot = 20,
    parcial = FALSE, provisional = c(rep(FALSE, 9), TRUE), no_comparable = FALSE
  )
  t <- unir_enso(temporada, af, anios_base = 2010:2015)
  expect_equal(nrow(t), 9L)                       # el provisional queda fuera
  expect_true(all(c("oni_djf", "fase", "z_dtot", "z_nd95") %in% names(t)))
  expect_true(all(is.na(t$z_nd95)))               # nd95 constante en la base
  r <- resumen_enso(t)
  expect_equal(r$rho[r$indice == "lon"], 1)
  expect_equal(r$rho[r$indice == "dtot"], 1)
  expect_lt(r$p[r$indice == "lon"], 0.01)
  expect_equal(r$n_nino[r$indice == "lon"], 3L)   # 2019 provisional no cuenta
  expect_true(r$mediana_nino[r$indice == "lon"] > r$mediana_nina[r$indice == "lon"])
  expect_true(is.na(r$rho[r$indice == "nd95"]))
})
