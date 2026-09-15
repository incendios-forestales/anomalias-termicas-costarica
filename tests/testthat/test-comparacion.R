# Pruebas del módulo R/comparacion.R con datos sintéticos.

# Tabla anual mínima de una plataforma, con las columnas que usa comparar_anual.
temporada_sintetica <- function(anios, ini, lon, dtot, parcial = FALSE,
                                provisional = FALSE) {
  tibble::tibble(
    anio_fuego = anios, dtot = dtot, ini_dia = ini, fin_dia = ini + lon - 1L,
    lon = lon, n50 = pmax(1L, dtot %/% 100L), c10 = round(3000 / dtot, 1),
    nd95 = dtot %/% 500L, d95ptot = round(100 * (dtot %/% 500L) / 10, 1),
    parcial = parcial, provisional = provisional
  )
}

test_that("pares_comparacion ordena los pares según PLATAFORMAS", {
  p <- pares_comparacion()
  expect_equal(p$par, c("modis_snpp", "modis_noaa20", "snpp_noaa20"))
  expect_equal(p$a, c("modis", "modis", "snpp"))
})

test_that("anios_traslape es la intersección de los periodos de referencia", {
  a <- temporada_sintetica(2010:2015, 150L, 80L, 1000L,
                           provisional = c(rep(FALSE, 5), TRUE))
  b <- temporada_sintetica(2013:2016, 150L, 80L, 5000L,
                           parcial = c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(anios_traslape(a, b), 2014:2014)
  b$parcial[1] <- FALSE
  expect_equal(anios_traslape(a, b), 2013:2014)
})

test_that("anomalia_z estandariza y devuelve NA sin dispersión", {
  z <- anomalia_z(c(1, 2, 3))
  expect_equal(mean(z), 0)
  expect_equal(z[3], 1)
  expect_true(all(is.na(anomalia_z(c(5, 5, 5)))))
})

test_that("comparar_anual calcula B − A y anomalías por plataforma", {
  anios <- 2013:2017
  a <- temporada_sintetica(anios, ini = c(150L, 160L, 155L, 170L, 165L),
                           lon = c(80L, 70L, 90L, 60L, 75L),
                           dtot = c(1000L, 800L, 1200L, 600L, 900L))
  # B empieza siempre 10 días antes y dura 5 días más; sus conteos son cinco
  # veces mayores pero ordenan igual los años.
  b <- temporada_sintetica(anios, ini = a$ini_dia - 10L, lon = a$lon + 5L,
                           dtot = a$dtot * 5L)
  tabla <- comparar_anual(a, b, anios)
  expect_equal(nrow(tabla), 5L)
  expect_true(all(tabla$dif_ini_dia == -10L))
  expect_true(all(tabla$dif_lon == 5L))
  expect_equal(tabla$z_dtot_a, tabla$z_dtot_b)   # misma anomalía, otra escala
  r <- resumen_comparacion_anual(tabla)
  expect_equal(r$mediana_dif[r$indice == "ini_dia"], -10)
  expect_equal(r$rho[r$indice == "lon"], 1)
  expect_equal(r$rho[r$indice == "dtot"], 1)
  expect_equal(r$mismo_signo[r$indice == "dtot"], 1)
  expect_equal(r$anio_max_a[r$indice == "dtot"], 2015L)
  expect_equal(r$anio_max_b[r$indice == "dtot"], 2015L)
  expect_true(is.na(r$mediana_dif[r$indice == "dtot"]))
  expect_error(comparar_anual(a, b, integer()), "traslape")
})

# Consolidado mínimo por celda con las columnas de unir_consolidados().
consolidado_sintetico <- function(celdas, dtot, ini, lon, valida, sin_estacion,
                                  dens) {
  tibble::tibble(
    celda_id = celdas, area_km2 = 100, dtot = dtot,
    ini_dia = ifelse(valida & !sin_estacion, ini, NA_integer_),
    fin_dia = ifelse(valida & !sin_estacion, ini + lon - 1L, NA_integer_),
    lon = ifelse(valida & !sin_estacion, lon, NA_integer_),
    valida = valida, sin_estacion = sin_estacion, dens = dens,
    anio_inicio = 2013L, anio_fin = 2025L
  )
}

test_that("comparar_celdas clasifica el acuerdo y resta B − A", {
  celdas <- paste0("c", 1:6)
  a <- consolidado_sintetico(celdas,
                             dtot = c(50L, 10L, 40L, 5L, 60L, 0L),
                             ini = 150L, lon = 80L,
                             valida = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
                             sin_estacion = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                             dens = c(0.5, 0.1, 0.4, 0.05, 0.6, 0))
  b <- consolidado_sintetico(celdas,
                             dtot = c(200L, 80L, 10L, 8L, 300L, 0L),
                             ini = 140L, lon = 90L,
                             valida = c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
                             sin_estacion = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
                             dens = c(2.0, 0.4, 1.6, 0.2, 2.4, 0))   # mismo orden que A
  t <- comparar_celdas(a, b)
  # La celda 6, sin fuego en ninguna, no aparece.
  expect_equal(t$celda_id, paste0("c", 1:5))
  expect_equal(t$acuerdo, c("ambas", "solo_b", "solo_a", "ninguna", "discordante"))
  expect_equal(t$acuerdo_codigo, c(1L, 2L, 3L, 4L, 5L))
  expect_equal(t$dif_ini[1], -10L)
  expect_equal(t$dif_lon[1], 10L)
  expect_true(all(is.na(t$dif_ini[-1])))
  r <- resumen_comparacion_celdas(t)
  expect_equal(r$n_con_fuego, 5L)
  expect_equal(r$n_ambas, 1L)
  expect_equal(r$n_clases$solo_b, 1L)
  expect_equal(r$dif_ini$mediana, -10)
  expect_equal(r$pct_acuerdo_ini, 100)
  expect_equal(r$pct_b_antes, 100)
  expect_equal(r$rho_dens, 1)           # mismo orden de densidades
  expect_equal(r$n_dens, 5L)
})
