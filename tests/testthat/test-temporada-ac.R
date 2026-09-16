# Pruebas del módulo R/temporada_ac.R con AC sintéticas: dos cuadrados de
# ~0,1° (ACX al oeste, ACY al este) y un país que solo cubre la mitad de ACY.

ac_prueba <- sf::st_sf(
  siglas_ac = c("ACX", "ACY"),
  nombre_ac = c("Área X", "Área Y"),
  geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(-85.3, 9.8), c(-85.2, 9.8), c(-85.2, 9.9),
                              c(-85.3, 9.9), c(-85.3, 9.8)))),
    sf::st_polygon(list(rbind(c(-85.2, 9.8), c(-85.1, 9.8), c(-85.1, 9.9),
                              c(-85.2, 9.9), c(-85.2, 9.8)))),
    crs = 4326)
)
pais_prueba <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(rbind(c(-85.3, 9.8), c(-85.15, 9.8), c(-85.15, 9.9),
                            c(-85.3, 9.9), c(-85.3, 9.8)))), crs = 4326))

rangos_ac <- data.frame(data_id = "MODIS_SP", nivel = "SP",
                        inicio = as.Date("2019-09-01"), fin = as.Date("2022-08-31"))

# Detecciones: ACX con 120 por año de fuego (una por día del 1 de enero al
# 30 de abril) en 2020, 2021 y 2022; ACY con 40 por año; una fuera de ambas.
puntos_ac <- local({
  fechas_x <- c(as.Date("2020-01-01") + 0:119, as.Date("2021-01-01") + 0:119,
                as.Date("2022-01-01") + 0:119)
  fechas_y <- c(as.Date("2020-02-01") + 0:39, as.Date("2021-02-01") + 0:39,
                as.Date("2022-02-01") + 0:39)
  n <- length(fechas_x) + length(fechas_y) + 1
  sf::st_as_sf(
    data.frame(id_deteccion = seq_len(n),
               acq_date = c(fechas_x, fechas_y, as.Date("2020-03-01")),
               frp = 10, satellite = "Aqua", daynight = "D",
               lon = c(rep(-85.25, length(fechas_x)), rep(-85.15, length(fechas_y)), -85.0),
               lat = c(rep(9.85, length(fechas_x)), rep(9.85, length(fechas_y)), 9.85)),
    coords = c("lon", "lat"), crs = 4326)
})

test_that("superficie_ac mide la tierra de cada AC y asignar_ac_ids usa la llave", {
  areas <- superficie_ac(ac_prueba, pais_prueba)
  expect_equal(areas$siglas_ac, c("ACX", "ACY"))
  # ACY solo tiene la mitad occidental dentro del país.
  expect_lt(areas$area_km2[2], areas$area_km2[1])
  expect_gt(areas$area_km2[2], 0.4 * areas$area_km2[1])
  ids <- asignar_ac_ids(puntos_ac, ac_prueba)
  expect_equal(names(ids), c("id_deteccion", "siglas_ac", "nombre_ac"))
  expect_equal(as.vector(table(ids$siglas_ac)[c("ACX", "ACY", ETIQUETA_SIN_AC)]),
               c(360L, 120L, 1L))
})

test_that("indices_temporada_ac aplica los umbrales y la anomalía por AC", {
  ids <- asignar_ac_ids(puntos_ac, ac_prueba)
  t <- indices_temporada_ac(puntos_ac, ids, rangos_ac, anios_base = 2020:2021,
                            satelite_control = "Aqua", minimo_dias_p95 = 100L)
  expect_false(ETIQUETA_SIN_AC %in% t$siglas_ac)
  x <- t[t$siglas_ac == "ACX", ]
  y <- t[t$siglas_ac == "ACY", ]
  expect_equal(x$dtot, c(120L, 120L, 120L))
  # 120 detecciones, una por día: 10 % el día 12, 90 % el día 108 → LON 97.
  expect_equal(x$lon, c(97L, 97L, 97L))
  # ACY tiene 40 por año: bajo el umbral de 100 → sin fechas, con FRP (≥ 30).
  expect_true(all(is.na(y$lon)) && all(is.na(y$frp95)))
  expect_true(all(y$frpi == 10))
  expect_true(all(y$pocas_detecciones))
  # P95 solo con ≥ 100 días de fuego en el periodo base: ACX (240) sí, ACY (80) no.
  expect_true(all(!is.na(x$p95)))
  expect_true(all(is.na(y$p95)) && all(is.na(y$nd95)))
  # Serie constante en el periodo base: sin dispersión, z NA.
  expect_true(all(is.na(x$z_dtot)))
  expect_equal(x$dias_base_p95[1], 240L)
})

test_that("consolidar_ac trata cada AC como una celda con su superficie", {
  areas <- superficie_ac(ac_prueba, pais_prueba)
  ids <- asignar_ac_ids(puntos_ac, ac_prueba)
  c <- consolidar_ac(puntos_ac, ids, areas, anios_referencia = 2020:2022,
                     anios_base = 2020:2021, satelite_control = "Aqua")
  expect_equal(c$siglas_ac, c("ACX", "ACY"))          # ordenadas por dtot
  expect_equal(c$nombre_ac, c("Área X", "Área Y"))
  expect_false("frec" %in% names(c))
  expect_equal(c$dtot, c(360L, 120L))
  expect_true(all(c$valida))
  expect_equal(c$fuera, c(0, 0))
  # Densidad sobre el periodo base: 240 detecciones de ACX en 2 años.
  expect_equal(c$dens[1], round(240 / c$area_km2[1] / 2, 4))
  expect_equal(c$aq, c(1, 1))
  expect_true(all(c$anio_inicio == 2020L & c$base_fin == 2021L))
})
