# Pruebas del módulo R/fuentes_estaticas.R con detecciones sintéticas sobre
# la grilla de prueba compartida (helper-grilla.R): las celdas de análisis
# tienen bordes en 9,75, 9,85, 9,95 y en −85,35, −85,25, −85,15, −85,05.

grilla_fuentes <- construir_grilla(area_prueba, GRILLA_RES_ANALISIS,
                                   centrada_en_nodos = TRUE)

# Tres celdas: A con 12 reflejos urbanos (tipo 2, diurnos, Terra) y 8 de
# vegetación; B con 15 volcánicas (tipo 1); C con 12 nocturnas de tipo 2 en
# tres años; D con 3 de tipo 2 (bajo el umbral).
puntos_fuentes <- local({
  fila <- function(n, lon, lat, type, daynight, satellite, anio) {
    data.frame(acq_date = as.Date(sprintf("%d-03-01", anio)) + seq_len(n) - 1,
               type = type, daynight = daynight, satellite = satellite,
               confidence = 45, frp = 10, longitude = lon, latitude = lat)
  }
  d <- rbind(
    fila(12, -85.18, 9.92, 2L, "D", "Terra", 2020),
    fila(8, -85.18, 9.92, 0L, "D", "Aqua", 2020),
    fila(15, -85.05, 10.02, 1L, "N", "Aqua", 2021),
    fila(4, -85.28, 9.82, 2L, "N", "Aqua", 2019),
    fila(4, -85.28, 9.82, 2L, "N", "Aqua", 2020),
    fila(4, -85.28, 9.82, 2L, "N", "Aqua", 2021),
    fila(3, -85.05, 9.85, 2L, "D", "Terra", 2022)
  )
  d$id_deteccion <- seq_len(nrow(d))
  sf::st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
})

test_that("metricas_fuentes_estaticas resume por celda y firma con las reglas", {
  m <- metricas_fuentes_estaticas(puntos_fuentes, grilla_fuentes, "Aqua")
  expect_equal(nrow(m), 4L)                      # solo celdas con tipo 1 o 2
  expect_equal(m$n_est[1], 15L)                  # ordenadas por N_EST
  a <- m[m$n_fija == 12 & m$diurna == 1, ]
  expect_equal(a$n_total, 20L)
  expect_equal(a$pct_est, 60)
  expect_equal(a$manana, 1)                      # todo Terra
  expect_equal(as.character(a$firma), "Reflejo urbano diurno (probable)")
  b <- m[m$n_volcan == 15, ]
  expect_equal(as.character(b$firma), "Cráter volcánico")
  c <- m[m$n_fija == 12 & m$diurna == 0, ]
  expect_equal(c$anios_est, 3L)
  expect_equal(as.character(c$firma), "Fuente térmica nocturna persistente")
  d <- m[m$n_est == 3, ]
  expect_false(d$es_fuente)
  expect_equal(m$conf_est[1], 45)
  expect_true(all(m$disp_m == 0))                # puntos idénticos
  expect_true(all(is.na(m$localidad)))            # celdas fuera del catálogo
})

test_that("sin satélite de control la condición de Terra no aplica", {
  m <- metricas_fuentes_estaticas(puntos_fuentes, grilla_fuentes, NA)
  expect_true(all(is.na(m$manana)))
  a <- m[m$n_fija == 12 & m$diurna == 1, ]
  expect_equal(as.character(a$firma), "Reflejo urbano diurno (probable)")
  # Confianza categórica (VIIRS): sin resumen.
  p <- puntos_fuentes; p$confidence <- "n"
  m2 <- metricas_fuentes_estaticas(p, grilla_fuentes, NA)
  expect_true(all(is.na(m2$conf_est)))
})

test_that("el catálogo tiene celdas únicas con formato de celda", {
  expect_false(any(duplicated(CATALOGO_FUENTES_ESTATICAS$celda_id)))
  expect_true(all(grepl("^c\\d{4}_m\\d{5}$", CATALOGO_FUENTES_ESTATICAS$celda_id)))
})
