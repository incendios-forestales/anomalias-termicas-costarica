# Pruebas del módulo R/temporada.R con datos sintéticos.
# Ejecutar: Rscript -e 'targets::tar_source("R"); testthat::test_dir("tests/testthat")'

test_that("anio_fuego y dia_anio_fuego siguen el corte de septiembre", {
  expect_equal(anio_fuego(as.Date(c("2023-08-31", "2023-09-01", "2024-05-01"))),
               c(2023L, 2024L, 2024L))
  expect_equal(inicio_anio_fuego(2024L), as.Date("2023-09-01"))
  expect_equal(fin_anio_fuego(2024L), as.Date("2024-08-31"))
  expect_equal(dia_anio_fuego(as.Date(c("2023-09-01", "2024-01-01", "2024-08-31"))),
               c(1L, 123L, 366L))   # 2024 es bisiesto: 366 días
  expect_equal(dia_anio_fuego(as.Date("2023-08-31")), 365L)
  expect_true(is.na(inicio_anio_fuego(NA_integer_)))
})

test_that("filtrar_vegetacion conserva tipo 0 y ausente, excluye 1, 2 y 3", {
  puntos <- data.frame(acq_date = as.Date("2024-03-01") + 0:5,
                       type = c(0L, NA, 1L, 2L, 3L, 0L))
  expect_equal(nrow(filtrar_vegetacion(puntos)), 3L)
  sin_columna <- puntos[, "acq_date", drop = FALSE]
  expect_equal(nrow(filtrar_vegetacion(sin_columna)), 6L)
})

test_that("resumen_tipos siempre trae todas las categorías", {
  puntos <- data.frame(acq_date = as.Date(c("2024-03-01", "2024-03-02",
                                            "2024-12-01")),
                       type = c(0L, 1L, NA))
  r <- resumen_tipos(puntos)
  expect_equal(names(r), c("anio_fuego", "vegetacion", "sin_tipo",
                           "volcan_activo", "fuente_estatica", "mar",
                           "excluidas"))
  expect_equal(r$anio_fuego, c(2024L, 2025L))
  expect_equal(r$volcan_activo, c(1L, 0L))
  expect_equal(r$sin_tipo, c(0L, 1L))
  expect_equal(r$excluidas, c(1L, 0L))
})

rangos_prueba <- data.frame(
  data_id = c("MODIS_SP", "MODIS_NRT"), nivel = c("SP", "NRT"),
  inicio = as.Date(c("2020-09-01", "2021-07-01")),
  fin    = as.Date(c("2021-06-30", "2021-08-31"))
)

test_that("serie_diaria completa con ceros y asigna el nivel por día", {
  puntos <- data.frame(acq_date = as.Date(c(rep("2021-01-10", 10),
                                            rep("2021-03-01", 5))))
  d <- serie_diaria(puntos, rangos_prueba)
  expect_equal(nrow(d), 365L)
  expect_equal(sum(d$detecciones), 15L)
  expect_equal(d$detecciones[d$fecha == as.Date("2021-01-10")], 10L)
  expect_equal(d$detecciones[d$fecha == as.Date("2021-01-11")], 0L)
  expect_equal(unique(d$anio_fuego), 2021L)
  expect_equal(d$dia[1], 1L)
  expect_equal(d$nivel[d$fecha == as.Date("2021-06-30")], "SP")
  expect_equal(d$nivel[d$fecha == as.Date("2021-07-01")], "NRT")
})

test_that("indices_temporada calcula INI, FIN y LON por fracciones acumuladas", {
  # Una detección por día del 1 de enero al 10 de abril de 2021 (100 días):
  # el 10 % se alcanza el día 10 (10 de enero) y el 90 % el día 90 (31 de
  # marzo: 31 + 28 + 31).
  puntos <- data.frame(acq_date = as.Date("2021-01-01") + 0:99)
  d <- serie_diaria(puntos, rangos_prueba)
  ix <- indices_temporada(d, rangos_prueba, minimo = 50L)
  expect_equal(nrow(ix), 1L)
  expect_equal(ix$dtot, 100L)
  expect_equal(ix$ini_fecha, as.Date("2021-01-10"))
  expect_equal(ix$fin_fecha, as.Date("2021-03-31"))
  expect_equal(ix$lon, 81L)
  expect_equal(ix$ini_dia, dia_anio_fuego(as.Date("2021-01-10")))
  expect_false(ix$parcial)
  expect_true(ix$provisional)          # julio y agosto son NRT
  expect_false(ix$pocas_detecciones)
})

test_that("las marcas de parcial y pocas detecciones se activan", {
  rangos <- data.frame(data_id = "MODIS_SP", nivel = "SP",
                       inicio = as.Date("2021-01-01"),
                       fin = as.Date("2022-08-31"))
  puntos <- data.frame(acq_date = as.Date(c("2021-02-01", "2022-02-01")))
  ix <- indices_temporada(serie_diaria(puntos, rangos), rangos)
  expect_equal(ix$anio_fuego, c(2021L, 2022L))
  expect_equal(ix$parcial, c(TRUE, FALSE))   # 2021 empieza en enero
  expect_equal(ix$provisional, c(FALSE, FALSE))
  expect_true(all(ix$pocas_detecciones))
  expect_equal(ix$lon, c(1L, 1L))
  expect_equal(sum(temporada_confiable(ix)), 0L)
  expect_equal(notas_temporada(ix)[1], "año parcial; < 300 detecciones")
})

test_that("un año sin detecciones da NA en las fechas y no aborta", {
  rangos <- data.frame(data_id = "MODIS_SP", nivel = "SP",
                       inicio = as.Date("2020-09-01"),
                       fin = as.Date("2022-08-31"))
  puntos <- data.frame(acq_date = as.Date("2022-03-01"))
  ix <- indices_temporada(serie_diaria(puntos, rangos), rangos)
  expect_true(is.na(ix$ini_fecha[ix$anio_fuego == 2021L]))
  expect_true(is.na(ix$lon[ix$anio_fuego == 2021L]))
  expect_equal(ix$dtot, c(0L, 1L))
})

test_that("banda_referencia proyecta los meses al año de referencia", {
  b <- banda_referencia(EPOCA_SECA_IMN)
  expect_equal(unname(b[["xmin"]]), as.Date("2001-12-01"))
  expect_equal(unname(b[["xmax"]]), as.Date("2002-04-30"))
  s <- banda_referencia(TEMPORADA_SINAC)
  expect_equal(unname(s[["xmax"]]), as.Date("2002-05-31"))
})

# --- Grilla y consolidado ---------------------------------------------------

test_that("la grilla base y la de análisis anidan como dice el README", {
  # Celda base con esquina SW en (9,90 N, 85,25 O): su madre de 0,1° está
  # centrada en el nodo (9,9; -85,2) y tiene esquina SW en (9,85; -85,25).
  expect_equal(id_celda(9.90, -85.25), "c0990_m08525")
  sw <- esquina_sw(9.925, -85.225, GRILLA_RES_ANALISIS, centrada_en_nodos = TRUE)
  expect_equal(c(sw$lat_sw, sw$lon_sw), c(9.85, -85.25))
  # Un punto justo en el nodo cae en la celda que lo tiene como centro.
  sw <- esquina_sw(9.9, -85.2, GRILLA_RES_ANALISIS, centrada_en_nodos = TRUE)
  expect_equal(c(sw$lat_sw, sw$lon_sw), c(9.85, -85.25))
  # Grilla base: bordes en múltiplos de 0,05.
  sw <- esquina_sw(9.949, -85.201, GRILLA_RES_BASE, centrada_en_nodos = FALSE)
  expect_equal(c(sw$lat_sw, sw$lon_sw), c(9.90, -85.25))
})

area_prueba <- sf::st_as_sf(
  sf::st_sfc(sf::st_polygon(list(rbind(c(-85.3, 9.8), c(-85.0, 9.8),
                                       c(-85.0, 10.1), c(-85.3, 10.1),
                                       c(-85.3, 9.8)))), crs = 4326)
)

test_that("construir_grilla cubre el área y asigna la celda madre", {
  base <- construir_grilla(area_prueba, GRILLA_RES_BASE, centrada_en_nodos = FALSE)
  analisis <- construir_grilla(area_prueba, GRILLA_RES_ANALISIS,
                               centrada_en_nodos = TRUE)
  expect_true(all(base$celda_madre %in% analisis$celda_id))
  expect_true(all(round(analisis$lat_sw * 100) %% 10 == 5))   # bordes impares
  expect_true(all(round(base$lat_sw * 100) %% 5 == 0))
  expect_true(all(sf::st_area(base) > units::set_units(0, "m^2")))
  expect_equal(anyDuplicated(base$celda_id), 0L)
})

test_that("asignar_celda e indices_consolidados calculan por celda", {
  analisis <- construir_grilla(area_prueba, GRILLA_RES_ANALISIS,
                               centrada_en_nodos = TRUE)
  # 40 detecciones en una celda: una por día del 1 de enero al 9 de febrero
  # de 2021 (40 días), repartidas en dos años de fuego; 5 en otra celda.
  fechas <- c(as.Date("2021-01-01") + 0:19, as.Date("2022-01-01") + 20:39)
  puntos <- sf::st_as_sf(
    data.frame(id_deteccion = 1:45,
               acq_date = c(fechas, rep(as.Date("2021-03-01"), 5)),
               lon = c(rep(-85.18, 40), rep(-85.05, 5)),
               lat = c(rep(9.92, 40), rep(10.02, 5))),
    coords = c("lon", "lat"), crs = 4326)
  celdas <- asignar_celda(puntos, analisis)
  expect_equal(unique(celdas$celda_id[1:40]), "c0985_m08525")
  ix <- indices_consolidados(puntos, celdas, anios = 2021:2022, minimo = 30L)
  grande <- ix[ix$celda_id == "c0985_m08525", ]
  expect_equal(grande$dtot, 40L)
  # Días 1-40 desde el 1 de enero (día 123 del año de fuego): 10 % en el
  # cuarto día, 90 % en el día 36.
  expect_equal(grande$ini_dia, 123L + 3L)
  expect_equal(grande$fin_dia, 123L + 35L)
  expect_equal(grande$lon, 33L)
  chica <- ix[ix$celda_id != "c0985_m08525", ]
  expect_equal(chica$dtot, 5L)
  expect_false(chica$valida)
  expect_true(is.na(chica$lon))
  # Restringir los años excluye detecciones.
  ix21 <- indices_consolidados(puntos, celdas, anios = 2021L, minimo = 1L)
  expect_equal(ix21$dtot[ix21$celda_id == "c0985_m08525"], 20L)
})
