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
  expect_equal(ix$df, 100L)
  expect_equal(ix$n50, 50L)      # una detección por día: 50 días para el 50 %
  expect_equal(ix$c10, 10)       # 10 de 100
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
  expect_equal(grande$fuera, 0)          # todo entre enero y febrero
  expect_false(grande$sin_estacion)
  # Restringir los años excluye detecciones.
  ix21 <- indices_consolidados(puntos, celdas, anios = 2021L, minimo = 1L)
  expect_equal(ix21$dtot[ix21$celda_id == "c0985_m08525"], 20L)
})

test_that("una celda bimodal queda sin estación definida", {
  analisis <- construir_grilla(area_prueba, GRILLA_RES_ANALISIS,
                               centrada_en_nodos = TRUE)
  # 20 detecciones en marzo y 20 en setiembre de 2021: 50 % fuera de dic-may.
  puntos <- sf::st_as_sf(
    data.frame(id_deteccion = 1:40,
               acq_date = c(as.Date("2021-03-01") + 0:19,
                            as.Date("2021-09-05") + 0:19),
               lon = -85.18, lat = 9.92),
    coords = c("lon", "lat"), crs = 4326)
  celdas <- asignar_celda(puntos, analisis)
  ix <- indices_consolidados(puntos, celdas, anios = 2021:2022, minimo = 30L)
  expect_equal(ix$fuera, 50)
  expect_true(ix$valida)
  expect_true(ix$sin_estacion)
  expect_true(is.na(ix$lon) && is.na(ix$ini_dia))
  # Con un umbral más permisivo sí se calcula y abarca ambos picos.
  ix2 <- indices_consolidados(puntos, celdas, anios = 2021:2022, minimo = 30L,
                              fuera_max = 60)
  expect_false(ix2$sin_estacion)
  expect_gt(ix2$lon, 150)
  expect_equal(nrow(trama_celdas(celdas_temporada_sf(ix, analisis))), 1L)
})


test_that("n50 y c10 ordenan de mayor a menor y toleran empates y ceros", {
  d <- c(0, 30, 10, 5, 3, 2, 0, 0)          # total 50
  expect_equal(n50(d), 1L)                    # 30 >= 25
  expect_equal(c10(d), 100)                   # solo hay 5 días con fuego
  expect_equal(n50(c(5, 5, 5, 5)), 2L)        # empates: 10 >= 10
  expect_equal(c10(rep(1, 40)), 25)           # 10 de 40
  expect_true(is.na(n50(c(0, 0))))
  expect_true(is.na(c10(numeric(0))))
})

test_that("N50F por celda usa las fechas reales y su propio umbral", {
  analisis <- construir_grilla(area_prueba, GRILLA_RES_ANALISIS,
                               centrada_en_nodos = TRUE)
  # Una celda con 5 fechas: 30, 10, 5, 3, 2 detecciones (50 en total,
  # todas en marzo de 2021): N50 = 1, DF = 5, N50F = 0,2.
  fechas <- rep(as.Date("2021-03-01") + 0:4, times = c(30, 10, 5, 3, 2))
  puntos <- sf::st_as_sf(
    data.frame(id_deteccion = seq_along(fechas), acq_date = fechas,
               lon = -85.18, lat = 9.92),
    coords = c("lon", "lat"), crs = 4326)
  celdas <- asignar_celda(puntos, analisis)
  ix <- indices_consolidados(puntos, celdas, anios = 2021L, minimo = 30L,
                             minimo_concentracion = 40L)
  expect_equal(ix$df, 5L)
  expect_true(ix$valida_n50f)
  expect_equal(ix$n50f, 0.2)
  ix2 <- indices_consolidados(puntos, celdas, anios = 2021L, minimo = 30L,
                              minimo_concentracion = 100L)
  expect_false(ix2$valida_n50f)
  expect_true(is.na(ix2$n50f))
})


test_that("la grilla conoce la superficie terrestre de cada celda", {
  analisis <- construir_grilla(area_prueba, GRILLA_RES_ANALISIS,
                               centrada_en_nodos = TRUE)
  expect_true(all(analisis$area_km2 > 0))
  # Una celda entera de 0,1° a ~10 N mide ~121 km²; ninguna puede superarlo.
  expect_true(all(analisis$area_km2 <= 125))
  # La suma de las piezas es el área del rectángulo de prueba (~0,3° × 0,3°).
  total <- as.numeric(sf::st_area(a_crtm05(area_prueba))) / 1e6
  expect_equal(sum(analisis$area_km2), total, tolerance = 0.01)
})

test_that("indices_frecuencia cubre toda la grilla y usa el periodo base", {
  analisis <- construir_grilla(area_prueba, GRILLA_RES_ANALISIS,
                               centrada_en_nodos = TRUE)
  # 6 detecciones en una celda: 4 en el año de fuego 2020 y 2 en 2022; nada
  # en 2021 ni 2023. Periodo base 2020-2023: FREC = 2/4.
  puntos <- sf::st_as_sf(
    data.frame(id_deteccion = 1:6,
               acq_date = as.Date(c("2020-03-01", "2020-03-02", "2020-03-03",
                                    "2020-04-01", "2022-02-10", "2022-02-11")),
               frp = c(5, 10, 20, 40, 80, 160),
               satellite = c("Terra", "Aqua", "Aqua", "Terra", "Aqua", "Aqua"),
               daynight = c("D", "D", "D", "N", "D", "D"),
               lon = -85.18, lat = 9.92),
    coords = c("lon", "lat"), crs = 4326)
  celdas <- asignar_celda(puntos, analisis)
  fr <- indices_frecuencia(puntos, celdas, analisis, anios = 2020:2023,
                           satelite_control = "Aqua", min_area = 1, minimo = 5L)
  # Intensidad y control de satélite por celda sobre el periodo base.
  expect_equal(fr$frpi[fr$celda_id == "c0985_m08525"], 30)     # mediana de 5..160
  expect_equal(fr$aq[fr$celda_id == "c0985_m08525"], round(4 / 6, 3))
  expect_true(all(is.na(fr$frpi[fr$celda_id != "c0985_m08525"])))
  fr_u <- indices_frecuencia(puntos, celdas, analisis, anios = 2020:2023,
                             satelite_control = "Aqua", min_area = 1, minimo = 30L)
  expect_true(all(is.na(fr_u$frpi)))
  expect_true(all(fr_u$frec[fr_u$celda_id == "c0985_m08525"] == 0.5))
  expect_equal(nrow(fr), nrow(analisis))
  con <- fr[fr$celda_id == "c0985_m08525", ]
  expect_equal(con$dtot_base, 6L)
  expect_equal(con$anios, 2L)
  expect_equal(con$frec, 0.5)
  expect_equal(con$dens, round(6 / con$area_km2 / 4, 4))
  sin <- fr[fr$celda_id != "c0985_m08525", ]
  expect_true(all(sin$dtot_base == 0 & sin$frec == 0 & sin$dens == 0))
  # Fuera del periodo base no cuenta.
  fr2 <- indices_frecuencia(puntos, celdas, analisis, anios = 2021:2023,
                            satelite_control = "Aqua", min_area = 1)
  expect_equal(fr2$anios[fr2$celda_id == "c0985_m08525"], 1L)
  # Umbral de superficie: con un mínimo imposible, todo NA.
  fr3 <- indices_frecuencia(puntos, celdas, analisis, anios = 2020:2023,
                            satelite_control = "Aqua", min_area = 1e6)
  expect_true(all(is.na(fr3$frec)))
  # La unión conserva todas las celdas, con ceros donde no hubo fuego.
  cons <- indices_consolidados(puntos, celdas, anios = 2020:2023, minimo = 3L,
                               minimo_concentracion = 3L)
  u <- unir_consolidados(cons, fr, anios = 2020:2023)
  expect_equal(nrow(u), nrow(analisis))
  expect_equal(u$dtot[u$celda_id == "c0985_m08525"], 6L)
  expect_true(all(u$dtot[u$celda_id != "c0985_m08525"] == 0))
  expect_true(all(is.na(u$lon[u$celda_id != "c0985_m08525"])))
  expect_false(any(is.na(u$valida)))
  # El periodo de referencia es el vector de años, no la columna `anios`.
  expect_true(all(u$anio_inicio == 2020L & u$anio_fin == 2023L))
  expect_true(all(u$base_inicio == 2020L & u$base_fin == 2023L))
  # Sin satélite de control (VIIRS), AQ queda en NA y lo demás no cambia.
  fr_v <- indices_frecuencia(puntos, celdas, analisis, anios = 2020:2023,
                             satelite_control = NA, min_area = 1, minimo = 5L)
  expect_true(all(is.na(fr_v$aq)))
  expect_equal(fr_v$frpi, fr$frpi)
  expect_equal(fr_v$frec, fr$frec)
})

test_that("plataformas_con_indices son las que tienen periodo base", {
  con <- plataformas_con_indices()
  expect_equal(con$clave, c("modis", "snpp", "noaa20"))
  expect_equal(anios_base("snpp"), 2013:2025)
  expect_equal(anios_base("noaa20"), 2019:2025)
  expect_equal(plataforma("modis")$satelite_control, "Aqua")
  expect_true(is.na(plataforma("snpp")$satelite_control))
  expect_true(is.na(etiquetas_plataforma("noaa20")$satelite_control))
})

test_that("anios_base falla con claridad si la plataforma no lo tiene", {
  expect_equal(anios_base("modis"), 2003:2022)
  expect_error(anios_base("noaa21"), "periodo base")
})


test_that("indices_intensidad resume FRP y controles por año de fuego", {
  puntos <- data.frame(
    id_deteccion = 1:8,
    acq_date = as.Date(c(rep("2021-03-01", 4), rep("2022-03-01", 4))),
    frp = c(1, 2, 3, 100, 10, 10, 10, 10),
    satellite = c("Terra", "Terra", "Terra", "Terra", "Aqua", "Aqua", "Terra", "Terra"),
    daynight = c("D", "D", "N", "N", "D", "D", "D", "D")
  )
  ix <- indices_intensidad(puntos, satelite_control = "Aqua")
  expect_equal(ix$anio_fuego, c(2021L, 2022L))
  expect_equal(ix$frpi, c(2.5, 10))
  expect_equal(ix$frp95[2], 10)
  expect_gt(ix$frp95[1], 3)              # la cola pesa en el percentil 95
  expect_equal(ix$aq, c(0, 0.5))
  expect_equal(ix$noc, c(0.5, 0))
  # La unión conserva los años sin detecciones con NA.
  rangos <- data.frame(data_id = "MODIS_SP", nivel = "SP",
                       inicio = as.Date("2020-09-01"), fin = as.Date("2022-08-31"))
  anual <- indices_temporada(serie_diaria(puntos, rangos), rangos)
  u <- unir_intensidad(anual, ix)
  expect_equal(u$frpi, c(2.5, 10))
  expect_true(all(c("frpi", "frp95", "aq", "noc") %in% names(u)))
  # Plataforma de un solo satélite: AQ en NA, FRP y NOC iguales.
  ix_v <- indices_intensidad(puntos, satelite_control = NA)
  expect_true(all(is.na(ix_v$aq)))
  expect_equal(ix_v$frpi, ix$frpi)
  expect_equal(ix_v$noc, ix$noc)
})


test_that("umbral_p95 e indices_extremos siguen la lógica del ETCCDI", {
  rangos <- data.frame(data_id = "MODIS_SP", nivel = "SP",
                       inicio = as.Date("2019-09-01"), fin = as.Date("2022-08-31"))
  # Año de fuego 2020: 100 días con 1 detección y 5 días con 50.
  # Año 2021: 20 días con 1. Año 2022: 3 días con 60.
  fechas <- c(as.Date("2020-01-01") + 0:99, rep(as.Date("2020-04-20") + 0:4, each = 50),
              as.Date("2021-02-01") + 0:19, rep(as.Date("2022-03-01") + 0:2, each = 60))
  puntos <- data.frame(id_deteccion = seq_along(fechas), acq_date = fechas,
                       frp = 10, satellite = "Aqua", daynight = "D")
  d <- serie_diaria(puntos, rangos)
  # Umbral sobre 2020-2021 (125 días de fuego: 120 con 1 y 5 con 50): P95 = 1
  # (el 95 % de los días tiene 1), así que los 5 días de 50 lo superan.
  p95 <- umbral_p95(d, anios = 2020:2021)
  expect_equal(p95, 1)
  ex <- indices_extremos(d, p95)
  expect_equal(ex$nd95, c(5L, 0L, 3L))
  expect_equal(ex$d95p, c(250L, 0L, 180L))
  expect_equal(ex$d95ptot, c(round(100 * 250 / 350, 1), 0, 100))
  expect_true(all(ex$p95 == 1))
  # Umbral estricto: con el 99 % nada supera en 2021.
  expect_error(umbral_p95(d, anios = 2030L), "periodo base")
  anual <- unir_intensidad(indices_temporada(d, rangos),
                           indices_intensidad(puntos, "Aqua"))
  u <- unir_extremos(anual, ex, satelite_control = "Aqua")
  expect_false(any(u$no_comparable))          # todo Aqua
  anual$aq[anual$anio_fuego == 2020L] <- 0
  u2 <- unir_extremos(anual, ex, satelite_control = "Aqua")
  expect_equal(u2$no_comparable, c(TRUE, FALSE, FALSE))
  expect_match(notas_temporada(u2, "Aqua")[1], "sin Aqua")
  # Sin satélite de control (VIIRS) ningún año es no comparable, aunque AQ
  # esté en NA, y la nota no menciona satélite alguno.
  anual_v <- unir_intensidad(indices_temporada(d, rangos),
                             indices_intensidad(puntos, NA))
  u3 <- unir_extremos(anual_v, ex, satelite_control = NA)
  expect_false(any(u3$no_comparable))
  expect_false(any(grepl("comparables", notas_temporada(u3, NA))))
  expect_equal(formato_p95(29), "29")
  expect_equal(formato_p95(108.8), "108,8")
})
