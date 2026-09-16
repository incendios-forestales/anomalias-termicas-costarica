# Pruebas del módulo R/manifiesto.R.

test_that("clasificar_salida reconoce tipo, plataforma y par por la ruta", {
  a <- clasificar_salida("outputs/tables/modis/temporada_anual.csv")
  expect_equal(a$tipo, "tables"); expect_equal(a$plataforma, "modis")
  expect_true(is.na(a$par)); expect_equal(a$nombre, "temporada_anual.csv")
  b <- clasificar_salida("outputs/figs/comparacion/modis_snpp_dif_ini.png")
  expect_equal(b$tipo, "figs"); expect_true(is.na(b$plataforma))
  expect_equal(b$par, "modis_snpp")
  c <- clasificar_salida("outputs/rasters/comparacion/snpp_noaa20.tif")
  expect_equal(c$par, "snpp_noaa20")
  d <- clasificar_salida("outputs/figs/series_plataformas.png")
  expect_true(is.na(d$plataforma)); expect_equal(d$nombre, "series_plataformas.png")
  e <- clasificar_salida("outputs/geometrias/pais.geojson")
  expect_equal(e$tipo, "geometrias")
})

test_that("escribir_geojson produce GeoJSON en WGS84 con los atributos", {
  dest <- file.path(tempdir(), "g", "prueba.geojson")
  capa <- sf::st_sf(id = c("a", "b"), geometry = sf::st_sfc(
    sf::st_point(c(-85.2, 9.9)), sf::st_point(c(-85.1, 10.0)), crs = 4326)) |>
    sf::st_transform(CRS_CRTM05)
  r <- escribir_geojson(capa, dest)
  expect_true(file.exists(r))
  leida <- sf::st_read(r, quiet = TRUE)
  expect_equal(leida$id, c("a", "b"))
  expect_equal(sf::st_crs(leida)$epsg, 4326L)
  # Sobrescribe sin fallar.
  expect_equal(escribir_geojson(capa, dest), dest)
})

test_that("escribir_manifiesto lista lo que hay en outputs/ y las plataformas", {
  raiz <- tempfile("manif"); dir.create(raiz)
  antes <- setwd(raiz); on.exit(setwd(antes), add = TRUE)
  dir.create("outputs/tables/modis", recursive = TRUE)
  writeLines("a,b\n1,2", "outputs/tables/modis/temporada_anual.csv")
  # Las carpetas *_files de los widgets no se publican (.gitignore): fuera.
  dir.create("outputs/tables/modis/resumen_anual_files/lib", recursive = TRUE)
  writeLines("x", "outputs/tables/modis/resumen_anual_files/lib/a.css")
  rangos <- purrr::map(stats::setNames(PLATAFORMAS$clave, PLATAFORMAS$clave), function(k) {
    data.frame(nivel = c("SP", "NRT"), inicio = as.Date(c("2012-01-20", "2026-05-01")),
               fin = as.Date(c("2026-04-30", "2026-09-13")))
  })
  pares <- pares_comparacion()
  traslapes <- purrr::map(stats::setNames(pares$par, pares$par), \(p) 2019:2025)
  dest <- escribir_manifiesto("outputs/manifest.json", rangos, pares, traslapes)
  m <- jsonlite::read_json(dest)
  expect_equal(m$contrato, MANIFIESTO_CONTRATO)
  expect_equal(length(m$plataformas), nrow(PLATAFORMAS))
  expect_true(m$plataformas[[1]]$en_suite)
  expect_false(m$plataformas[[4]]$en_suite)             # NOAA-21
  expect_equal(m$plataformas[[1]]$satelite_control, "Aqua")
  expect_null(m$plataformas[[2]]$satelite_control)      # NA → null
  expect_equal(m$plataformas[[1]]$fin_estandar, "2026-04-30")
  expect_equal(length(m$pares), nrow(pares))
  expect_equal(m$pares[[1]]$anios, 7L)
  expect_equal(length(m$archivos), 1L)                   # el manifiesto no se lista
  expect_equal(m$archivos[[1]]$plataforma, "modis")
  expect_equal(nchar(m$archivos[[1]]$sha256), 64L)
})
