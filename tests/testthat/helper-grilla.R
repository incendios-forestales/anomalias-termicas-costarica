# Área de prueba compartida (helper: testthat lo carga antes que los tests).
area_prueba <- sf::st_as_sf(
  sf::st_sfc(sf::st_polygon(list(rbind(c(-85.3, 9.8), c(-85.0, 9.8),
                                       c(-85.0, 10.1), c(-85.3, 10.1),
                                       c(-85.3, 9.8)))), crs = 4326)
)
