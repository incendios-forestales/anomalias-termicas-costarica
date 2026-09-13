# Grilla de análisis común a plataformas y variables climáticas.
#
# Ver README, «Ráster consolidado de LON». Celdas regulares en WGS84 con dos
# niveles anidados (GRILLA_RES_BASE y GRILLA_RES_ANALISIS, R/constantes.R).
# El identificador de celda se deriva de su esquina suroeste en centésimas
# de grado ("c0985_m08525" = 9,85 N, 85,25 O) y es la llave entre índices,
# clima y cobertura, con el papel que `id_deteccion` tiene entre detecciones
# y capas. Todo el cálculo de pertenencia es aritmético: para una grilla
# regular en lat/lon no hace falta una unión espacial.

# Identificador a partir de la esquina suroeste (grados; lon negativa).
id_celda <- function(lat_sw, lon_sw) {
  sprintf("c%04d_m%05d", as.integer(round(lat_sw * 100)),
          as.integer(round(-lon_sw * 100)))
}

# Desplazamiento de los bordes respecto de los múltiplos de `res`: 0 para la
# grilla base (bordes en múltiplos de 0,05°) y res/2 para la de análisis
# centrada en los nodos de ERA5-Land (bordes en múltiplos impares de 0,05°).
desplazamiento_grilla <- function(res, centrada_en_nodos) {
  if (centrada_en_nodos) res / 2 else 0
}

# Esquina suroeste de la celda que contiene cada punto. Se opera en
# centésimas de grado enteras para evitar que floor() de un flotante caiga en
# la celda vecina justo en un borde.
esquina_sw <- function(lat, lon, res, centrada_en_nodos) {
  r <- as.integer(round(res * 100))
  d <- as.integer(round(desplazamiento_grilla(res, centrada_en_nodos) * 100))
  lat_h <- floor((lat * 100 - d) / r) * r + d
  lon_h <- floor((lon * 100 - d) / r) * r + d
  data.frame(lat_sw = lat_h / 100, lon_sw = lon_h / 100)
}

# Polígonos sf (WGS84) de las celdas que intersecan el área, con celda_id y,
# para la grilla base, el identificador de su celda madre de análisis.
construir_grilla <- function(area, res, centrada_en_nodos) {
  area_wgs84 <- sf::st_transform(area, CRS_WGS84)
  b <- sf::st_bbox(area_wgs84)
  d <- desplazamiento_grilla(res, centrada_en_nodos)
  # Bordes de celda que cubren el bbox, con margen de una celda.
  x0 <- floor((b[["xmin"]] - d) / res) * res + d - res
  x1 <- ceiling((b[["xmax"]] - d) / res) * res + d + res
  y0 <- floor((b[["ymin"]] - d) / res) * res + d - res
  y1 <- ceiling((b[["ymax"]] - d) / res) * res + d + res
  celdas <- sf::st_make_grid(
    cellsize = c(res, res),
    offset = c(x0, y0),
    n = c(round((x1 - x0) / res), round((y1 - y0) / res)),
    crs = CRS_WGS84, what = "polygons"
  )
  sw <- sf::st_coordinates(sf::st_centroid(celdas)) - res / 2
  grilla <- sf::st_sf(
    celda_id = id_celda(sw[, "Y"], sw[, "X"]),
    lat_sw = round(sw[, "Y"], 4), lon_sw = round(sw[, "X"], 4),
    geometry = celdas
  )
  toca <- lengths(sf::st_intersects(grilla, area_wgs84)) > 0
  grilla <- grilla[toca, ]
  # Superficie terrestre de cada celda (km², en CRTM05): su intersección con
  # el área. Es el denominador de la densidad; las celdas de costa tienen
  # una fracción pequeña de tierra.
  grilla$area_km2 <- area_terrestre_km2(grilla, area)
  if (!centrada_en_nodos) {
    madre <- esquina_sw(grilla$lat_sw + res / 2, grilla$lon_sw + res / 2,
                        GRILLA_RES_ANALISIS, centrada_en_nodos = TRUE)
    grilla$celda_madre <- id_celda(madre$lat_sw, madre$lon_sw)
  }
  rownames(grilla) <- NULL
  grilla
}

# Superficie terrestre (km²) de cada celda: intersección con el área en
# CRTM05, sumada por celda (una celda puede cortar el área en varias piezas).
area_terrestre_km2 <- function(grilla, area) {
  celdas_m <- a_crtm05(grilla)
  union_m <- sf::st_union(a_crtm05(area))
  piezas <- suppressWarnings(sf::st_intersection(celdas_m, union_m))
  areas <- tapply(as.numeric(sf::st_area(piezas)) / 1e6, piezas$celda_id, sum)
  out <- as.numeric(areas[grilla$celda_id])
  out[is.na(out)] <- 0
  round(out, 3)
}

# Celda de análisis de cada detección: tabla (id_deteccion, celda_id). Se
# verifica que toda celda exista en la grilla: una detección dentro del país
# siempre cae en una celda que lo interseca, así que un faltante es un error
# de geometría, no un caso legítimo.
asignar_celda <- function(puntos, grilla, res = GRILLA_RES_ANALISIS,
                          centrada_en_nodos = TRUE) {
  coords <- sf::st_coordinates(sf::st_transform(puntos, CRS_WGS84))
  sw <- esquina_sw(coords[, "Y"], coords[, "X"], res, centrada_en_nodos)
  celdas <- data.frame(
    id_deteccion = puntos$id_deteccion,
    celda_id = id_celda(sw$lat_sw, sw$lon_sw)
  )
  faltantes <- setdiff(unique(celdas$celda_id), grilla$celda_id)
  if (length(faltantes) > 0) {
    stop("Detecciones en celdas fuera de la grilla: ",
         paste(head(faltantes), collapse = ", "), call. = FALSE)
  }
  celdas
}
