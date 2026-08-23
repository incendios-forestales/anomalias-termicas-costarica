# Descarga del límite nacional (SNIT/IGN) y de capas del SINAC vía WFS.

# Descarga idempotente de las 7 provincias de la cartografía oficial 1:5000
# del IGN (SNIT). Si el GPKG ya existe, lo reutiliza. Retorna el path al GPKG
# (target con format = "file").
descargar_provincias_wfs <- function(dest = "data/raw/wfs/provincias.gpkg") {
  if (file.exists(dest) && file.info(dest)$size > 0) {
    message(glue::glue("[cache] {basename(dest)} ya existe"))
    return(dest)
  }
  consulta <- paste0(
    WFS_SNIT,
    "?service=WFS&version=2.0.0&request=GetFeature",
    "&typeNames=", utils::URLencode(WFS_CAPA_PROVINCIAS, reserved = TRUE),
    "&outputFormat=", utils::URLencode("application/json", reserved = TRUE),
    "&srsName=", utils::URLencode(CRS_CRTM05, reserved = TRUE)
  )
  message(glue::glue("[descarga] provincias desde el WFS del SNIT (IGN 1:5000)"))
  provincias <- sf::st_read(consulta, quiet = TRUE)
  if (nrow(provincias) != N_PROVINCIAS) {
    stop("Se esperaban ", N_PROVINCIAS, " provincias; el WFS devolvió ",
         nrow(provincias), call. = FALSE)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(provincias, dest, delete_dsn = TRUE, quiet = TRUE)
  dest
}

# Construye el polígono del área de estudio: la unión de las provincias,
# SIN la Isla del Coco (toda parte con centroide al sur de LAT_MIN_CONTINENTAL;
# ver R/constantes.R). Devuelve un sf de una sola fila en CRTM05, análogo al
# polígono de parque del proyecto original: todo el pipeline recorta contra él.
construir_pais <- function(path) {
  provincias <- sf::st_read(path, quiet = TRUE) |> a_crtm05()
  partes <- provincias |>
    sf::st_union() |>
    sf::st_cast("POLYGON")
  lat <- partes |>
    sf::st_point_on_surface() |>
    sf::st_transform(CRS_WGS84) |>
    sf::st_coordinates()
  continentales <- partes[lat[, "Y"] >= LAT_MIN_CONTINENTAL]
  if (length(continentales) == length(partes)) {
    warning("Ningún polígono quedó excluido: ¿la capa de provincias ya no ",
            "incluye la Isla del Coco?", call. = FALSE)
  }
  sf::st_sf(
    nombre = AREA_NOMBRE,
    geometry = sf::st_combine(continentales)
  )
}

# Versión simplificada del límite nacional para productos web y figuras a
# escala nacional (en WGS84). El litoral 1:5000 completo pesa demasiado para
# HTML autocontenidos y es invisible a ~430 m/px.
pais_para_web <- function(pais) {
  simplificar_para_web(pais, tolerancia_m = TOLERANCIA_WEB_M)
}

# Descarga idempotente de una capa del WFS del SINAC recortada a un bbox
# (en CRTM05, formato de sf::st_bbox). Retorna el path al GPKG.
#
# Se usa WFS 1.0.0 con `bbox`: la capa de humedales se sirve en EPSG:8908
# (variante CRTM05) y con 2.0.0 el orden de ejes del filtro espacial es
# ambiguo entre versiones/servidores. Se pide srsName CRTM05 para
# homogeneizar la salida.
descargar_capa_wfs <- function(capa, bbox, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0) {
    message(glue::glue("[cache] {basename(dest)} ya existe"))
    return(dest)
  }
  consulta <- paste0(
    WFS_SINAC,
    "?service=WFS&version=1.0.0&request=GetFeature",
    "&typeName=", utils::URLencode(capa, reserved = TRUE),
    "&outputFormat=", utils::URLencode("application/json", reserved = TRUE),
    "&srsName=", utils::URLencode(CRS_CRTM05, reserved = TRUE),
    "&bbox=", paste(bbox[c("xmin", "ymin", "xmax", "ymax")], collapse = ",")
  )
  message(glue::glue("[descarga] {capa} desde el WFS del SINAC"))
  capa_sf <- sf::st_read(consulta, quiet = TRUE)
  if (nrow(capa_sf) == 0) {
    stop("El WFS no devolvió features para ", capa, " en el bbox solicitado.",
         call. = FALSE)
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  capa_sf |>
    a_crtm05() |>
    sf::st_write(dest, delete_dsn = TRUE, quiet = TRUE)
  dest
}
