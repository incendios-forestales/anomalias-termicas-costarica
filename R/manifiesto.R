# Las salidas como interfaz: manifest.json y geometrías de outputs/.
#
# Implementa la sección «Las salidas como interfaz» del README. El manifiesto
# se escribe al final de la corrida leyendo el disco, para que liste lo que
# de verdad se publicó; las geometrías publican en GeoJSON lo que las tablas
# referencian por identificador (celda_id, siglas_ac).

# Versión del contrato de la interfaz: sube solo al renombrar o eliminar un
# archivo o una columna, nunca al agregar (README).
MANIFIESTO_CONTRATO <- 1L

# --- Geometrías -------------------------------------------------------------------

# Escribe una capa sf como GeoJSON en WGS84 (sobrescribe) y devuelve la ruta.
escribir_geojson <- function(capa, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(dest)) unlink(dest)
  sf::st_write(sf::st_transform(capa, CRS_WGS84), dest, driver = "GeoJSON",
               quiet = TRUE, layer_options = c("COORDINATE_PRECISION=5",
                                               "RFC7946=YES"))
  dest
}

# Las tres geometrías del contrato. La grilla ya está en WGS84 con sus
# atributos; las AC llevan la superficie terrestre de superficie_ac(); el país
# es la versión simplificada para la web.
escribir_geometrias <- function(grilla, ac_areas, pais_web, dir = "outputs/geometrias") {
  c(
    escribir_geojson(grilla[, c("celda_id", "lon_sw", "lat_sw", "area_km2")],
                     file.path(dir, "grilla_analisis.geojson")),
    escribir_geojson(simplificar_para_web(ac_areas)[, c("siglas_ac", "nombre_ac", "area_km2")],
                     file.path(dir, "areas_conservacion.geojson")),
    escribir_geojson(pais_web, file.path(dir, "pais.geojson"))
  )
}

# --- Manifiesto -------------------------------------------------------------------

# Tipo y plataforma (o par) de un archivo de outputs/ según su ruta:
# outputs/<tipo>/<plataforma|comparacion>/<nombre> o outputs/<tipo>/<nombre>.
clasificar_salida <- function(ruta) {
  partes <- strsplit(sub("^outputs/", "", ruta), "/", fixed = TRUE)[[1]]
  tipo <- partes[1]
  grupo <- if (length(partes) >= 3) partes[2] else NA_character_
  nombre <- partes[length(partes)]
  par <- if (!is.na(grupo) && grupo == "comparacion") {
    sub("_(anual|celdas|acuerdo|anomalias|dif_ini|dif_lon)\\..*$", "", sub("\\.tif$", "", nombre))
  } else NA_character_
  list(tipo = tipo,
       plataforma = if (!is.na(grupo) && grupo != "comparacion") grupo else NA_character_,
       par = par, nombre = nombre)
}

# SHA-256 de un archivo como cadena hexadecimal sin clase: el objeto `hash`
# de openssl no se serializa a JSON según el contexto en que se cargue.
sha256_hex <- function(ruta) {
  con <- file(ruta, "rb"); on.exit(close(con))
  paste(sprintf("%02x", as.integer(unclass(openssl::sha256(con)))), collapse = "")
}

# manifest.json de la corrida. `dependencias` no se usa: existe para que el
# target se ejecute después de todo lo que publica (las páginas), y el
# listado sale del disco. `rangos` es una lista con nombre por plataforma.
escribir_manifiesto <- function(dest, rangos, pares, traslapes, dependencias = NULL) {
  archivos <- list.files("outputs", recursive = TRUE, full.names = TRUE)
  archivos <- archivos[!grepl("manifest\\.json$", archivos)]
  lista <- purrr::map(archivos, function(a) {
    c(clasificar_salida(a),
      list(ruta = a, bytes = file.size(a),
           sha256 = sha256_hex(a)))
  })
  plataformas <- purrr::pmap(PLATAFORMAS, function(clave, etiqueta, corta, base_inicio,
                                                   base_fin, satelite_control, ...) {
    r <- rangos[[clave]]
    list(clave = clave, etiqueta = etiqueta, corta = corta,
         en_suite = !is.na(base_inicio),
         base_inicio = base_inicio, base_fin = base_fin,
         satelite_control = satelite_control,
         registro_inicio = as.character(min(r$inicio)),
         fin_estandar = if (any(r$nivel == "SP")) as.character(max(r$fin[r$nivel == "SP"])) else NA,
         ultimo_dia = as.character(max(r$fin)))
  })
  pares_lista <- purrr::pmap(pares[, c("par", "a", "b")], function(par, a, b) {
    tr <- traslapes[[par]]
    list(par = par, a = a, b = b, traslape_inicio = min(tr), traslape_fin = max(tr),
         anios = length(tr))
  })
  manifiesto <- list(
    contrato = MANIFIESTO_CONTRATO,
    proyecto = "anomalias-termicas-costarica",
    generado = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    corrida = as.character(Sys.Date()),
    plataformas = plataformas,
    pares = pares_lista,
    archivos = lista
  )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(manifiesto, dest, auto_unbox = TRUE, pretty = TRUE,
                       na = "null", digits = NA)
  dest
}
