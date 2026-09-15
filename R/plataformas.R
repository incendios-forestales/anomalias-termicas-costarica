# Catálogo de plataformas satelitales.
#
# Una fila por plataforma y de ella se deriva TODO lo visible: rótulos de
# figuras, pies de fuente, nombres de capa y rutas de salida. Es la pieza que
# mantiene a las plataformas como hermanas: ninguna es el valor por defecto de
# nada, porque todas salen de la misma tabla. Las funciones de R/ no tienen
# valores por defecto de rótulo justamente para que sea imposible generar un
# producto "de MODIS" por olvido.
#
# Columnas:
#   clave        identificador corto; da nombre a targets, directorios y rutas web
#   etiqueta     nombre completo para prosa y títulos
#   corta        nombre para rótulos de figura, donde el espacio es escaso
#   fuente_sp    colección de FIRMS con procesamiento estándar (NA si no existe)
#   fuente_nrt   colección de FIRMS en tiempo casi real
#   ba_producto  producto de área quemada que acompaña a la plataforma
#   ba_etiqueta  cómo se nombra ese producto en los rótulos (indica la
#                plataforma de origen cuando es prestado)
#   ba_creditos  cómo se nombra en los pies de fuente
#   ba_version   con número de versión, para el pie del producto de área quemada
#   base_inicio, base_fin
#                periodo base (años de fuego) de los índices que dependen del
#                conteo absoluto (README, «Tercer índice»): para MODIS,
#                2003–2022, los años con Terra y Aqua completos y sin deriva
#                orbital; para las VIIRS coincide con su periodo de
#                referencia (README, «Extensión a las plataformas VIIRS»).
#                NA excluye a la plataforma de la suite de índices (NOAA-21,
#                sin procesamiento estándar).
#   satelite_control
#                valor de `satellite` cuya fracción es el control AQ de la
#                intensidad (README, «Cuarto índice»): Aqua en MODIS, el
#                paso de la tarde en una serie de dos satélites. NA en las
#                plataformas de un solo satélite: AQ queda en NA y no marca
#                años no comparables.
#
# NOAA-20 y NOAA-21 no tienen producto de área quemada propio (VJ164A1 no está
# publicado; verificado en CMR el 2026-08-04), así que toman el de Suomi-NPP y
# lo declaran en la etiqueta.
PLATAFORMAS <- tibble::tribble(
  ~clave,   ~etiqueta,             ~corta,          ~fuente_sp,        ~fuente_nrt,          ~ba_producto, ~ba_etiqueta,     ~ba_creditos,          ~ba_version,             ~base_inicio, ~base_fin, ~satelite_control,
  "modis",  "MODIS (Terra/Aqua)",  "MODIS",         "MODIS_SP",        "MODIS_NRT",          "MCD64A1",    "MCD64A1",        "MCD64A1",             "MCD64A1 v6.1",          2003L,        2022L,     "Aqua",
  "snpp",   "VIIRS (Suomi-NPP)",   "VIIRS S-NPP",   "VIIRS_SNPP_SP",   "VIIRS_SNPP_NRT",     "VNP64A1",    "VNP64A1",        "VNP64A1",             "VNP64A1 v2",            2013L,        2025L,     NA,
  "noaa20", "VIIRS (NOAA-20)",     "VIIRS NOAA-20", "VIIRS_NOAA20_SP", "VIIRS_NOAA20_NRT",   "VNP64A1",    "VNP64A1, S-NPP", "VNP64A1, Suomi-NPP",  "VNP64A1 v2, Suomi-NPP", 2019L,        2025L,     NA,
  "noaa21", "VIIRS (NOAA-21)",     "VIIRS NOAA-21", NA,                "VIIRS_NOAA21_NRT",   "VNP64A1",    "VNP64A1, S-NPP", "VNP64A1, Suomi-NPP",  "VNP64A1 v2, Suomi-NPP", NA,           NA,        NA
)

# Plataformas que forman parte de la suite de índices de temporada: las que
# tienen periodo base. NOAA-21 queda fuera mientras no tenga procesamiento
# estándar (README, «Extensión a las plataformas VIIRS»).
plataformas_con_indices <- function() {
  PLATAFORMAS[!is.na(PLATAFORMAS$base_inicio) & !is.na(PLATAFORMAS$base_fin), ]
}

# Años de fuego del periodo base de una plataforma; error claro si no está
# fijado, porque un índice por conteo sin periodo base no debe calcularse.
anios_base <- function(clave) {
  p <- plataforma(clave)
  if (is.na(p$base_inicio) || is.na(p$base_fin)) {
    stop("La plataforma '", clave, "' no tiene periodo base definido en ",
         "PLATAFORMAS (base_inicio, base_fin).", call. = FALSE)
  }
  seq(p$base_inicio, p$base_fin)
}

# Fila de PLATAFORMAS, con error claro si la clave no existe (un típo en una
# clave produciría si no un data frame vacío y rótulos NA silenciosos).
plataforma <- function(clave) {
  fila <- PLATAFORMAS[PLATAFORMAS$clave == clave, ]
  if (nrow(fila) != 1) {
    stop("Plataforma desconocida: '", clave, "'. Definidas: ",
         paste(PLATAFORMAS$clave, collapse = ", "), call. = FALSE)
  }
  fila
}

# Identificador de la colección de FIRMS que nombra a la plataforma en los pies
# de fuente: el estándar cuando existe y el de tiempo casi real cuando no
# (NOAA-21 nunca ha tenido procesamiento estándar).
id_fuente_principal <- function(clave) {
  p <- plataforma(clave)
  if (is.na(p$fuente_sp)) p$fuente_nrt else p$fuente_sp
}

# Todas las cadenas visibles de una plataforma, derivadas de su fila. Las
# funciones de figuras y tablas reciben estos valores; así un cambio de
# nomenclatura se hace en un solo lugar y no puede quedar a medias entre
# plataformas.
etiquetas_plataforma <- function(clave) {
  p <- plataforma(clave)
  id <- id_fuente_principal(clave)
  # Los pies de fuente nombran TODAS las colecciones que alimentan la serie:
  # desde que se empalma la cola en tiempo casi real, citar solo el
  # procesamiento estándar dejaría sin acreditar los meses más recientes.
  ids <- if (is.na(p$fuente_sp)) p$fuente_nrt else
    paste(p$fuente_sp, "y", p$fuente_nrt)
  list(
    plataforma       = p$etiqueta,
    corta            = p$corta,
    # Satélite cuya fracción es el control AQ (NA: un solo satélite)
    satelite_control = p$satelite_control,
    id_fuente        = id,
    ids_fuente       = ids,
    # Subtítulo del video: todas las fuentes que componen lo que se ve
    # (detecciones estándar + cola NRT + producto de área quemada). unique()
    # evita repetir la NRT cuando es también la principal (NOAA-21).
    fuentes_video    = paste(unique(c(id, p$fuente_nrt, p$ba_etiqueta)),
                             collapse = " + "),
    # Rótulo del sensor en subtítulos de figura y encabezados de tabla
    fuente_fig       = paste0(p$corta, " (FIRMS)"),
    etiqueta_ba      = p$ba_etiqueta,
    etiqueta_quemas  = paste0("Área quemada (", p$ba_etiqueta, ")"),
    # Pies de fuente, por combinación de insumos usada en cada producto
    pie_firms        = paste0("Datos: NASA FIRMS (", ids, ")"),
    pie_animacion    = paste0("Datos: NASA FIRMS (", ids,
                              ") y SINAC. Fondo: ESA WorldCover 2021"),
    pie_cobertura    = paste0("Datos: NASA FIRMS (", ids,
                              "), ESA WorldCover 2021 y SINAC"),
    pie_ba           = paste0("Datos: NASA LP DAAC (", p$ba_version, ")"),
    pie_ambos        = paste0("Datos: NASA FIRMS (", ids, ") y LP DAAC (",
                              p$ba_creditos, ")"),
    pie_cobertura_ba = paste0("Datos: NASA LP DAAC (", p$ba_creditos,
                              "), ESA WorldCover 2021 y SINAC")
  )
}
