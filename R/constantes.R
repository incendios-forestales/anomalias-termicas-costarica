# Constantes y configuración del proyecto.
#
# CRS de referencia:
#   EPSG:4326 — WGS 84 (coordenadas de FIRMS y del API de área)
#   EPSG:5367 — CRTM05 (CRS oficial métrico de Costa Rica, para mapas y áreas)

CRS_WGS84  <- "EPSG:4326"
CRS_CRTM05 <- "EPSG:5367"

# Sin barras de progreso de terra: a escala nacional aggregate()/project()
# las imprimen, y dentro de un chunk de Quarto esa salida de texto termina
# renderizada en el reporte como líneas "|---------|====". Se fija aquí
# porque este archivo se carga siempre (tar_source), tanto en el pipeline
# como en los qmd.
if (requireNamespace("terra", quietly = TRUE)) {
  terra::terraOptions(progress = 0)
}

# --- Área de estudio ---
# Costa Rica continental + islas cercanas. La Isla del Coco (5,5 N, 87,1 O)
# queda EXCLUIDA: su actividad de fuego es nula, estira el bbox de descarga a
# un rectángulo dominado por océano (donde FIRMS detecta barcos) y queda fuera
# de la zona de validez práctica de CRTM05. La exclusión se documenta en el
# README y en los reportes.
AREA_NOMBRE       <- "Costa Rica"
AREA_TITULO       <- "Anomalías térmicas en Costa Rica"
AREA_LIMITE_LABEL <- "Límite nacional"

# Umbral de latitud (grados N) para descartar las partes insulares lejanas al
# construir el polígono continental: solo la Isla del Coco cae al sur de 7 N.
LAT_MIN_CONTINENTAL <- 7

# --- SNIT / IGN (límite nacional) ---
# El límite nacional se construye como la unión de las 7 provincias de la
# cartografía oficial 1:5000 del IGN, servida por el SNIT.
WFS_SNIT            <- "https://geos.snitcr.go.cr/be/IGN_5_CO/wfs"
WFS_CAPA_PROVINCIAS <- "IGN_5_CO:limiteprovincial_5k"
N_PROVINCIAS        <- 7L

# --- SINAC / WFS ---
WFS_SINAC <- "https://geos1pne.sirefor.go.cr/wfs"

# Capas nacionales de contexto y desagregación (mismo geoserver).
# Las áreas de conservación son la unidad de desagregación espacial de los
# reportes (10 features terrestres con nombre_ac / siglas_ac).
WFS_CAPA_AREAS_CONSERVACION <- "PNE:areas_conservacion"
WFS_CAPA_HUMEDALES          <- "PNE:registro_nacional_humedales"

# Tolerancia de simplificación (m) para las capas que van embebidas en HTML o
# se dibujan a escala nacional. A ~430 m/px de los productos nacionales, 100 m
# es imperceptible y reduce el litoral 1:5000 a un peso manejable.
TOLERANCIA_WEB_M <- 100

# --- NASA FIRMS ---
FIRMS_BASE <- "https://firms.modaps.eosdis.nasa.gov/api"

# Tamaño de fragmento de descarga (días por solicitud). El API de área admite
# rangos pequeños por solicitud (la documentación actual indica 1-5 días).
# NO cambiar una vez iniciada la descarga: la rejilla de fragmentos depende de
# este valor y cambiarlo invalida la caché completa.
FIRMS_DIAS_FRAGMENTO <- 5L

# Origen fijo de la rejilla de fragmentos (coincide con el inicio del registro
# MODIS_SP, la fuente más antigua). La rejilla es COMÚN a todas las fuentes:
# los límites de fragmento se calculan como ORIGEN_GRILLA + k * FIRMS_DIAS_FRAGMENTO,
# independientes del rango solicitado, y clamp_rango() recorta el arranque de
# las fuentes más recientes (p. ej. VIIRS_SNPP_SP desde 2012) al primer
# fragmento que las contiene. Ampliar el rango solo agrega fragmentos en los
# extremos sin invalidar los ya descargados.
ORIGEN_GRILLA <- as.Date("2000-11-01")

# Buffer (km) alrededor del país para el bbox de descarga: cubre de sobra la
# geolocalización de los sensores (~1 km en MODIS, ~375 m en VIIRS) para
# capturar detecciones de borde; el análisis recorta estrictamente al polígono.
FIRMS_BUFFER_KM <- 5

# El área quemada (BA_MODIS/BA_VIIRS) NO se obtiene de FIRMS: su API acepta
# esas colecciones pero responde vacío siempre; se usan los productos
# originales MCD64A1 y VNP64A1 (ver abajo).

# Clave del API de FIRMS, desde .Renviron (no versionado; ver .Renviron.example)
firms_map_key <- function() {
  clave <- Sys.getenv("FIRMS_MAP_KEY")
  if (!nzchar(clave)) {
    stop("Falta FIRMS_MAP_KEY. Copie .Renviron.example a .Renviron e ingrese su clave ",
         "(se solicita en https://firms.modaps.eosdis.nasa.gov/api/map_key/).",
         call. = FALSE)
  }
  clave
}

# --- LP DAAC (área quemada: MCD64A1 y VNP64A1) ---
# Búsqueda de granulos en el catálogo CMR de NASA (pública, sin credenciales).
CMR_BASE <- "https://cmr.earthdata.nasa.gov/search"

MCD64A1_SHORT_NAME <- "MCD64A1"
MCD64A1_VERSION    <- "061"

# Heredero de MCD64A1 derivado de VIIRS S-NPP (registro desde 2012-03); mismo
# algoritmo, rejilla sinusoidal y formato HDF4. Verificado contra CMR el
# 2026-08-04: versión "002", descargas .hdf en lp-prod-protected.
VNP64A1_SHORT_NAME <- "VNP64A1"
VNP64A1_VERSION    <- "002"

# Teselas de la rejilla sinusoidal MODIS que cubren Costa Rica continental:
# h09v07 (10-20 N) para la mitad norte y h09v08 (0-10 N) para la mitad sur.
# La rejilla es común a MCD64A1 y VNP64A1. extraer_quemas() verifica en tiempo
# de ejecución que la UNIÓN de las teselas cubra el país; un granulo individual
# legítimamente no lo cubre.
TESELAS_SINUSOIDALES <- c("h09v07", "h09v08")

# Token de Earthdata Login, desde .Renviron (no versionado). A diferencia de
# la MAP_KEY de FIRMS, los tokens de Earthdata expiran (~60 días).
earthdata_token <- function() {
  token <- Sys.getenv("EARTHDATA_TOKEN")
  if (!nzchar(token)) {
    stop("Falta EARTHDATA_TOKEN. Genere un token en https://urs.earthdata.nasa.gov/ ",
         "(Generate Token) y agréguelo a .Renviron (ver .Renviron.example).",
         call. = FALSE)
  }
  token
}
