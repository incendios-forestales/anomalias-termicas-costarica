# Pipeline de anomalías térmicas y área quemada en Costa Rica (continental).
#
# Cuatro plataformas satelitales HERMANAS —MODIS, VIIRS S-NPP, VIIRS NOAA-20 y
# VIIRS NOAA-21— con juegos de productos equivalentes e independientes. Sus
# detecciones NUNCA se suman: cada sensor y cada plataforma observa con
# resolución y hora de paso distintas, así que una serie combinada mostraría
# saltos que reflejan el instrumental disponible y no el régimen de fuego.
#
# Ejecución:      targets::tar_make()
# Grafo:          targets::tar_visnetwork()
# Estado:         targets::tar_outdated()
# Targets:        targets::tar_manifest(fields = "name")
#
# CÓMO LEER ESTE ARCHIVO
# Las cuatro cadenas se generan con tarchetypes::tar_map a partir de
# PLATAFORMAS (R/plataformas.R): cada target del bloque `tar_map` existe
# cuatro veces, con el sufijo de la clave de plataforma (_modis, _snpp,
# _noaa20, _noaa21). Es decir, `firms_pais` no existe como tal; existen
# `firms_pais_modis`, `firms_pais_snpp`, etc.
#
# La descarga es reanudable: cada fragmento de fechas es una rama dinámica
# respaldada por un CSV en data/raw/firms/; si la ejecución se interrumpe,
# volver a correr tar_make() continúa donde quedó.

library(targets)
library(tarchetypes)

tar_source("R")

tar_option_set(
  packages = c(
    "sf", "dplyr", "tidyr", "purrr", "readr", "lubridate", "glue", "httr2",
    "ggplot2", "gganimate", "gifski", "av", "plotly", "leaflet",
    "leaflet.extras2", "leafgl", "yyjsonr", "htmlwidgets", "DT", "here",
    "quarto", "terra", "exactextractr", "tibble"
  ),
  format = "rds"
)

# Valores del map. `quemas_origen` apunta al target del PRODUCTO de área
# quemada que le corresponde a cada plataforma: MODIS y S-NPP tienen el suyo;
# NOAA-20 y NOAA-21 toman el de S-NPP porque no existe uno propio.
plataformas_pipeline <- dplyr::mutate(
  PLATAFORMAS[, c("clave", "etiqueta", "ba_producto")],
  quemas_origen = rlang::syms(paste0("quemas_", tolower(ba_producto))),
  ultimo_ba     = rlang::syms(paste0("ultimo_mes_", tolower(ba_producto)))
)
plataformas_pipeline$ba_producto <- NULL

list(
  # --- Parámetros del pipeline (editar aquí) -------------------------------
  # El rango se recorta automáticamente al disponible en FIRMS, por lo que
  # una fecha_fin lejana significa "hasta lo más reciente disponible".
  tar_target(fecha_inicio, as.Date("2001-01-01")),
  tar_target(fecha_fin,    as.Date("2100-01-01")),

  # --- Contexto compartido por las cuatro plataformas ----------------------
  # Nada de esto depende del sensor: el límite nacional (unión de provincias
  # IGN 1:5000, sin Isla del Coco), las capas del SINAC, la cobertura de la
  # tierra y el relieve del video. `pais` conserva el detalle 1:5000 para los
  # recortes; `pais_web` (WGS84) y `pais_mapa` (CRTM05) son la versión
  # simplificada para productos web y figuras a escala nacional.
  tar_target(archivo_provincias,
             descargar_provincias_wfs("data/raw/wfs/provincias.gpkg"),
             format = "file"),
  tar_target(pais, construir_pais(archivo_provincias)),
  tar_target(pais_web, pais_para_web(pais)),
  tar_target(pais_mapa, a_crtm05(pais_web)),
  tar_target(bbox_descarga, bbox_con_buffer(pais)),
  tar_target(bbox_pais, sf::st_bbox(pais)),
  tar_target(archivos_worldcover, descargar_worldcover(bbox_descarga),
             format = "file"),
  tar_target(archivo_humedales,
             descargar_capa_wfs(WFS_CAPA_HUMEDALES, bbox_pais,
                                "data/raw/wfs/humedales.gpkg"),
             format = "file"),
  tar_target(archivo_ac,
             descargar_capa_wfs(WFS_CAPA_AREAS_CONSERVACION, bbox_pais,
                                "data/raw/wfs/areas_conservacion.gpkg"),
             format = "file"),
  tar_target(humedales, sf::st_read(archivo_humedales, quiet = TRUE)),
  tar_target(areas_conservacion, sf::st_read(archivo_ac, quiet = TRUE)),
  tar_target(ac_web, simplificar_para_web(preparar_ac(areas_conservacion))),
  tar_target(paisaje_pais,
             composicion_paisaje(pais, archivos_worldcover, bbox_descarga)),
  tar_target(archivos_dem, descargar_dem_terrarium(bbox_descarga),
             format = "file"),
  tar_target(relieve_video, fondo_relieve_video(archivos_dem, pais_mapa,
                                                bbox_descarga,
                                                archivos_worldcover)),

  # --- Área quemada, por PRODUCTO y no por plataforma ----------------------
  # Hay exactamente dos productos y son un recurso compartido: VNP64A1
  # alimenta a tres plataformas. Nombrarlos por producto evita repetir la
  # consulta al catálogo CMR (que corre en cada ejecución) una vez por
  # plataforma. Costa Rica cruza dos teselas sinusoidales (h09v07/h09v08),
  # así que cada rama dinámica es UN GRANULO (mes x tesela), no un mes:
  # con ramas mensuales de dos filas, descargar_granulo_ba() perdería la
  # segunda tesela en silencio.
  tar_target(granulos_mcd64a1,
             cmr_granulos_ba(fecha_inicio, fecha_fin,
                             MCD64A1_SHORT_NAME, MCD64A1_VERSION),
             cue = tar_cue(mode = "always"), iteration = "group"),
  tar_target(hdf_mcd64a1,
             descargar_granulo_ba(granulos_mcd64a1, "data/raw/mcd64a1"),
             pattern = map(granulos_mcd64a1), format = "file"),
  tar_target(quemas_mcd64a1, extraer_quemas(hdf_mcd64a1, pais)),
  # Frontera de PUBLICACIÓN del producto, tomada de la lista de granulos y no
  # de los píxeles: un mes publicado sin fuego en el país no aporta píxeles
  # y correría la frontera hacia atrás.
  tar_target(ultimo_mes_mcd64a1, max(granulos_mcd64a1$aniomes)),
  tar_target(granulos_vnp64a1,
             cmr_granulos_ba(fecha_inicio, fecha_fin,
                             VNP64A1_SHORT_NAME, VNP64A1_VERSION),
             cue = tar_cue(mode = "always"), iteration = "group"),
  tar_target(hdf_vnp64a1,
             descargar_granulo_ba(granulos_vnp64a1, "data/raw/vnp64a1"),
             pattern = map(granulos_vnp64a1), format = "file"),
  tar_target(quemas_vnp64a1, extraer_quemas(hdf_vnp64a1, pais)),
  tar_target(ultimo_mes_vnp64a1, max(granulos_vnp64a1$aniomes)),

  # --- Una cadena por plataforma -------------------------------------------
  # Todo lo que sigue existe cuatro veces, con el sufijo de la clave. El área
  # quemada se recorta al período observado por la plataforma: sin ese
  # recorte, una plataforma que empieza en 2018 mostraría superficie quemada
  # de 2012, años sin detecciones, sugiriendo un vacío de detección
  # inexistente.
  tar_map(
    values = plataformas_pipeline,
    names = clave,
    descriptions = etiqueta,

    tar_target(etiquetas, etiquetas_plataforma(clave)),
    tar_target(rangos, rangos_plataforma(clave, fecha_inicio, fecha_fin),
               cue = tar_cue(mode = "always")),
    tar_target(fragmentos, construir_fragmentos_multi(rangos),
               iteration = "group"),
    tar_target(csv_fragmentos,
               descargar_firms_fragmento(fragmentos, bbox_descarga),
               pattern = map(fragmentos), format = "file"),
    tar_target(firms_crudo,   leer_y_unir_csv(csv_fragmentos, rangos)),
    tar_target(firms_puntos,  a_sf_puntos(firms_crudo)),
    tar_target(firms_pais,    recortar_al_area(firms_puntos, pais)),
    tar_target(firms_mensual, agregar_mensual(firms_pais, rangos)),
    tar_target(cobertura, extraer_cobertura(firms_pais, archivos_worldcover)),
    tar_target(humedales_detecciones,
               cruzar_con_humedales(firms_pais, humedales)),
    tar_target(ac_detecciones,
               detecciones_por_ac(firms_pais, areas_conservacion)),
    tar_target(area_quemada_pais,
               recortar_quemas_al_periodo(quemas_origen, firms_mensual)),
    tar_target(area_quemada_mensual,
               agregar_mensual_quemas(area_quemada_pais,
                                      rango_meses = range(firms_mensual$aniomes),
                                      hasta = ultimo_ba)),
    tar_target(ac_quemas,
               quemas_por_ac(area_quemada_pais, areas_conservacion)),
    tar_target(cobertura_quemas,
               extraer_cobertura_quemas(area_quemada_pais,
                                        archivos_worldcover)),
    tar_target(humedales_quemas,
               cruzar_quemas_con_humedales(area_quemada_pais, humedales)),

    tar_target(cartel_resumen,
               generar_cartel_resumen(firms_mensual, area_quemada_mensual,
                                      file.path("outputs/figs", clave,
                                                "cartel_resumen.png"),
                                      etiquetas$corta, etiquetas$ids_fuente,
                                      etiquetas$etiqueta_ba,
                                      etiquetas$fuentes_video),
               format = "file"),
    tar_target(video_anomalias,
               generar_video_anomalias(firms_pais, area_quemada_pais,
                                       firms_mensual, area_quemada_mensual,
                                       pais_mapa, relieve_video,
                                       file.path("outputs/figs", clave,
                                                 "video_anomalias_termicas.mp4"),
                                       etiquetas$corta, etiquetas$ids_fuente,
                                       etiquetas$etiqueta_ba,
                                       etiquetas$fuentes_video, fps = 5),
               format = "file"),
    tar_target(anim_gif,
               animar_detecciones(firms_pais, pais_mapa, firms_mensual,
                                  archivos_worldcover, bbox_descarga,
                                  file.path("outputs/figs", clave,
                                            "animacion_mensual.gif"),
                                  etiquetas$fuente_fig, etiquetas$pie_animacion),
               format = "file"),
    tar_target(anim_mp4,
               animar_detecciones(firms_pais, pais_mapa, firms_mensual,
                                  archivos_worldcover, bbox_descarga,
                                  file.path("outputs/figs", clave,
                                            "animacion_mensual.mp4"),
                                  etiquetas$fuente_fig, etiquetas$pie_animacion),
               format = "file"),
    tar_target(mapa_html,
               mapa_leaflet_temporal(firms_pais, pais_web, cobertura,
                                     archivos_worldcover, bbox_descarga,
                                     ac_web,
                                     file.path("outputs/maps", clave,
                                               "mapa_temporal.html"),
                                     etiquetas$etiqueta_quemas,
                                     quemas = area_quemada_pais),
               format = "file"),
    tar_target(fig_serie,
               grafico_serie_temporal(firms_mensual,
                                      file.path("outputs/figs", clave,
                                                "serie_mensual.png"),
                                      etiquetas$fuente_fig, etiquetas$pie_firms),
               format = "file"),
    tar_target(fig_climatologia,
               grafico_climatologia(firms_mensual,
                                    file.path("outputs/figs", clave,
                                              "climatologia_mensual.png"),
                                    etiquetas$fuente_fig, etiquetas$pie_firms),
               format = "file"),
    tar_target(fig_serie_html,
               grafico_serie_html(firms_mensual,
                                  file.path("outputs/figs", clave,
                                            "serie_mensual.html"),
                                  etiquetas$fuente_fig, etiquetas$pie_firms),
               format = "file"),
    tar_target(fig_climatologia_html,
               grafico_climatologia_html(firms_mensual,
                                         file.path("outputs/figs", clave,
                                                   "climatologia_mensual.html"),
                                         etiquetas$fuente_fig,
                                         etiquetas$pie_firms),
               format = "file"),
    tar_target(fig_area_quemada,
               grafico_area_quemada(area_quemada_mensual,
                                    file.path("outputs/figs", clave,
                                              "area_quemada_mensual.png"),
                                    etiquetas$etiqueta_ba, etiquetas$pie_ba),
               format = "file"),
    tar_target(fig_comparacion,
               grafico_comparacion(firms_mensual, area_quemada_mensual,
                                   file.path("outputs/figs", clave,
                                             "detecciones_vs_area_quemada.png"),
                                   etiquetas$etiqueta_ba, etiquetas$pie_ambos),
               format = "file"),
    tar_target(fig_cobertura_quemas,
               grafico_cobertura_quemas(cobertura_quemas, humedales_quemas,
                                        file.path("outputs/figs", clave,
                                                  "cobertura_area_quemada.png"),
                                        etiquetas$pie_cobertura_ba),
               format = "file"),
    tar_target(fig_climatologia_comparada,
               grafico_climatologia_comparada(firms_mensual,
                                              area_quemada_mensual,
                                              file.path("outputs/figs", clave,
                                                        "climatologia_comparada.png"),
                                              etiquetas$etiqueta_ba,
                                              etiquetas$pie_ambos),
               format = "file"),
    tar_target(fig_cobertura,
               grafico_cobertura(cobertura, humedales_detecciones,
                                 file.path("outputs/figs", clave,
                                           "cobertura_detecciones.png"),
                                 etiquetas$pie_cobertura),
               format = "file"),
    tar_target(fig_ac,
               grafico_ac(ac_detecciones, ac_quemas,
                          file.path("outputs/figs", clave,
                                    "detecciones_por_ac.png"),
                          etiquetas$pie_ambos),
               format = "file"),
    # Mapa del incendio del humedal Catalina (mayo-junio de 2026), el único
    # evento del registro con una cifra oficial de superficie contra la cual
    # contrastar el producto de área quemada. Ver R/evento.R.
    tar_target(fig_evento,
               grafico_evento("catalina_2026", firms_pais,
                              area_quemada_pais, pais_mapa, humedales,
                              archivos_worldcover, bbox_descarga,
                              file.path("outputs/figs", clave,
                                        "evento_catalina_2026.png"),
                              etiquetas$fuente_fig, etiquetas$etiqueta_ba,
                              etiquetas$pie_animacion),
               format = "file"),
    tar_target(tabla_area_quemada,
               tabla_area_quemada_csv(area_quemada_pais,
                                      file.path("outputs/tables", clave,
                                                "area_quemada_anual.csv")),
               format = "file"),
    tar_target(tabla_cobertura,
               tabla_cobertura_csv(cobertura,
                                   file.path("outputs/tables", clave,
                                             "cobertura_detecciones.csv")),
               format = "file"),
    tar_target(tabla_contraste,
               tabla_contraste_csv(cobertura, humedales_detecciones,
                                   cobertura_quemas, humedales_quemas,
                                   file.path("outputs/tables", clave,
                                             "contraste_humedales.csv")),
               format = "file"),
    tar_target(tabla_ac,
               tabla_ac_csv(ac_detecciones, ac_quemas,
                            file.path("outputs/tables", clave,
                                      "detecciones_por_ac.csv")),
               format = "file"),
    tar_target(tabla_csv,
               tabla_resumen_csv(firms_pais, area_quemada_pais,
                                 file.path("outputs/tables", clave,
                                           "resumen_anual.csv")),
               format = "file"),
    tar_target(tabla_html,
               tabla_resumen_html(firms_pais, area_quemada_pais,
                                  file.path("outputs/tables", clave,
                                            "resumen_anual.html"),
                                  etiquetas$fuente_fig, etiquetas$etiqueta_ba),
               format = "file")
  ),

  # --- Año de fuego e índices anuales de temporada: solo MODIS --------------
  # Primer índice de la suite (README, «Año de fuego e índices anuales»):
  # longitud de la temporada (LON) con INI y FIN. Se conecta solo para MODIS
  # en esta entrega, la serie con 25 años y dos satélites; las funciones son
  # genéricas (R/temporada.R) y extender a VIIRS es mover estos targets al
  # tar_map. Los índices usan solo detecciones de vegetación; las series
  # publicadas siguen incluyendo todos los tipos.
  tar_target(firms_vegetacion_modis, filtrar_vegetacion(firms_pais_modis)),
  tar_target(tipos_modis, resumen_tipos(firms_pais_modis)),
  tar_target(serie_diaria_modis,
             serie_diaria(firms_vegetacion_modis, rangos_modis)),
  tar_target(temporada_modis,
             indices_temporada(serie_diaria_modis, rangos_modis)),
  tar_target(tabla_temporada_modis,
             tabla_temporada_csv(temporada_modis,
                                 "outputs/tables/modis/temporada_anual.csv"),
             format = "file"),
  tar_target(tabla_tipos_modis,
             tabla_tipos_csv(tipos_modis,
                             "outputs/tables/modis/detecciones_por_tipo.csv"),
             format = "file"),
  tar_target(fig_temporada_modis,
             grafico_temporada(temporada_modis,
                               "outputs/figs/modis/temporada_anual.png",
                               etiquetas_modis$fuente_fig,
                               etiquetas_modis$pie_firms),
             format = "file"),

  # --- Figura comparativa de las cuatro plataformas ------------------------
  tar_target(fig_series_plataformas,
             grafico_series_plataformas(
               list(firms_mensual_modis, firms_mensual_snpp,
                    firms_mensual_noaa20, firms_mensual_noaa21),
               list(etiquetas_modis, etiquetas_snpp,
                    etiquetas_noaa20, etiquetas_noaa21),
               "outputs/figs/series_plataformas.png"),
             format = "file"),

  # --- Reportes Quarto ------------------------------------------------------
  # Se escriben explícitamente: son el elemento menos uniforme del proyecto
  # (su prosa es distinta por definición) y son pocos. tar_quarto solo detecta
  # como dependencias los targets leídos con tar_read() dentro del qmd, no las
  # funciones de R/ que el qmd llama vía tar_source(); de ahí `extra_files`.
  tar_quarto(reporte_modis, "analysis/modis.qmd",
             extra_files = list.files("R", pattern = "[.][Rr]$",
                                      full.names = TRUE)),
  tar_target(pagina_modis, {
    reporte_modis
    dir.create("modis", showWarnings = FALSE)
    file.copy("analysis/modis.html", "modis/index.html", overwrite = TRUE)
    "modis/index.html"
  }, format = "file"),
  tar_quarto(reporte_snpp, "analysis/snpp.qmd",
             extra_files = list.files("R", pattern = "[.][Rr]$",
                                      full.names = TRUE)),
  tar_target(pagina_snpp, {
    reporte_snpp
    dir.create("snpp", showWarnings = FALSE)
    file.copy("analysis/snpp.html", "snpp/index.html", overwrite = TRUE)
    "snpp/index.html"
  }, format = "file"),
  tar_quarto(reporte_noaa20, "analysis/noaa20.qmd",
             extra_files = list.files("R", pattern = "[.][Rr]$",
                                      full.names = TRUE)),
  tar_target(pagina_noaa20, {
    reporte_noaa20
    dir.create("noaa20", showWarnings = FALSE)
    file.copy("analysis/noaa20.html", "noaa20/index.html", overwrite = TRUE)
    "noaa20/index.html"
  }, format = "file"),
  tar_quarto(reporte_noaa21, "analysis/noaa21.qmd",
             extra_files = list.files("R", pattern = "[.][Rr]$",
                                      full.names = TRUE)),
  tar_target(pagina_noaa21, {
    reporte_noaa21
    dir.create("noaa21", showWarnings = FALSE)
    file.copy("analysis/noaa21.html", "noaa21/index.html", overwrite = TRUE)
    "noaa21/index.html"
  }, format = "file"),

  # --- Portada: entrada común en la raíz del sitio -------------------------
  # Depende de los targets de las cuatro plataformas porque calcula su tabla
  # comparativa leyéndolos.
  tar_quarto(portada, "analysis/portada.qmd",
             extra_files = list.files("R", pattern = "[.][Rr]$",
                                      full.names = TRUE)),
  tar_target(pagina_portada, {
    portada
    file.copy("analysis/portada.html", "index.html", overwrite = TRUE)
    "index.html"
  }, format = "file")
)
