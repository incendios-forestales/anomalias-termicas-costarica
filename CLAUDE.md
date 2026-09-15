# anomalias-termicas-costarica

Pipeline {targets} en R que monitorea anomalías térmicas (NASA FIRMS) y área
quemada (MCD64A1/VNP64A1) en Costa Rica continental y publica el sitio en
GitHub Pages desde la raíz de `main`. Generalización nacional de
`../anomalias-termicas-paloverde` (misma arquitectura; leer su README y el de
este proyecto antes de tocar el diseño).

## Ejecución

Todo corre en Docker (`rocker/geospatial` + renv). SIEMPRE como UID 1000 —
como root, renv enlaza los paquetes a `/root/.cache`, que muere con el
contenedor y deja `renv/library` con symlinks rotos:

```bash
docker compose run --rm --user 1000:1000 -e HOME=/home/rstudio rstudio \
  Rscript -e "targets::tar_make(reporter = 'balanced')"
```

Pruebas unitarias (testthat, datos sintéticos; cubren R/temporada.R y
R/grilla.R):

```bash
docker compose run --rm --user 1000:1000 -e HOME=/home/rstudio rstudio \
  Rscript -e "targets::tar_source('R'); testthat::test_dir('tests/testthat')"
```

Para renderizar un reporte fuera del pipeline hay que fijar el directorio de
ejecución en la raíz (las rutas de los targets son relativas a ella):
`quarto::quarto_render("analysis/modis.qmd", execute_dir = getwd())`.

Credenciales en `.Renviron` (no versionado): `FIRMS_MAP_KEY` y
`EARTHDATA_TOKEN` (este expira ~60 días; HTTP 401 en LP DAAC = regenerarlo).

La descarga es idempotente y reanudable (caché en `data/raw/`, ~1,8 GB);
interrumpir y relanzar `tar_make()` es seguro. Una corrida incremental típica
(unos días de FIRMS nuevos, sin gránulos nuevos de área quemada) toma ~15-20
minutos, casi todo en regenerar videos y reportes Quarto de las plataformas
con detecciones nuevas (corrida del 2026-08-27: 16 min, 113 targets
recomputados, 4 778 saltados); la histórica completa desde cero, ~2 horas.

## Publicación

`tar_make()` regenera `index.html`, `{modis,snpp,noaa20,noaa21}/index.html`
y `comparacion/index.html`.
Publicar = commit + push de esos productos y de `outputs/` a `main` (mensaje
tipo «Corrida del AAAA-MM-DD», separado de cambios de código). Pages sirve
`main` raíz: <https://incendios-forestales.github.io/anomalias-termicas-costarica/>

## Invariantes de diseño (no romper)

- Las series de las 4 plataformas NUNCA se suman ni empalman.
- Ámbito continental sin Isla del Coco (decisión de alcance; ver README).
- Área quemada: ramas de targets por GRÁNULO (mes × tesela, h09v07+h09v08);
  ramas mensuales perderían una tesela en silencio.
- `id_deteccion` (asignado en `a_sf_puntos()`) es la llave entre detecciones,
  cobertura, humedales y eventos; nada debe depender de posiciones de fila.
- Rótulos visibles: solo desde `PLATAFORMAS` (R/plataformas.R) y las
  constantes `AREA_*` (R/constantes.R); nunca codificados en funciones.
- `FIRMS_DIAS_FRAGMENTO` y `ORIGEN_GRILLA` no se cambian: invalidan toda la
  caché de descarga.
- Dimensiones de video/MP4 siempre pares (libx264); ya lo garantizan
  `layout_video()` y `animar_detecciones()`.
- `terraOptions(progress = 0)` en constantes.R: sin él, las barras de
  progreso de terra aparecen como texto en los reportes Quarto.

## Suite de índices de temporada (README, «Año de fuego e índices anuales»)

Se construye índice por índice, MODIS primero, y el README es el contrato:
cada índice se define ahí (con sustento y referencias verificadas en
Crossref) ANTES de escribir código, en un commit de documentación aparte.
Luego: funciones genéricas en R/temporada.R, pruebas sintéticas, targets
en el segundo `tar_map` de `_targets.R` (plataformas con periodo base:
MODIS, S-NPP y NOAA-20; NOAA-21 queda fuera hasta tener procesamiento
estándar), sección en analysis/{modis,snpp,noaa20}.qmd, commit de código,
corrida y commit «Corrida del AAAA-MM-DD (<índice>)». Dentro de ese
`tar_map`, un target no puede llamarse como una función que invoque
(`serie_diaria_plat` existe por eso).

Invariantes propios de la suite (cambiarlos invalida índices publicados):

- Año de fuego del 1 de septiembre al 31 de agosto, nombrado por el año en
  que termina (`MES_INICIO_ANIO_FUEGO`); INI/FIN al 10 %/90 % acumulado.
- Índices solo con detecciones de vegetación (`type` 0 o ausente); las
  series publicadas siguen con todos los tipos. Sin umbral de confianza.
- Grilla común: celda base de 0,05° con bordes en múltiplos de 0,05° (CHIRPS)
  y celda de análisis de 0,1° CENTRADA en los nodos de ERA5-Land, con
  `celda_id` por esquina suroeste. Nunca remuestrear a otra grilla.
- Dos periodos: los índices por fracciones (LON, FUERA, N50F) usan todos los
  años completos y no provisionales; los que dependen del conteo (FREC,
  DENS, FRPI, AQ, P95) usan el periodo base de `PLATAFORMAS`
  (`base_inicio`/`base_fin`; MODIS 2003–2022).
- Nunca mezclar detecciones de plataformas distintas en un índice o ráster;
  las comparaciones entre plataformas son productos aparte.
- El control AQ es la fracción de `satelite_control` (columna de
  `PLATAFORMAS`: Aqua en MODIS, NA en las VIIRS). Con NA, AQ queda en NA en
  tablas y ráster, no hay mapa de ciclo diurno y ningún año se marca no
  comparable. Periodos base VIIRS: S-NPP 2013–2025, NOAA-20 2019–2025.
- Umbrales: 300 detecciones/año (nacional), 30/celda (LON, FRPI, AQ),
  100/celda (N50F), 10 km² de tierra (FREC, DENS), 25 % fuera de dic–may
  (celda sin estación definida).
- Comparación entre plataformas (R/comparacion.R, README «Comparación entre
  plataformas en el traslape»): tercer `tar_map` sobre `pares_comparacion()`
  (pares ordenados A→B de las plataformas con periodo base; diferencias
  siempre B − A; traslape = intersección de periodos de referencia). Es el
  ÚNICO sitio donde dos plataformas comparten un producto, y solo lado a
  lado; los consolidados se recalculan sobre el traslape y no alimentan nada
  de las plataformas. Reporte propio en analysis/comparacion.qmd →
  `comparacion/`. `tar_source()` carga comparacion.R antes que
  constantes.R: nada de nivel superior ahí puede usar constantes de otros
  archivos (por eso `colores_par()` es función).
- `terra::metags()` descarta TODAS las etiquetas si un valor contiene «=»;
  los joins de dplyr sobre sf fallan en los qmd (sf no está cargado): unir
  sin geometría y volver a pegar con `st_sf()`.
