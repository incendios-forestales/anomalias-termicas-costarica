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

Pruebas unitarias (testthat, datos sintéticos; hoy cubren R/temporada.R):

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

`tar_make()` regenera `index.html` y `{modis,snpp,noaa20,noaa21}/index.html`.
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
