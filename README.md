# Anomalías térmicas en Costa Rica

Pipeline reproducible en R que descarga las anomalías térmicas (detecciones de
fuego activo) de [NASA FIRMS](https://firms.modaps.eosdis.nasa.gov/) y el
**área quemada** mensual de NASA LP DAAC para el territorio continental de
**Costa Rica**, caracteriza la **cobertura de la tierra** en la que ocurren,
las desagrega por **área de conservación** del SINAC y genera un **mapa
animado a través del tiempo**, un mapa interactivo con deslizador temporal,
gráficos estadísticos interactivos y tablas.

Es la generalización a escala nacional del pipeline de
[anomalias-termicas-paloverde](https://github.com/incendios-forestales/anomalias-termicas-paloverde),
que monitorea el Parque Nacional Palo Verde con la misma arquitectura.

Cada sensor se procesa por separado y tiene su propio juego de productos:

| Plataforma | Fuego activo (FIRMS) | Área quemada | Registro | Reporte |
|---|---|---|---|---|
| MODIS (Terra/Aqua, ~1 km) | `MODIS_SP` + `MODIS_NRT` | MCD64A1 v6.1 | desde 2001 | [modis/](https://incendios-forestales.github.io/anomalias-termicas-costarica/modis/) |
| VIIRS (Suomi-NPP, 375 m) | `VIIRS_SNPP_SP` + `_NRT` | VNP64A1 v002 | desde 2012 | [snpp/](https://incendios-forestales.github.io/anomalias-termicas-costarica/snpp/) |
| VIIRS (NOAA-20, 375 m) | `VIIRS_NOAA20_SP` + `_NRT` | VNP64A1 (de S-NPP)¹ | desde 2018 | [noaa20/](https://incendios-forestales.github.io/anomalias-termicas-costarica/noaa20/) |
| VIIRS (NOAA-21, 375 m) | `VIIRS_NOAA21_NRT`² | VNP64A1 (de S-NPP)¹ | desde 2024 | [noaa21/](https://incendios-forestales.github.io/anomalias-termicas-costarica/noaa21/) |

¹ NOAA-20 y NOAA-21 no tienen producto de área quemada propio (`VJ164A1` no
está publicado en CMR), así que toman el de Suomi-NPP, recortado a su ventana
temporal y **rotulado como tal** en toda figura, tabla y leyenda.
² NOAA-21 es la única plataforma **sin procesamiento estándar**: todos sus
datos son en tiempo casi real y por tanto provisionales.

Cada serie se sirve de su procesamiento estándar y, a continuación, de su cola
en tiempo casi real, de modo que llega hasta hace pocos días; ese tramo va
marcado como provisional en todos los productos. Las series **no se suman ni
se empalman** entre plataformas: más resolución detecta más fuegos y más
satélites observan más veces, así que una serie combinada mostraría saltos
—en 2012, 2018 y 2024— que reflejarían el instrumental disponible y no el
régimen de fuego.

**La portada del sitio** presenta las cuatro y reúne los hallazgos comunes:
<https://incendios-forestales.github.io/anomalias-termicas-costarica/>

Tener cuatro series paralelas sirve para tres cosas: **continuidad** (Terra y
Aqua terminan en 2027 y las plataformas VIIRS son su relevo), **contraste**
—cuando cuatro instrumentos independientes coinciden, el hallazgo no es
artefacto de ninguno— y **cobertura reciente** gracias a las colas en tiempo
casi real.

## Ámbito territorial

El área de estudio es **Costa Rica continental más las islas cercanas**,
construida como la unión de las siete provincias de la cartografía oficial
1:5000 del IGN (WFS del SNIT). La **Isla del Coco queda excluida**
deliberadamente: su actividad de fuego es prácticamente nula, incluirla
extendería el rectángulo de descarga de FIRMS sobre cientos de kilómetros de
océano —donde VIIRS registra barcos como anomalías térmicas nocturnas— y
queda fuera de la zona de uso práctico de la proyección CRTM05. El recorte al
polígono nacional es estricto: las detecciones del buffer de descarga que
caen en el mar o en países vecinos se descartan.

A escala nacional, además, una **anomalía térmica no es necesariamente un
incendio forestal**: el registro incluye quemas agrícolas (caña, pastos) y
otras fuentes de calor. Los reportes lo advierten.

## Arquitectura

El flujo de trabajo está implementado con [{targets}](https://books.ropensci.org/targets/):

1. **Obtención de datos**
   - Detecciones: API de área de FIRMS, descargada en fragmentos de 5 días.
   - Área quemada: granulos mensuales de MCD64A1 v6.1 y VNP64A1 v002 (500 m)
     desde LP DAAC, descubiertos vía el API CMR de Earthdata. FIRMS lista
     estos productos (`BA_MODIS`, `BA_VIIRS`) pero su API no los entrega como
     datos (responde vacío), por lo que se usan los originales. Costa Rica
     cruza **dos teselas** de la rejilla sinusoidal (h09v07 y h09v08): cada
     rama dinámica de targets es un **gránulo** (mes × tesela), no un mes.
     No existe versión en tiempo casi real del área quemada: el método
     necesita observar la cicatriz durante semanas, así que el tramo más
     reciente de cada serie tiene detecciones pero todavía no superficie.
   - Límite nacional: provincias 1:5000 del IGN (WFS del SNIT), unidas y sin
     la Isla del Coco.
   - Áreas de conservación y Registro Nacional de Humedales: WFS del SINAC.
   - Cobertura de la tierra: teselas de ESA WorldCover 2021 (10 m) desde S3
     (cuatro teselas para el territorio continental).
2. **Procesamiento**: conversión a puntos `sf` con un `id_deteccion` estable,
   recorte estricto al polígono nacional y reproyección a CRTM05
   (EPSG:5367); agregación mensual.
3. **Análisis**: composición de clases de cobertura en el *footprint*
   elíptico de cada detección, contraste con el Registro Nacional de
   Humedales y desagregación por área de conservación (detecciones por punto,
   píxeles de quema por centroide).
4. **Salidas**: animación GIF/MP4 (gganimate) con fondo de cobertura, video
   estilo cartel con relieve y contadores acumulados, mapa leaflet con
   deslizador temporal (ver abajo), series mensuales y climatologías (PNG +
   plotly), gráficos por área de conservación, tablas (CSV y HTML) y
   reportes Quarto autocontenidos con una portada común.

### Descarga idempotente y reanudable

Cada fragmento de fechas es una rama dinámica de targets respaldada por un CSV
en `data/raw/firms/<data_id>/<inicio>_<fin>.csv`:

- Si la ejecución se interrumpe, volver a correr `targets::tar_make()` continúa
  exactamente donde quedó (los CSV existentes no se vuelven a descargar).
- Los límites de los fragmentos están anclados a una rejilla fija
  (2000-11-01 + k·5 días), por lo que **ampliar el rango de fechas solo
  descarga los fragmentos nuevos** sin invalidar los existentes.
- La escritura es atómica (`.part` → renombrar): nunca queda un CSV truncado.
- A escala nacional un fragmento de temporada seca trae miles de filas, así
  que además se verifica que todas las filas tengan el número de campos del
  encabezado antes de cachear.

Las capas de contexto (WFS del SNIT y del SINAC, teselas de WorldCover y del
DEM) también se cachean en `data/raw/` y solo se descargan la primera vez.

### Cobertura de la tierra por *footprint*

Una detección MODIS no es un punto: es un píxel de ~1 km, mayor fuera del
nadir. Asignarle la clase del punto exacto sobre un mapa de 10 m sería
precisión espuria, así que la cobertura se caracteriza sobre el **footprint
completo** —una elipse con las dimensiones reales del píxel, columnas `scan` ×
`track`— y se reporta la fracción por clase y la clase dominante. A escala
nacional (10⁵–10⁶ detecciones) la extracción corre por lotes con
`exactextractr` sobre un VRT de las teselas WorldCover, unida siempre por el
`id_deteccion` estable.

El resultado se contrasta con el Registro Nacional de Humedales del SINAC:
WorldCover clasifica como «pastizal» buena parte de la vegetación herbácea
inundable del país, y el cruce distingue cuánto del fuego «en pastizal»
ocurre en humedales estacionales. Dos advertencias que los reportes
documentan: WorldCover es una foto fija de 2021 frente a un registro
2001–2026, y el registro de humedales asigna una clase por polígono (sirve
para determinar si un sitio es humedal, no qué vegetación ardió).

### Mapa interactivo a escala nacional

Un HTML autocontenido no puede cargar un marcador con ficha por cada una de
los cientos de miles de detecciones del registro. El mapa divide el trabajo:

- el **deslizador temporal** opera sobre los últimos 24 meses, con ficha
  completa por detección y los píxeles de quema de esa misma ventana;
- el **registro completo** va como capa de puntos WebGL
  ([{leafgl}](https://github.com/r-spatial/leafgl)), sin fichas, apagada al
  inicio;
- el fondo de WorldCover se embebe agregado por moda (~250 m) y las capas
  vectoriales (límite nacional, áreas de conservación) van simplificadas a
  100 m.

### Video estilo cartel

`outputs/figs/<plataforma>/video_anomalias_termicas.mp4` resume el registro
en ~1 minuto, un cuadro por mes: detecciones de fuego con resplandor y estela
de los meses recientes, píxeles de área quemada y, por cada serie, el
acumulado desde el inicio con el valor del mes en curso debajo (por separado:
son magnitudes complementarias y no sumables), en un estilo inspirado en los
videos de [Milos Popovic](https://milospopovic.net/).

- **Render cuadro a cuadro** con ggplot2 (PNG numerados ensamblados con el
  paquete `av`), no con gganimate: los contadores, la fecha y el resplandor
  multicapa cambian texto y número de capas en cada cuadro.
- **Encuadre calculado**: el lienzo (1080 px de ancho, ~440 m/px) se deriva
  del bbox nacional; nada del encuadre está codificado a mano.
- **Relieve**: teselas [Terrain Tiles](https://registry.opendata.aws/terrain-tiles/)
  (Mapzen/AWS, formato Terrarium, públicas y sin autenticación) a **zoom 9**
  (~300 m/px, acorde con el lienzo), que codifican la elevación en los
  canales RGB del PNG: `elevación (m) = R·256 + G + B/256 − 32768`. Se
  cachean en `data/raw/dem/` y el hillshade se calcula con terra.
- **Fondo por cobertura**: dentro del país el matiz del fondo viene de la
  clase de WorldCover (paleta oscura propia, no la oficial) y la luminancia
  del hillshade; fuera, una rampa neutra atenuada.

El cartel estático `outputs/figs/<plataforma>/cartel_resumen.png` resume las
dos series mensuales con el mismo estilo visual, en dos paneles apilados —
nunca un doble eje: son magnitudes no comparables — con el mes máximo de
cada serie rotulado.

### Un episodio con contraparte en tierra

Casi nada de lo que mide el proyecto puede contrastarse contra una medición
independiente. La excepción es el episodio del humedal Catalina (PN Palo
Verde) de mayo de 2026: un rayo lo encendió el 28 de mayo y el Minae-Sinac
reportó **cerca de 4000 ha afectadas**, con *Typha* como combustible
principal. Los eventos así se declaran como filas de la tabla `EVENTOS` en
[`R/evento.R`](R/evento.R) y de ahí salen las cifras, el mapa y la prosa de
las cuatro plataformas. Dos resultados: **el píxel manda** (MODIS registró un
orden de magnitud menos detecciones que VIIRS sobre el mismo fuego) y **el
área quemada se queda muy corta** en humedales (el algoritmo exige un cambio
persistente de reflectancia que una quema de *Typha* sobre agua no produce).

## Año de fuego e índices anuales

El proyecto incorpora, paso a paso, **índices anuales de la temporada de
fuego** inspirados en los índices de extremos del ETCCDI para precipitación
(Zhang et al. 2011). Las definiciones se escriben aquí antes que el código y
son el contrato de lo que se calcula. Cada índice se agrega cuando el
anterior está publicado; esta sección documenta el primero, la **longitud
de la temporada**, en su versión anual y en su versión espacial
consolidada, y solo lo necesario para calcularlas. Nada de esta sección
altera las series mensuales, los videos ni los mapas ya publicados.

### Año de fuego

Los índices se calculan por **año de fuego**, del 1 de septiembre al 31 de
agosto, **nombrado por el año calendario en que termina**, que es el año en
que ocurre la temporada. El corte sigue el criterio de Boschetti y Roy
(2008) de situar el inicio del año en el mínimo de actividad, de modo que
ninguna temporada quede partida y el total anual no dependa del mes elegido:
en la serie MODIS 2001–2026 el 94 % de las detecciones ocurre entre enero y
mayo, entre junio y octubre ocurre menos del 2 % y septiembre es el centro
de ese mínimo. El año calendario no cumple la condición porque parte
diciembre (3,5 % de las detecciones, hasta ~100 en un solo diciembre) de la
temporada a la que pertenece.

El año de fuego es un **periodo de cómputo, no una definición de
temporada**, y no choca con las que usan las instituciones nacionales,
porque las contiene enteras:

- El IMN describe la **época seca** del Pacífico como el periodo «que se
  extiende de diciembre a abril en la Vertiente Pacífica» y «en el que se
  concentra la mayor cantidad de incendios forestales» (Villalobos, Retana y
  Acuña, s. f.).
- El SINAC define la **temporada de incendios** como la «época de menor
  precipitación, que comprende los meses de enero a mayo de cada año,
  pudiéndose adelantar o postergar dependiendo del comportamiento climático»
  (SINAC 2012), y reporta sus estadísticas por temporada nombrada con un
  solo año («temporada 2011», «temporadas 1998–2012»).

Así, el año de fuego 2024 contiene la temporada 2024 del SINAC y la época
seca 2023–2024 del IMN. La temporada *observada* de cada año se estima con
los índices `INI` y `FIN` (abajo), que responden con una fecha al
«pudiéndose adelantar o postergar» del SINAC y miden el rezago entre la
estación seca y el fuego.

Dos consecuencias: el año de fuego 2001 está **incompleto** (la serie
empieza el 1 de enero de 2001 y faltan septiembre a diciembre de 2000) y se
marca como parcial; y el año en curso, cubierto en parte por la cola en
tiempo casi real, se marca como **provisional** con el mismo `nivel` de la
serie mensual.

### Detecciones incluidas

Los índices usan solo detecciones de **vegetación**: `type` igual a 0 o
ausente. Se excluyen los tipos 1 (volcán activo: Turrialba, Poás, Rincón de
la Vieja), 2 (otra fuente estática en tierra, típicamente industrial) y 3
(mar, que el recorte al polígono ya elimina). La cola en tiempo casi real de
FIRMS **no trae el campo `type`**, por lo que «ausente» cuenta como
vegetación y en el año provisional pueden colarse fuentes que el
procesamiento estándar sí etiquetaría. Las detecciones excluidas se reportan
en una tabla aparte por tipo y año. Ferreira et al. (2020) documentan el
mismo problema a escala global: volcanes y quemadores de gas producen
«temporadas» anómalamente largas si no se apartan antes de calcular la
estacionalidad.

Las series mensuales, los videos y los mapas publicados siguen incluyendo
todos los tipos: son anomalías térmicas en sentido amplio.

### Serie diaria

Base de los índices: una fila por día del año de fuego con el número de
detecciones, con **ceros explícitos** en los días sin detección, construida
desde `rangos` (lo observado por el satélite) y no desde las fechas con
detecciones, por la misma razón que la serie mensual.

### Primer índice: longitud de la temporada (`LON`)

| Código | Definición | Unidad |
|---|---|---|
| `DTOT` | Detecciones del año de fuego (auxiliar) | n |
| `INI` | Primer día del año de fuego en que la suma acumulada de detecciones alcanza el 10 % de `DTOT` | día 1–366 y fecha |
| `FIN` | Primer día en que alcanza el 90 % | ídem |
| `LON` | `FIN` − `INI` + 1 | días |

Definir inicio y fin como percentiles de la distribución acumulada de
fechas tiene precedente: percentil 5 (10 o 15 en regiones con quemas de
hombro de temporada) para el inicio de la temporada en California (Science
Advances 2025), intervalo 5–95 como longitud de temporada para evitar que
un fuego aislado la infle (Frontiers in Forests and Global Change 2024), y
los meses que contienen el 80 % central del área quemada en Archibald et
al. (2013). Aquí se usa 10–90 y no 5–95 porque con 700–2000 detecciones
anuales el 5 % son unas pocas decenas y la fecha saltaría con un solo día
de quemas agrícolas. Los años con `DTOT` < 300 se marcan para que su
temporalidad no se interprete.

Al depender de fracciones del total anual y no del conteo absoluto, `LON`
es insensible a los cambios de detectabilidad de la serie MODIS: los años
2001 y 2002 con solo Terra, y la deriva de las horas de paso de Terra y
Aqua desde 2020 y 2022. Los índices por percentil, que sí dependen del
conteo, vendrán después con un periodo base explícito.

Una definición alternativa sin umbrales, el mínimo y máximo de la anomalía
diaria acumulada (Liebmann et al. 2012; Dunning et al. 2016 para regímenes
bimodales), queda como comprobación futura.

### Ráster consolidado de `LON`

La versión espacial del índice responde a dónde empieza, termina y cuánto
dura la temporada, celda por celda. Es un producto **consolidado**: para
cada celda se juntan las detecciones de vegetación de todos los años de
fuego del periodo de referencia, se toma su distribución en días del año
de fuego y se calculan `INI`, `FIN` y `LON` con las mismas fracciones del
10 % y el 90 %. Es la temporada climatológica de la celda, el producto
estándar de la pirogeografía (Giglio et al. 2006; Benali et al. 2017), y
**no es el promedio de los `LON` anuales**: la distribución agrupada
incluye la variabilidad entre años, así que es sistemáticamente más ancha.

**No hay rásteres anuales de `LON`.** Con MODIS, a 0,1° solo el 1 % de las
celda-año alcanza 30 detecciones, y a 0,25° el 12 %; con VIIRS S-NPP, a
0,1° el 12 %. `INI` y `FIN` por celda y año dependerían de un puñado de
días en casi todo el país. La dimensión interanual espacial corresponde a
las áreas de conservación, con cientos de detecciones por año en las del
Pacífico.

**Periodo de referencia**: los años de fuego completos y no provisionales
del registro; para MODIS, 2002–2025. Es un parámetro y no una constante,
para poder calcular el consolidado de MODIS sobre el periodo de otra
plataforma (2013–2025 para S-NPP) y compararlos en igualdad de años.

**Grilla.** Una sola grilla para todas las plataformas y para las variables
climáticas que se agreguen después, definida en WGS84 con dos niveles
anidados:

- **Celda base de 0,05°** con bordes en múltiplos de 0,05°: la grilla de
  CHIRPS, que es la común más fina con IMERG (0,1°, bordes en múltiplos de
  0,1°) y con ERA5-Land (0,1°, celdas centradas en múltiplos de 0,1°).
  Cada celda lleva un identificador estable derivado de su esquina
  suroeste (`c0985_m08525` = 9,85 N, 85,25 O), que es la llave entre
  índices, clima y cobertura, con el papel que `id_deteccion` tiene entre
  detecciones y capas.
- **Celda de análisis de 0,1°** formada por 2 × 2 celdas base y **centrada
  en los nodos de ERA5-Land** (bordes en múltiplos impares de 0,05°), de
  modo que coincide uno a uno con ERA5-Land y con cualquier índice de
  peligro que se derive de él. Cada celda base conoce su celda madre.

Con MODIS, la celda de 0,1° es la más fina que deja celdas suficientes: de
las 477 con alguna detección, 195 (41 %) acumulan 30 o más en 2002–2025 y
cubren la vertiente del Pacífico y la zona norte casi sin huecos; a 0,05°
casi ninguna alcanza el mínimo. Se descartan la rejilla sinusoidal de MODIS
(1 km: 30 000 detecciones para 50 000 celdas y una proyección incómoda) y
una grilla propia en CRTM05 (obligaría a remuestrear todos los productos
climáticos). Para los mapas, las celdas se vectorizan y se proyectan a
CRTM05 como polígonos, sin remuestreo. Una detección pertenece a la celda
que contiene su punto; a 11 km, la geolocalización de ~1 km de MODIS y la
elipse del footprint no importan.

**Umbral**: las celdas con menos de 30 detecciones acumuladas en el periodo
de referencia quedan en NA y se dibujan en gris. Es distinto del umbral de
300 de la tabla anual, que aplica al total nacional de un año.

**Plataformas.** El ráster se calcula por plataforma, sobre la misma
grilla y con el mismo umbral, y nunca juntando las detecciones de dos
plataformas: VIIRS produce cinco veces más detecciones por año que MODIS y
ve las quemas pequeñas de inicio de temporada que MODIS no ve, así que un
consolidado mixto sería un mapa de VIIRS con ruido de MODIS y con `INI`
adelantado. Las comparaciones legítimas, diferencia de `INI` o `LON` por
celda en el periodo de traslape y mapa de acuerdo entre plataformas,
vendrán como productos aparte.

### Salidas

- Tabla con una fila por año de fuego: año, marca de parcial/provisional,
  `DTOT`, `INI`, `FIN` (día y fecha) y `LON`. Es el CSV publicado.
- Tabla de detecciones excluidas por tipo y año.
- Figura de temporada: un segmento por año de `INI` a `FIN`, ordenado
  cronológicamente, con enero a mayo (SINAC) y diciembre a abril (IMN) como
  bandas de referencia.
- Ráster consolidado: una capa por celda de 0,1° para `INI`, `FIN`, `LON` y
  `DTOT` (auxiliar), en GeoTIFF con la plataforma, el periodo de referencia
  y el umbral en los metadatos; mapa estático de `LON` con las celdas bajo
  el umbral en gris, y las mismas celdas como capa del mapa interactivo.

### Referencias

- Archibald, S., Lehmann, C. E. R., Gómez-Dans, J. L. y Bradstock, R. A.
  (2013). Defining pyromes and global syndromes of fire regimes. *PNAS*,
  110(16), 6442–6447. <https://doi.org/10.1073/pnas.1211466110>
- Benali, A. et al. (2017). Bimodal fire regimes unveil a global-scale
  anthropogenic fingerprint. *Global Ecology and Biogeography*, 26,
  799–811. <https://doi.org/10.1111/geb.12586>
- Boschetti, L. y Roy, D. P. (2008). Defining a fire year for reporting and
  analysis of global interannual fire variability. *Journal of Geophysical
  Research: Biogeosciences*, 113, G03020.
  <https://doi.org/10.1029/2008JG000686>
- Dunning, C. M., Black, E. C. L. y Allan, R. P. (2016). The onset and
  cessation of seasonal rainfall over Africa. *Journal of Geophysical
  Research: Atmospheres*, 121.
  <https://doi.org/10.1002/2016JD025428>
- Ferreira, L. N., Vega-Oliveros, D. A., Zhao, L., Cardoso, M. F. y Macau,
  E. E. N. (2020). Global fire season severity analysis and forecasting.
  *Computers & Geosciences*.
  <https://www.sciencedirect.com/science/article/abs/pii/S0098300419302808>
- Giglio, L., Csiszar, I. y Justice, C. O. (2006). Global distribution and
  seasonality of active fires as observed with the Terra and Aqua MODIS
  sensors. *Journal of Geophysical Research: Biogeosciences*, 111, G02016.
  <https://doi.org/10.1029/2005JG000142>
- Liebmann, B. et al. (2012). Seasonality of African precipitation from 1996
  to 2009. *Journal of Climate*, 25, 4304–4322.
  <https://doi.org/10.1175/JCLI-D-11-00157.1>
- SINAC (2012). *Estrategia Nacional de Manejo Integral del Fuego en Costa
  Rica 2012–2021*. Sistema Nacional de Áreas de Conservación, MINAE.
  <https://www.sinac.go.cr/ES/partciudygober/Documents/Estrategia%20Nacional%20Manejo%20del%20Fuego.pdf>
- Villalobos Flores, R., Retana, J. A. y Acuña, A. (s. f.). *El Niño y los
  incendios forestales en Costa Rica*. Instituto Meteorológico Nacional,
  Gestión de Desarrollo (datos hasta 2000).
  <https://www.imn.ac.cr/documents/10179/20911/El+Ni%C3%B1o+y+los+incendios+forestales>
- Zhang, X. et al. (2011). Indices for monitoring changes in extremes based
  on daily temperature and precipitation data. *WIREs Climate Change*, 2,
  851–870. <https://doi.org/10.1002/wcc.147>
- Anthropogenic warming drives earlier wildfire season onset in California
  (2025). *Science Advances*. <https://doi.org/10.1126/sciadv.adt2041>
- Biogeographic patterns of daily wildfire spread and extremes across North
  America (2024). *Frontiers in Forests and Global Change*, 7.
  <https://doi.org/10.3389/ffgc.2024.1355361>

## Requisitos

- Una clave (MAP_KEY) gratuita del API de FIRMS:
  <https://firms.modaps.eosdis.nasa.gov/api/map_key/>
- Un token de Earthdata Login (cuenta gratuita) para descargar MCD64A1:
  <https://urs.earthdata.nasa.gov/> → *Generate Token*. Los tokens expiran a
  los ~60 días; si la descarga devuelve HTTP 401, hay que regenerarlo.
- Docker (recomendado) o R ≥ 4.5 con renv (alternativa, p. ej. en Windows).
- Espacio en disco: la caché nacional (FIRMS + ~940 gránulos HDF + 4 teselas
  WorldCover + DEM) ronda los 10 GB.

## Uso con Docker (recomendado)

```bash
cp .env.example .env            # defina RSTUDIO_PASSWORD
cp .Renviron.example .Renviron  # ingrese FIRMS_MAP_KEY y EARTHDATA_TOKEN
docker compose up -d --build
```

Abra RStudio Server en <http://localhost:8787> (usuario `rstudio`, la contraseña
de `.env`), abra el proyecto `anomalias-termicas-costarica.Rproj` y ejecute:

```r
renv::restore()      # instala las versiones fijadas de los paquetes
targets::tar_make()  # ejecuta el pipeline completo
```

También puede ejecutarse sin RStudio. Importante: **con `--user 1000:1000`**,
porque `docker compose run` ejecuta como root por defecto y renv enlazaría los
paquetes a la caché de root (`/root/.cache`), que se pierde al salir el
contenedor y deja la biblioteca del proyecto con enlaces rotos:

```bash
docker compose run --rm --user 1000:1000 -e HOME=/home/rstudio rstudio \
  Rscript -e "renv::restore(); targets::tar_make()"
```

La primera corrida completa descarga el registro histórico entero (~1860
fragmentos de FIRMS por fuente, con pausas por el límite de transacciones del
API) y puede tomar varias horas; es reanudable en todo momento.

## Uso con renv (sin Docker, p. ej. Windows)

1. Instale [R ≥ 4.5](https://cran.r-project.org/),
   [RTools](https://cran.r-project.org/bin/windows/Rtools/) (Windows) y
   [Quarto](https://quarto.org/).
2. Clone el repositorio, copie `.Renviron.example` a `.Renviron` e ingrese sus
   credenciales (`FIRMS_MAP_KEY` y `EARTHDATA_TOKEN`).
3. En R, dentro del proyecto:

```r
renv::restore()
targets::tar_make()
```

## Configuración del pipeline

Los parámetros se editan al inicio de [`_targets.R`](_targets.R):

| Parámetro | Descripción | Valor por defecto |
|---|---|---|
| `fecha_inicio` | Inicio del período | `2001-01-01` |
| `fecha_fin` | Fin del período (se recorta a lo disponible) | `2100-01-01` (= todo lo disponible) |

Las plataformas se definen en la tabla `PLATAFORMAS` de
[`R/plataformas.R`](R/plataformas.R): una fila por plataforma, de la que se
derivan las colecciones de FIRMS, el producto de área quemada, los
directorios de salida y **todos los rótulos visibles**. Agregar una plataforma
es agregar una fila.

### Cómo leer el pipeline

Las cuatro cadenas se generan con `tarchetypes::tar_map`, así que los nombres
de target **no aparecen literalmente** en `_targets.R`: cada target del bloque
`tar_map` existe cuatro veces con el sufijo de la clave de plataforma. No
existe `firms_pais`; existen `firms_pais_modis`, `firms_pais_snpp`,
`firms_pais_noaa20` y `firms_pais_noaa21`.

```r
targets::tar_manifest(fields = "name")   # lista los targets generados
targets::tar_visnetwork()                # grafo, con el nombre de cada plataforma
```

El área quemada se organiza por **producto** y no por plataforma
(`quemas_mcd64a1`, `quemas_vnp64a1`): hay exactamente dos y VNP64A1 alimenta a
tres plataformas. Sus ramas dinámicas son por **gránulo** (mes × tesela),
porque Costa Rica cruza las teselas h09v07 y h09v08.

Constantes adicionales (área de estudio, buffer de descarga, tamaño de
fragmento, teselas) en [`R/constantes.R`](R/constantes.R).

## Estructura del repositorio

```
├── _targets.R          # definición del pipeline
├── R/                  # funciones: descarga (FIRMS, CMR, WFS), procesamiento,
│                       #   cobertura de la tierra, áreas de conservación,
│                       #   eventos documentados, visualización y tablas
├── R/temporada.R       # año de fuego e índices anuales de temporada (LON)
├── tests/testthat/     # pruebas unitarias con datos sintéticos
├── analysis/portada.qmd # portada         → index.html (GitHub Pages)
├── analysis/modis.qmd   # reporte MODIS   → modis/index.html
├── analysis/snpp.qmd    # reporte S-NPP   → snpp/index.html
├── analysis/noaa20.qmd  # reporte NOAA-20 → noaa20/index.html
├── analysis/noaa21.qmd  # reporte NOAA-21 → noaa21/index.html
├── data/raw/           # caché de datos crudos (no versionada)
├── outputs/            # figuras, mapas y tablas, en una carpeta por
│                       #   plataforma: figs/modis/, figs/snpp/, ...
├── Dockerfile          # rocker/geospatial + paquetes del proyecto
├── docker-compose.yml  # RStudio Server (puerto 8787)
└── renv.lock           # versiones fijadas de paquetes
```

## Fuentes de datos

| Fuente | Datos | Licencia/atribución |
|---|---|---|
| [NASA FIRMS](https://firms.modaps.eosdis.nasa.gov/) | Anomalías térmicas MODIS Collection 6.1 (MODIS_SP), DOI: 10.5067/FIRMS/MODIS/MCD14ML, y VIIRS 375 m de Suomi-NPP, NOAA-20 y NOAA-21 | Acceso abierto; se agradece atribución a NASA FIRMS |
| [NASA LP DAAC](https://lpdaac.usgs.gov/) | Área quemada mensual MCD64A1 v6.1 (500 m), DOI: 10.5067/MODIS/MCD64A1.061, y VNP64A1 v002 (VIIRS/NPP, 500 m), DOI: 10.5067/VIIRS/VNP64A1.002 | Acceso abierto con Earthdata Login; se agradece atribución a NASA LP DAAC |
| [IGN / SNIT](https://www.snitcr.go.cr/) | Límite provincial 1:5000 (`IGN_5_CO:limiteprovincial_5k`); su unión es el límite nacional del análisis | Datos públicos del Estado costarricense |
| [SINAC](https://geos1pne.sirefor.go.cr/wfs) | Áreas de conservación (`PNE:areas_conservacion`) y Registro Nacional de Humedales, actualización 2016–2018 (`PNE:registro_nacional_humedales`) | Datos públicos del Estado costarricense |
| [ESA WorldCover](https://esa-worldcover.org/) | Cobertura de la tierra 2021 a 10 m (v200), DOI: 10.5281/zenodo.7254221 | CC BY 4.0; atribución a ESA WorldCover |
| [Terrain Tiles](https://registry.opendata.aws/terrain-tiles/) | Modelo de elevación (formato Terrarium, zoom 9) para el relieve del video | Datos abiertos en AWS; atribución a Mapzen y las fuentes del DEM (SRTM, NASA) |

## Trabajo futuro

- **Contexto paisajístico nacional**: el análisis de fracción de bosque
  alrededor de cada detección contra un modelo nulo (heredado del proyecto de
  Palo Verde) quedó fuera de esta versión; a escala de 51 100 km² exige
  agregar el ráster de bosque y repensar la pregunta ecológica.
- **Isla del Coco**: incorporarla como área separada (con su propio bbox de
  descarga) si alguna vez interesa, en lugar de estirar el bbox nacional.
- Desagregar también por provincia o cantón, además del área de conservación.
- Comparar formalmente las cuatro plataformas en sus traslapes, en vez de
  solo publicarlas lado a lado.
- Vigilar el fin de las misiones Terra y Aqua en 2027 y decidir cuál serie
  pasa a ser la de referencia del proyecto.
- Aprovechar las bandas `Burn Date Uncertainty` y `QA` de los productos de
  área quemada.
- Cobertura con resolución temporal (Dynamic World, compuestos propios de
  Sentinel-2 o Landsat) en lugar de la foto fija de WorldCover 2021.
- Modelar los factores de ignición con covariables de accesibilidad (cercanía
  a caminos, linderos y zonas de cultivo).
- Reestructurar el reporte para que solo lea targets (`tar_read()`) en lugar
  de llamar funciones de `R/`; hoy esa dependencia se cubre con `extra_files`
  en `tar_quarto()`.

## Licencia

El código se distribuye bajo la [licencia MIT](LICENSE). Los datos conservan
las condiciones de sus fuentes originales.
