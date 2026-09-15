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
anterior está publicado; esta sección documenta los cinco primeros, la
**longitud de la temporada**, la **concentración diaria del fuego**, la
**frecuencia y densidad del fuego**, la **intensidad del fuego** y los
**días extremos**, cada uno en su versión anual o espacial consolidada, y
solo lo necesario para calcularlos, sigue con las reglas de su
**extensión a las plataformas VIIRS** y cierra con la **comparación entre
plataformas en el traslape**, el único lugar del proyecto donde dos
plataformas se miran juntas. Nada de esta sección
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

**No se aplica umbral de confianza.** La confianza de MODIS (0–100; FIRMS
llama «baja» a < 30 y «alta» a ≥ 80) se probó como filtro sobre la serie de
vegetación 2002–2025 antes de descartarla: con ≥ 30 se pierde el 4 % de las
detecciones y `LON` anual cambia en promedio 0,8 días (3 como máximo); con
≥ 50, el 16 % y 2 días (8 como máximo); con ≥ 80, el 71 %, la mitad de los
años cae bajo el mínimo de 300 y `LON` cambia 11 días en promedio. Además,
la confianza alta selecciona fuegos grandes (FRP mediana de 31 MW frente a
12 MW) y nocturnos, y es estacional (mediana 72 de enero a abril, 55–61 de
junio a noviembre), de modo que un umbral elimina preferentemente las
detecciones fuera de temporada sin haber demostrado que sean falsas. Por
último, la confianza de VIIRS es categórica (baja, nominal, alta) y con
otro algoritmo, así que un umbral numérico rompería la simetría entre
plataformas.

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

**Celdas bimodales.** La definición de `INI` y `FIN` por percentiles
acumulados supone una sola temporada. En una celda con dos picos de fuego
separados por meses sin detecciones, el 10 % cae al inicio del primero y el
90 % al final del segundo, y `LON` abarca también el vacío entre ambos: el
número es correcto según la definición pero no significa «temporada larga».
En Costa Rica ocurre en las llanuras del norte y la vertiente Caribe, donde
la lluvia tiene dos periodos relativamente secos (febrero a abril y el
veranillo de setiembre y octubre) y el fuego sigue el calendario agrícola,
no una estación seca (Benali et al. 2017 muestran que estas temporadas
bimodales son en gran parte de origen humano). Con MODIS, en la celda de
Boca San Carlos las detecciones de 2002–2025 tienen un pico en abril y otro
en setiembre, julio en cero y `LON` = 266.

El indicador es la **fracción fuera de temporada** (`FUERA`): proporción de
las detecciones de la celda, en el periodo de referencia, cuya fecha cae
fuera de diciembre a mayo, la unión de la época seca del IMN y la temporada
del SINAC. En las celdas unimodales del Pacífico ronda el 2 %; en las
bimodales, entre 21 y 59 %. Las celdas con `FUERA` > 25 % se marcan como
**sin estación definida**: `INI`, `FIN` y `LON` quedan en NA en el ráster
y en la tabla, `FUERA` se publica como capa propia y los mapas las dibujan
con trama en lugar de color, para que no se lean como temporadas largas ni
desaparezcan como si no tuvieran fuego. Con MODIS son 4 de 163 celdas
válidas (todas en las llanuras de San Carlos); el 97 % restante es
unimodal, así que la definición se mantiene y solo se marcan las
excepciones. Un umbral de confianza no las corrige: las detecciones fuera
de temporada tienen confianza nominal, como el 67 % del registro (ver
«Detecciones incluidas»). La extensión natural, dos pares de `INI` y `FIN`
por celda al estilo de Dunning et al. (2016) para lluvias bimodales, queda
como trabajo futuro; la concentración circular de las fechas sería la
alternativa general al indicador, más abstracta y por eso no adoptada.

**Plataformas.** El ráster se calcula por plataforma, sobre la misma
grilla y con el mismo umbral, y nunca juntando las detecciones de dos
plataformas: VIIRS produce cinco veces más detecciones por año que MODIS y
ve las quemas pequeñas de inicio de temporada que MODIS no ve, así que un
consolidado mixto sería un mapa de VIIRS con ruido de MODIS y con `INI`
adelantado. Las comparaciones legítimas, diferencia de `INI` o `LON` por
celda en el periodo de traslape y mapa de acuerdo entre plataformas,
vendrán como productos aparte.

### Segundo índice: concentración diaria del fuego (`N50`, `C10`)

`LON` dice cuánto dura la temporada; este índice dice si el fuego de un año
llega repartido a lo largo de ella o en unas pocas oleadas de quema masiva.
Se calcula sobre la misma serie diaria, ordenando los días del año de fuego
de mayor a menor número de detecciones:

| Código | Definición | Unidad |
|---|---|---|
| `DF` | Días de fuego del año: días con al menos una detección (auxiliar) | días |
| `N50` | Número mínimo de días que, ordenados de mayor a menor, acumulan al menos el 50 % de `DTOT` | días |
| `C10` | Porcentaje de `DTOT` que ocurre en los 10 días con más detecciones | % |

Un `N50` de 8 significa que la mitad del fuego del año cupo en 8 días; un
`C10` de 40 % que los 10 días más activos concentraron dos quintos de las
detecciones. Los empates entre días con el mismo conteo no afectan las
sumas. Como `LON`, el índice se define por fracciones del total anual y es
**insensible a la escala del conteo**: no lo alteran los años 2001 y 2002
con solo Terra ni la deriva orbital. Los años parciales, provisionales o
con `DTOT` < 300 llevan las mismas marcas que en la tabla de `LON`.

**Sustento.** No conocemos un uso de este índice con detecciones de fuego
activo; es un préstamo razonado de tres fuentes. El índice de concentración
diaria de la precipitación de Martín-Vide (2004) mide con la curva de Lorenz
cuánto del total anual cae en los días más lluviosos, y `N50` y `C10` son
su lectura directa sobre esa misma curva. En el ETCCDI, `R95pTOT` expresa
la fracción del total aportada por los días extremos; `C10` hace lo mismo
con un número fijo de días en lugar de un percentil del periodo base, lo
que evita depender del conteo absoluto. Y Cunningham et al. (2024) muestran
que la dimensión del régimen de fuego que más cambia a escala global es la
simultaneidad, cuántos fuegos intensos coinciden en pocos días, que es
justamente lo que este índice captura a escala nacional.

**Versión consolidada por celda.** Sobre la distribución agrupada de cada
celda, con las fechas reales de los años de fuego del periodo de
referencia, se calcula la proporción de sus días de fuego que reúnen la
mitad de sus detecciones:

| Código | Definición | Unidad |
|---|---|---|
| `N50F` | `N50` de la celda dividido entre sus días de fuego `DF`, ambos sobre las fechas agrupadas del periodo de referencia | 0–0,5 |

Vale 0,5 cuando todos los días de fuego de la celda tuvieron el mismo
número de detecciones (fuego repartido) y se acerca a 0 cuando unas pocas
fechas concentran casi todo (fuego en oleadas, como una quema extensa que
produce decenas de detecciones en dos o tres días). La razón normaliza por
el número de fechas y hace comparables celdas con distinto conteo. Con
pocas detecciones casi todas las fechas tienen una sola y la razón tiende
a 0,5 sin significar nada, por lo que el umbral de esta capa es de **100
detecciones acumuladas**, más alto que el de `LON`. Las celdas marcadas sin
estación definida sí reciben `N50F`: la concentración no depende de que
haya una temporada.

### Tercer índice: frecuencia y densidad del fuego (`FREC`, `DENS`)

Los dos primeros índices dicen cuándo y cómo llega el fuego; este dice
**dónde y cuánto**. Es el mapa más antiguo de la pirogeografía satelital
(la densidad de píxeles de fuego de Giglio et al. 2006) y, junto con la
variabilidad interanual, uno de los ejes con que Chuvieco et al. (2008)
definen regímenes de fuego a partir de observaciones de la Tierra. Solo
tiene versión consolidada por celda, sobre las detecciones de vegetación:

| Código | Definición | Unidad |
|---|---|---|
| `AREA` | Superficie terrestre de la celda: su intersección con el límite nacional (auxiliar) | km² |
| `ANIOS` | Años de fuego del periodo base con al menos una detección en la celda (auxiliar) | años |
| `FREC` | `ANIOS` dividido entre los años del periodo base: probabilidad empírica de que la celda tenga fuego en un año dado | 0–1 |
| `DENS` | Detecciones por km² de superficie terrestre y por año del periodo base | det./km²/año |

A diferencia de `LON` y `N50F`, **el cero es un dato**: una celda sin
detecciones en veinte años tiene `FREC` = 0 y `DENS` = 0, y el ráster
cubre todas las celdas de la grilla que tocan el país, no solo las que
tuvieron fuego. Por eso no hay umbral de detecciones. Sí hay uno de
superficie: las celdas con menos de 10 km² de tierra, fragmentos de costa
donde una sola detección daría una densidad enorme, quedan en NA.

**Periodo base.** `FREC` y `DENS` son los primeros índices de la suite que
**dependen del conteo absoluto** de detecciones, y por tanto de la
detectabilidad de la serie. Para que sean comparables entre celdas y no
arrastren los cambios del instrumental, se consolidan sobre el **periodo
base 2003–2022**, los veinte años de fuego con Terra y Aqua completos:
empieza en 2003 porque Aqua entra en julio de 2002 y los años de fuego 2001
y 2002 tienen la mitad de las pasadas, y termina en 2022 porque Terra dejó
de mantener su hora de paso en 2020 y Aqua en 2022, y desde entonces sus
horas de cruce derivan. Los índices por fracciones (`LON`, `FUERA`, `N50F`)
siguen usando todos los años completos, 2002–2025, porque no les afecta.
El periodo base queda declarado por plataforma (el de las VIIRS, en
«Extensión a las plataformas VIIRS») y es el que usan los índices por
percentil. La consecuencia práctica: `DENS` y `FREC` describen 2003–2022 y
no el presente; su versión reciente vendrá como comparación de periodos,
no como actualización continua.

**Lectura.** `FREC` separa el fuego recurrente del ocasional: una celda con
0,9 arde casi todos los años, una con 0,2 arde uno de cada cinco. `DENS`
gradúa la intensidad de uso del fuego dentro de las recurrentes. Ambas son
específicas de la plataforma: MODIS ve cinco veces menos detecciones que
VIIRS, así que las densidades no se comparan entre sensores, solo entre
celdas de un mismo mapa. No hay versión anual por celda: con 2,6
detecciones por celda y año en promedio, un ráster anual sería ruido; la
dimensión interanual corresponde a las áreas de conservación.

### Cuarto índice: intensidad del fuego (`FRPI`, `FRP95`)

Los tres primeros índices cuentan detecciones; este usa el único campo
físico que trae cada una, la **potencia radiativa del fuego** (`frp`, en
megavatios), que MODIS deriva de la radiancia del píxel en la banda de 4 µm
y que es proporcional a la tasa de combustión de biomasa (Wooster et al.
2005). Responde con qué fuerza arde el fuego, no cuándo ni cuánto, y
distingue las quemas agrícolas pequeñas de los incendios extensos de
sabana y humedal.

| Código | Definición | Unidad |
|---|---|---|
| `FRPI` | Mediana de la FRP de las detecciones de vegetación del año de fuego | MW |
| `FRP95` | Percentil 95 de la FRP de esas detecciones | MW |
| `AQ` | Fracción de las detecciones del año que son de Aqua, el paso de la tarde (control) | 0–1 |
| `NOC` | Fracción de las detecciones del año que son nocturnas (control) | 0–1 |

Se usa la mediana y no la media porque la distribución de la FRP es muy
asimétrica: unas pocas detecciones de cientos de megavatios dominarían el
promedio. `FRP95` captura precisamente esa cola, la intensidad de los
fuegos más fuertes del año, que Cunningham et al. (2024) identifican como
la dimensión del régimen de fuego que más crece a escala global. Ambos son
independientes del número de detecciones, así que el conteo reducido de
2001 y 2002 no los altera por sí mismo.

**Controles: satélite y hora.** La FRP sí depende de la hora de
observación. Aqua pasa a primera hora de la tarde, cuando los fuegos arden
con más fuerza, y registra FRP mayores que Terra a media mañana (Giglio
2007); las detecciones nocturnas, pocas en Costa Rica, corresponden a
fuegos grandes o persistentes (Balch et al. 2022). Un año con otra
proporción de Aqua o de noche tendría otra FRP sin que cambiara el fuego.
Por eso la tabla anual lleva `AQ` y `NOC` como columnas de control, la
prosa advierte de 2001 y 2002 (sin Aqua, `AQ` = 0) y de los años recientes
con deriva de las horas de paso, y la versión por celda se consolida sobre
el **periodo base 2003–2022**, donde la mezcla de satélites es estable. Un
segundo matiz: la FRP es por píxel, y los píxeles del borde del barrido
cubren varias veces más superficie que los del nadir, lo que también
favorece la mediana frente a la media. No se normaliza por el área del
píxel: la FRP se reporta como la publica FIRMS, que es como la usa la
literatura (Ichoku et al. 2008).

**Versión consolidada por celda.** Sobre las detecciones de vegetación del
periodo base:

| Código | Definición | Unidad |
|---|---|---|
| `FRPI` | Mediana de la FRP de las detecciones de la celda | MW |
| `AQ` | Fracción de esas detecciones que son de Aqua | 0–1 |

El mapa de `FRPI` es la climatología de FRP de Ichoku et al. (2008) a
escala nacional; el de `AQ` es el ciclo diurno de Giglio (2007): una celda
con `AQ` alta arde sobre todo por la tarde, el patrón de las quemas
agrícolas encendidas a media mañana que MODIS ve ya crecidas en el paso de
Aqua, y una con `AQ` cerca de la mitad tiene fuego que persiste desde la
mañana. Ambas capas usan el umbral de 30 detecciones de `LON`; `FRP95` no
tiene versión por celda porque el percentil 95 de unas decenas de valores
no es estable.

### Quinto índice: días extremos (`ND95`, `D95p`, `D95pTOT`)

Es el análogo directo de `R95p` y `R95pTOT`, los índices de extremos por
percentil del ETCCDI (Zhang et al. 2011): cuánto del fuego del año ocurre
en días que, por su número de detecciones, son extremos respecto del
registro. A diferencia de `N50` y `C10`, que son relativos al propio año,
este mide los extremos contra un umbral fijo y por eso permite decir si un
año tuvo más días de quema masiva que otro en términos absolutos.

| Código | Definición | Unidad |
|---|---|---|
| `P95` | Umbral: percentil 95 de las detecciones diarias en los **días de fuego del periodo base** (días con al menos una detección) | detecciones/día |
| `ND95` | Días del año de fuego con más de `P95` detecciones | días |
| `D95p` | Detecciones acumuladas en esos días | n |
| `D95pTOT` | Fracción de `DTOT` que aportan (análogo de `R95pTOT`) | % |

**El umbral.** Con MODIS, los 2 841 días de fuego de 2003–2022 dan un
`P95` de **29 detecciones por día** (el percentil 99 es 52). Es un cuantil
empírico calculado una sola vez y aplicado a todos los años, dentro y
fuera del periodo base. El ETCCDI evita con un remuestreo la
inhomogeneidad que introduce evaluar un año contra un umbral calculado con
ese mismo año (Zhang et al. 2005); aquí no hace falta: quitando cualquier
año del periodo base el umbral queda en 28 o 29, porque se apoya en casi
tres mil días y no en los de un solo año. Se calcula sobre los días de
fuego, como el ETCCDI lo hace sobre los días húmedos, para que los días sin
detecciones, la mayoría del año, no arrastren el percentil hacia cero.

**Dependencia del conteo.** Como `FREC` y `DENS`, este índice depende de
la detectabilidad de la serie: un día con 29 detecciones de Terra y Aqua
no es comparable con uno de solo Terra. Por eso los años de fuego 2001 y
2002 se marcan como **no comparables** (sin Aqua, su `ND95` es de un solo
día) y los años posteriores a 2022 se leen con la fracción de Aqua (`AQ`)
de la tabla a la vista. `P95` es específico de la plataforma: cada VIIRS
lo calcula sobre su propio periodo base («Extensión a las plataformas
VIIRS»).

**Lectura.** En el registro MODIS, `ND95` va de 0 a 12 días por año y
`D95pTOT` de 0 a 43 %: en 2022 diez días concentraron el 43 % de las
detecciones, mientras que 2023 y 2025 no tuvieron ningún día extremo. Es
la diferencia entre un año con temporada corta e intensa y años tranquilos
en los que el fuego nunca superó el umbral del registro. Cunningham et al.
(2024) muestran que la frecuencia de los días extremos es lo que más
cambia en el régimen de fuego global; este índice es su versión para el
país.

**Sin versión espacial.** Por celda no hay conteos diarios suficientes para
un percentil (2,6 detecciones por celda y año); la desagregación natural
es por área de conservación, cuando se incorpore.

### Extensión a las plataformas VIIRS

La suite se calcula **por plataforma**, con las mismas definiciones, las
mismas fracciones y los mismos umbrales, sobre la misma grilla, y nunca
juntando detecciones de dos plataformas: cada VIIRS tiene su tabla anual,
su consolidado por celda y su GeoTIFF, hermanos de los de MODIS y no
mezclados con ellos. Lo que cambia entre plataformas es lo que sigue.

**Periodo de referencia y periodo base.** El periodo de referencia (años
de fuego completos y no provisionales) sale del registro de cada
plataforma: el procesamiento estándar de Suomi-NPP empieza el 20 de enero
de 2012 y el de NOAA-20 el 1 de abril de 2018, así que los años de fuego
2012 y 2018 son parciales y los periodos de referencia son **2013–2025**
para S-NPP y **2019–2025** para NOAA-20. En MODIS el periodo base es más
corto que el de referencia porque descarta los años con un solo satélite y
los de deriva orbital; en las VIIRS no hay motivo instrumental equivalente
(una sola plataforma por serie, en órbita heliosincrónica mantenida a las
13:30), así que el **periodo base coincide con el de referencia**:
2013–2025 (trece años) para S-NPP y 2019–2025 (siete) para NOAA-20. Como
en MODIS, es una constante de `PLATAFORMAS` y no se extiende sola al
cerrarse cada año: ampliarlo es una decisión explícita que recalcula
`FREC`, `DENS`, `FRPI` por celda y `P95`, y por eso cambia índices
publicados. El de NOAA-20 es corto: su `P95` se apoya en unos 1 500 días de
fuego y al quitar un año oscila entre 92 y 108 detecciones (en S-NPP, con
unos 2 800 días, entre 107 y 114), de modo que sus días extremos se leen
como provisionales hasta que el registro crezca.

**Control de satélite.** `AQ` mide la mezcla de dos satélites en una misma
serie (la fracción del paso de la tarde, Aqua) y solo tiene sentido en
MODIS. Cada plataforma VIIRS es un solo satélite, así que `AQ` queda en NA
en su tabla anual, en su tabla por celda y en la banda `aq` de su GeoTIFF,
no se dibuja el mapa de ciclo diurno y ningún año se marca como no
comparable por ese motivo. Qué satélite actúa de control es una columna de
`PLATAFORMAS` (`satelite_control`: Aqua para MODIS, NA para las VIIRS) y no
un nombre escrito en las funciones. `NOC` se conserva y es mayor en VIIRS
(en torno al 20–30 % de las detecciones frente al 10–35 % de MODIS) porque
el píxel de 375 m detecta de noche fuegos pequeños que MODIS no ve; eso
lo hace, también, no comparable en nivel entre sensores.

**NOAA-21 queda fuera de la suite** mientras FIRMS no publique su
procesamiento estándar: toda su serie es tiempo casi real, así que no tiene
ningún año de fuego completo y no provisional del que sacar un periodo de
referencia o base, y su cola no trae `type`, con lo que el filtro de
vegetación no apartaría volcanes ni fuentes estáticas. Entra sola cuando
tenga periodo base en `PLATAFORMAS`; sus series, mapas y videos siguen
publicándose como hasta ahora.

**Umbrales iguales, lectura distinta.** VIIRS produce entre cinco y ocho
veces más detecciones de vegetación por año que MODIS (S-NPP, 1 800–8 600
por año de fuego), así que el mínimo de 300 detecciones anuales se cumple
siempre y los umbrales por celda validan muchas más celdas: con S-NPP,
269 de las 456 celdas con fuego alcanzan las 30 detecciones (163 de 406 en
MODIS) y el 17 % de las celda-años (1 % en MODIS). Los umbrales no se
ajustan por plataforma, porque son mínimos de estabilidad estadística y no
de comparabilidad; la consecuencia es que los mapas VIIRS tienen más
celdas con índice, no que sean más precisos donde MODIS también los tiene.

**Qué se compara y qué no.** Entre plataformas son comparables en nivel
los índices por fracciones y fechas: `INI`, `FIN`, `LON`, `FUERA`, `N50`,
`C10` y `N50F`. No lo son los que dependen del conteo o del píxel: `DTOT`,
`DENS`, `ND95`, `P95` y `D95p` (más detecciones por fuego), `FREC` en
parte (una celda con fuego pequeño arde «más años» para VIIRS) y `FRPI` y
`FRP95` (la FRP de VIIRS es por píxel de 375 m, un fuego se reparte en
varios píxeles y el algoritmo la deriva de otra banda; la mediana ronda
4–5 MW frente a 15–18 MW en MODIS). Las comparaciones formales en el
periodo de traslape, diferencias de `INI` y `LON` por celda, mapa de
acuerdo entre plataformas y anomalías estandarizadas de los índices de
conteo, son productos aparte y se definen en la subsección siguiente.

### Comparación entre plataformas en el traslape

Las series no se suman ni se empalman, pero sí se pueden **comparar** en
los años que dos plataformas observaron a la vez. Es la única parte del
proyecto en que dos plataformas aparecen en un mismo producto, y lo hacen
como dos columnas o dos mapas puestos lado a lado, nunca como una mezcla
de detecciones. Responde a dos preguntas: si los hallazgos de la serie
larga (MODIS) se sostienen en el sensor que la relevará, y qué parte de la
diferencia entre plataformas es propiedad del fuego y qué parte del
píxel. La literatura da la expectativa: el píxel de 375 m de VIIRS
detecta fuegos más pequeños y más fríos que el de 1 km de MODIS
(Schroeder et al. 2014; Giglio et al. 2016), produce varias veces más
detecciones sobre los mismos fuegos (Fu et al. 2020) y reporta una FRP
por píxel que no equivale a la de MODIS (Li et al. 2018).

**Pares y periodo de traslape.** Se comparan pares ordenados `(A, B)` de
plataformas con periodo base; las diferencias son siempre **`B` − `A`**.
El **periodo de traslape** de un par es la intersección de sus periodos
de referencia (años de fuego completos y no provisionales en ambas):

| Par | Traslape | Lectura |
|---|---|---|
| MODIS → VIIRS S-NPP | 2013–2025 (13 años) | El par principal: la serie larga frente a su relevo |
| MODIS → VIIRS NOAA-20 | 2019–2025 (7) | Comprobación del anterior con la otra VIIRS |
| VIIRS S-NPP → VIIRS NOAA-20 | 2019–2025 (7) | Control: el mismo instrumento en dos plataformas; lo que difiera aquí no es el píxel |

Un `ΔINI` negativo significa que `B` empieza antes que `A`. Con 13 años
las correlaciones se reportan; con 7 solo se describen. Los pares se
derivan de `PLATAFORMAS`: una plataforma entra en la comparación cuando
tiene periodo base, y NOAA-21 se incorporará sola cuando lo tenga.

**Producto 1: índices anuales lado a lado.** Para los índices
comparables en nivel (`INI`, `FIN`, `LON`, `N50`, `C10`), una tabla por
par con el valor de cada plataforma en cada año del traslape y su
diferencia, y un resumen con la mediana y el rango de la diferencia y la
correlación de Spearman entre las dos series anuales. La correlación
dice si las dos plataformas ordenan igual los años (una temporada larga
lo es para ambas); la mediana de la diferencia, si hay un sesgo
sistemático, como el adelanto de `INI` que cabe esperar en VIIRS por las
quemas pequeñas de inicio de temporada.

**Producto 2: anomalías estandarizadas de los índices de conteo.** Los
índices que dependen del conteo (`DTOT`, `ND95`, `D95pTOT`) no se
comparan en nivel, pero sí como **anomalía estandarizada**: el valor de
cada año menos la media del traslape, dividido entre la desviación típica
del traslape, calculada por plataforma. Es adimensional y responde a la
pregunta que importa, si un año fue extremo para las dos plataformas a
la vez, sin que el número absoluto de detecciones intervenga. Se
presentan las dos series de anomalías por año, la correlación de
Spearman, la fracción de años en que ambas tienen el mismo signo y los
años que cada plataforma sitúa en su máximo y su mínimo. Cuando las
plataformas coinciden, el hallazgo no es artefacto de ninguna
(«contraste», en la introducción de este README).

**Producto 3: consolidados por celda en el traslape.** Para cada par se
recalculan los consolidados por celda de las dos plataformas **sobre los
años del traslape** (el periodo de referencia es un parámetro justamente
para esto), con la misma grilla y los mismos umbrales, y se derivan:

| Código | Definición | Unidad |
|---|---|---|
| `ACUERDO` | Clase de cada celda con fuego en alguna de las dos: índices en ambas; solo en `B`; solo en `A`; en ninguna (bajo el umbral en las dos); estación discordante (sin estación definida en una sola) | categoría |
| `ΔINI`, `ΔFIN`, `ΔLON` | `B` − `A` en las celdas con índices en ambas | días |
| `ρ(DENS)` | Correlación de Spearman entre las densidades de las dos plataformas sobre todas las celdas con al menos 10 km² de tierra, con `DENS` recalculada sobre el traslape | 0–1 |

`ACUERDO` dice dónde se puede comparar y dónde no: las celdas «solo en
`B`» son el mapa de lo que el píxel fino añade. Las diferencias por celda
se resumen con su mediana, su rango intercuartílico y la fracción de
celdas con `|ΔINI|` de 15 días o menos. `ρ(DENS)` mide si las dos
plataformas dibujan la misma geografía del fuego aunque cuenten
detecciones distintas; es una comparación de orden, no de nivel, y por
eso es la única que involucra `DENS`. `FREC` no se compara por celda: en
un traslape corto es un múltiplo de una fracción pequeña y el píxel fino
la sesga hacia arriba.

**Lectura (traslape 2013–2025, MODIS → S-NPP).** La expectativa del
adelanto no se cumple: a escala nacional VIIRS fecha `INI` 14 días
**después** que MODIS en mediana (de 2 a 28, en todos los años), el fin
coincide (0 días, ρ 0,94) y la temporada resulta 15 días más corta. Por
celda, en cambio, los inicios casi coinciden (mediana de `ΔINI` +2 días,
83 % de las celdas a 15 días o menos), así que el rezago nacional es un
efecto de composición: las detecciones que VIIRS añade se concentran en
las celdas y los días del pico, y ese peso desplaza el 10 % acumulado del
país. Las dos plataformas ordenan igual los años (ρ 0,66 a 0,94 según el
índice), coinciden en signo en el 92 % de las anomalías de `DTOT` y `ND95`
(ρ 0,98 y 0,76) y dibujan la misma geografía del fuego (ρ de `DENS`
entre celdas 0,89). De 457 celdas con fuego, 107 son comparables, 144
tienen índices solo en VIIRS y ninguna solo en MODIS. El par de control
S-NPP → NOAA-20 difiere en 0 días de inicio, 1 de longitud y tiene ρ de
`DENS` 0,96: la diferencia con MODIS es del píxel, no de las pasadas.

**Lo que no se hace.** No hay serie intercalibrada ni factor de
conversión entre plataformas; las diferencias se publican como tales.
No se comparan `FRPI` ni `FRP95` (Li et al. 2018) ni los umbrales `P95`.
Los productos de comparación no alimentan ningún índice de las
plataformas ni se actualizan con la cola en tiempo casi real: cambian
solo cuando se cierra un año de fuego completo en ambas.

### Salidas

Todas las salidas existen por plataforma (MODIS, VIIRS S-NPP y VIIRS
NOAA-20), en `outputs/<tipo>/<plataforma>/`, y aparecen en el reporte de
cada una.

- Tabla con una fila por año de fuego: año, marca de parcial/provisional,
  `DTOT`, `INI`, `FIN` (día y fecha) y `LON`. Es el CSV publicado.
- Tabla de detecciones excluidas por tipo y año.
- Figura de temporada: un segmento por año de `INI` a `FIN`, ordenado
  cronológicamente, con enero a mayo (SINAC) y diciembre a abril (IMN) como
  bandas de referencia.
- Ráster consolidado: una capa por celda de 0,1° para `INI`, `FIN`, `LON`,
  `FUERA`, `N50F`, `FREC`, `DENS`, `FRPI`, `AQ` y `DTOT` (auxiliar), en GeoTIFF con la plataforma, el
  periodo de referencia y los umbrales en los metadatos, acompañado de un
  estilo `.qml` de QGIS con la simbología de `LON` (sin él, QGIS abre el
  ráster como color multibanda con las tres primeras bandas); mapas estáticos de
  `LON`, `INI`, `FIN` y `N50F` con las celdas bajo el umbral en gris y las
  celdas sin estación definida con trama, y las mismas celdas como capa del
  mapa interactivo.
- Concentración anual: columnas `DF`, `N50` y `C10` en la tabla por año de
  fuego, y una figura de barras por año con `N50` y `C10`.
- Días extremos: columnas `ND95`, `D95p`, `D95pTOT` y `P95` (el umbral,
  repetido en cada fila para que el CSV se explique solo) en la tabla por
  año de fuego, la marca de año no comparable en la nota, y una figura de
  barras por año de `ND95` y `D95pTOT`.
- Intensidad: columnas `FRPI`, `FRP95`, `AQ` y `NOC` en la tabla por año de
  fuego, con una figura de barras por año de `FRPI` y `FRP95`; capas `FRPI`
  y `AQ` en el GeoTIFF con sus mapas estáticos, y ambos valores en la ficha
  del mapa interactivo.
- Frecuencia y densidad: capas `FREC` y `DENS` en el mismo GeoTIFF y
  columnas `AREA`, `ANIOS`, `FREC` y `DENS` en la tabla por celda, que pasa
  a incluir todas las celdas de la grilla (las sin fuego con ceros y los
  demás índices en NA); mapas estáticos de `FREC` y `DENS`; ambos valores
  en la ficha del mapa interactivo.
- Comparación en el traslape, por par de plataformas y en
  `outputs/<tipo>/comparacion/`: tabla anual lado a lado con diferencias y
  anomalías estandarizadas (CSV), tabla por celda con los consolidados de
  ambas, las diferencias y la clase de acuerdo (CSV), GeoTIFF con las
  bandas `acuerdo`, `dif_ini`, `dif_fin` y `dif_lon`, figuras de índices
  anuales y de anomalías por año, y mapas de acuerdo y de `ΔINI` y `ΔLON`.
  Se publican en un reporte propio, `comparacion/`, enlazado desde la
  portada.

### Referencias

- Archibald, S., Lehmann, C. E. R., Gómez-Dans, J. L. y Bradstock, R. A.
  (2013). Defining pyromes and global syndromes of fire regimes. *PNAS*,
  110(16), 6442–6447. <https://doi.org/10.1073/pnas.1211466110>
- Balch, J. K. et al. (2022). Warming weakens the night-time barrier to
  global fire. *Nature*, 602, 442–448.
  <https://doi.org/10.1038/s41586-021-04325-1>
- Benali, A. et al. (2017). Bimodal fire regimes unveil a global-scale
  anthropogenic fingerprint. *Global Ecology and Biogeography*, 26,
  799–811. <https://doi.org/10.1111/geb.12586>
- Boschetti, L. y Roy, D. P. (2008). Defining a fire year for reporting and
  analysis of global interannual fire variability. *Journal of Geophysical
  Research: Biogeosciences*, 113, G03020.
  <https://doi.org/10.1029/2008JG000686>
- Chuvieco, E., Giglio, L. y Justice, C. (2008). Global characterization of
  fire activity: toward defining fire regimes from Earth observation data.
  *Global Change Biology*, 14(7), 1488–1502.
  <https://doi.org/10.1111/j.1365-2486.2008.01585.x>
- Cunningham, C. X., Williamson, G. J. y Bowman, D. M. J. S. (2024).
  Increasing frequency and intensity of the most extreme wildfires on Earth.
  *Nature Ecology & Evolution*, 8(8), 1420–1425.
  <https://doi.org/10.1038/s41559-024-02452-2>
- Dunning, C. M., Black, E. C. L. y Allan, R. P. (2016). The onset and
  cessation of seasonal rainfall over Africa. *Journal of Geophysical
  Research: Atmospheres*, 121.
  <https://doi.org/10.1002/2016JD025428>
- Ferreira, L. N., Vega-Oliveros, D. A., Zhao, L., Cardoso, M. F. y Macau,
  E. E. N. (2020). Global fire season severity analysis and forecasting.
  *Computers & Geosciences*.
  <https://www.sciencedirect.com/science/article/abs/pii/S0098300419302808>
- Fu, Y., Li, R., Wang, X., Bergeron, Y., Valeria, O. y Chavardès, R. D.
  (2020). Fire detection and fire radiative power in forests and
  low-biomass lands in Northeast Asia: MODIS versus VIIRS fire products.
  *Remote Sensing*, 12(18), 2870. <https://doi.org/10.3390/rs12182870>
- Giglio, L. (2007). Characterization of the tropical diurnal fire cycle
  using VIRS and MODIS observations. *Remote Sensing of Environment*,
  108(4), 407–421. <https://doi.org/10.1016/j.rse.2006.11.018>
- Giglio, L., Schroeder, W. y Justice, C. O. (2016). The collection 6
  MODIS active fire detection algorithm and fire products. *Remote Sensing
  of Environment*, 178, 31–41. <https://doi.org/10.1016/j.rse.2016.02.054>
- Giglio, L., Csiszar, I. y Justice, C. O. (2006). Global distribution and
  seasonality of active fires as observed with the Terra and Aqua MODIS
  sensors. *Journal of Geophysical Research: Biogeosciences*, 111, G02016.
  <https://doi.org/10.1029/2005JG000142>
- Ichoku, C., Giglio, L., Wooster, M. J. y Remer, L. A. (2008). Global
  characterization of biomass-burning patterns using satellite measurements
  of fire radiative energy. *Remote Sensing of Environment*, 112(6),
  2950–2962. <https://doi.org/10.1016/j.rse.2008.02.009>
- Li, F., Zhang, X., Kondragunta, S. y Csiszar, I. (2018). Comparison of
  fire radiative power estimates from VIIRS and MODIS observations.
  *Journal of Geophysical Research: Atmospheres*, 123(9), 4545–4563.
  <https://doi.org/10.1029/2017JD027823>
- Liebmann, B. et al. (2012). Seasonality of African precipitation from 1996
  to 2009. *Journal of Climate*, 25, 4304–4322.
  <https://doi.org/10.1175/JCLI-D-11-00157.1>
- Martín-Vide, J. (2004). Spatial distribution of a daily precipitation
  concentration index in peninsular Spain. *International Journal of
  Climatology*, 24(8), 959–971. <https://doi.org/10.1002/joc.1030>
- Schroeder, W., Oliva, P., Giglio, L. y Csiszar, I. A. (2014). The New
  VIIRS 375 m active fire detection data product: Algorithm description
  and initial assessment. *Remote Sensing of Environment*, 143, 85–96.
  <https://doi.org/10.1016/j.rse.2013.12.008>
- SINAC (2012). *Estrategia Nacional de Manejo Integral del Fuego en Costa
  Rica 2012–2021*. Sistema Nacional de Áreas de Conservación, MINAE.
  <https://www.sinac.go.cr/ES/partciudygober/Documents/Estrategia%20Nacional%20Manejo%20del%20Fuego.pdf>
- Villalobos Flores, R., Retana, J. A. y Acuña, A. (s. f.). *El Niño y los
  incendios forestales en Costa Rica*. Instituto Meteorológico Nacional,
  Gestión de Desarrollo (datos hasta 2000).
  <https://www.imn.ac.cr/documents/10179/20911/El+Ni%C3%B1o+y+los+incendios+forestales>
- Wooster, M. J., Roberts, G., Perry, G. L. W. y Kaufman, Y. J. (2005).
  Retrieval of biomass combustion rates and totals from fire radiative power
  observations: FRP derivation and calibration relationships between biomass
  consumption and fire radiative energy release. *Journal of Geophysical
  Research: Atmospheres*, 110, D24311. <https://doi.org/10.1029/2005JD006318>
- Zhang, X., Hegerl, G., Zwiers, F. W. y Kenyon, J. (2005). Avoiding
  inhomogeneity in percentile-based indices of temperature extremes.
  *Journal of Climate*, 18(11), 1641–1651.
  <https://doi.org/10.1175/JCLI3366.1>
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
├── R/temporada.R       # año de fuego e índices de temporada (LON) y ráster
├── R/grilla.R          # grilla de análisis común (0,05° y 0,1°, WGS84)
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
- Incorporar NOAA-21 a la suite de índices y a la comparación cuando FIRMS
  publique su procesamiento estándar.
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
