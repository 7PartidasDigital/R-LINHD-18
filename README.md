# R-LINHD-18
Materiales del curso de verano de LinhdUned
### _¿Qué es la estilometría? R como lenguaje para analizar textos_
#### Madrid, 10 de julio de 2018

José Manuel **Fradejas Rueda**
Departamento de Lengua Española, [Universidad de Valladolid](http://www.uva.es/export/sites/uva/)

Antonio **Robles**
Departamento de Sistemas de Comunicación y Control, [UNED](http://portal.uned.es/portal/page?_pageid=93,1&_dad=portal&_schema=PORTAL)

#### Presentación
En este repositorio se encuentran todos los materiales que se emplearán en este curso en el que introduciremos a los alumnos en el análisis de sentimientos aplicados a la literatura española. Se ejemplificará con textos literarios en español y se contrastarán con los resultados que arrojan los mismos textos en inglés (cuando sea posible). Para poder llegar a ello, en la Universidad de Valladolid, el proyecto [7PartidasDigital](https://7partidas.hypotheses.org/), ha estado trabajando en la creación / modificación / traducción de los diccionarios de sentimientos instalados o empleados en dos paquetes básicos para estos análisis: [tidytext](https://cran.r-project.org/web/packages/tidytext/index.html) de Julia Silge y David Robinson y [syuzhet](https://cran.r-project.org/web/packages/syuzhet/index.html) de M.L. Jockers.



##### Recomendaciones
Lo mejor es descargar este repositorio como un `zip` y dejarlo en el escritorio del ordenador, o reubicarlo en donde mejor convenga, pero se ha de tener en cuenta que los scripts buscarán los datos en la carpeta descargada, se llame como se llame y se ubique en donde se ubique.

Quienes sean nuevos en el manejo de R, y para ahorrar tiempos muertos durante la sesión de trabajo, lo mejor es que instalen el software según se explica en los vídeos que se indican un poco más abajo. Ha de hacerse en la secuencia indicada (la explicación de cada vídeo es inferior a cinco minutos, pero el tiempo necesario para instalar el software y los componentes necesarios es un poco más largo):

[Instalar R](https://canal.uned.es/video/5b32192eb1111f771d8b456d)

[Instalar RStudio](https://canal.uned.es/video/5b32192fb1111f771d8b4570)

[Instalar Librerías en R](https://canal.uned.es/video/5b32192db1111f771d8b456a)

Este último vídeo necesita un fichero para instalar las librerías pertinentes que se llama `instalar-paquetes.txt`. Pero también se puede hacer con esta línea de código (cópiala y pégala en la consola de RStudio):

```R
install.packages(c("tidyverse","tidytext","scales","igraph","ggraph","wordcloud","syuzhet","stylo","XML","RCurl","tm","topicmodels"))
```

El orden en que se utilizarán los scripts en el curso es:

1. `MdT-Linhuned_Final.R`
2. `Sentiment-1-Linhduned_Final.R`
3. `Sentiment-2-Linhduned_Final.R`
4. `AS-SHAPE-STORIES-NRC.R`

Cada uno de estos scripts buscará en el directorio de trabajo los distintos componentes que necesite en cada momento, ya sean ficheros `.rda`, `.r` o `.txt`.

Un último aviso. Todos los ficheros son UTF-8, quienes utilicen sistemas basados en Windows deberán convertir a la codificación windows los ficheros pertinentes.

En el sitio web de los [Cursos de verano de la UNED](https://campusverano.innova.uned.es/register/?return_url=%2fdotlrn%2f) hay una copia para máquinas Windows y se encuentran todos las presentaciones que se utilizarán.
<p align="center">
<img src=https://f-origin.hypotheses.org/wp-content/blogs.dir/3658/files/2015/06/EXPLICIT-7PARTIDAS-e1495528094806.png>
</p>
