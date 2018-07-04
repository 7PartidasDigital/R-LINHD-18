################################# TOPIC MODELLING CON TIDYTEXT ##############################
#                                                                                           #
#                    Qué es la estilometría. R como lenguaje para analizar textos           #
#                       Stylometry? R as a computer language to analyze texts               #
#                                    DH@MADRID SUMMER SCHOOL                                #
#                                      10 DE JULIO DE 2018                                  #
#                                                                                           #
#                                                                                           #
#                                         Basado en                                         #
#                                                                                           #
#       https://cran.r-project.org/web/packages/tidytext/vignettes/topic_modeling.html      #
#                                                                                           #
#                                 Adaptación desarrollada por                               #
#                                                                                           #
#                                 José Manuel Fradejas Rueda                                #
#                              Departamento de Lengua Española                              #
#                                 Universidad de Valladolid                                 #
#  Proyecto 7PartidasDigital "Edición crítica digital de las Siete Partidas de Alfonso X"   #
#        Proyecto financiado por el MINECO, referencia FFI2016-75014-P AEI-FEDER, EU        #
#                Universidad de Valladolid -- IP José Manuel Fradejas Rueda                 #
#                              https://7partidas.hypotheses.org/                            #
#                             https://github.com/7PartidasDigital                           #
#                         Este material se distribuye con una licencia                      #
#                                            MIT                                            #
#                                         v. 1.0.0                                          #
#                                                                                           #


# Establece directorio de trabajo
setwd("~/Desktop/R-LINHD-18")

# Carga las librerías necesarias
library(tidyverse)
library(tidytext) 
library(tm)
library(topicmodels)

# Carga la lista de stopwords españolas ya que tidytext solo las tiene en inglés
# Carga los datos para puentear los ingleses
load("datos_esp-small.rda")

# Lee los textos
load("topico.rda")

# Divide por capítulos. Solo válido para aquellos textos en los que los capítulos
# estén marcados con el término CAPÍTULO. No importa si está en mayúscula, minúscula
# o si está correctamente acentuado.

por_capitulo <- novelas %>%
  group_by(titulo) %>%   
  mutate(capitulo = cumsum(str_detect(texto, regex("^cap[í|i]tulo ", ignore_case = TRUE)))) %>%   
  ungroup() %>%   
  filter(capitulo > 0)  
por_capitulo

# Divide (tokeniza) en palabras por capítulo
por_capitulo_palabras <- por_capitulo %>%   
  unite(titulo_capitulo, titulo, capitulo) %>%                 
  unnest_tokens(palabra, texto)  
# Mira como queda ahora
por_capitulo_palabras

# Echa una ojeada a las palabras más frecuentes. En otros análisis pueden ser útiles
# pero para hacer un modelado de tópicos de nada nos sirven las llamadas paalbras
# vacías
por_capitulo_palabras %>%
  count(palabra, sort = T)

# Elimina palabras vacías
palabra_conteo <- por_capitulo_palabras %>%   
  anti_join(vacias_esp) %>%   
  count(titulo_capitulo, palabra, sort = TRUE) %>%   
  ungroup()  

# Y ahora podemos saber cuántas palabras de "valor" hay en cada capítulo y cuál es su frecuencia
palabra_conteo

# En este momento, este dataframe está ordenado, con un término por documento por fila. 
# Sin embargo, el paquete TOPICMODELS requiere un DocumentTermMatrix (del paquete TM). 
# Se logra la adaptación a DocumentTermMatrix con cast_dtm de tidytext:
capitulos_dtm <- palabra_conteo %>%
  cast_dtm(titulo_capitulo, palabra, n)

capitulos_dtm

# Ahora estás listo para usar el paquete topicmodels y crear un modelo LDA 
# con varios tópicos. Lo vas hacer con uno por cada título.
# Sin embargo, si se requiere un mayor número de tópicos al de títulos de obras, entonces,
# el valor de k = 4.
# Para hacer el modelo para hacer el modelo más águil: length(unique(novelas$titulo)) quizá

capitulos_lda <- LDA(capitulos_dtm, k = 4, control = list(seed = 1234)) # Ojo al valor de k
capitulos_lda

# (En este caso, sabemos que hay 4 temas porque hay 4 libros, 
# en la práctica es posible que tengamos que probar algunos valores diferentes para k).
# Ahora tidytext da la opción de volver a un análisis ordenado, 
# utilizando las voces ordenadas.

capitulos_lda_td <- tidy(capitulos_lda, matrix = "beta")
capitulos_lda_td

# Para cada combinación, el modelo decide la probabilidad de que ese término se genere a partir de ese tema.
# Podríamos usar top_n de dplyr para encontrar los 5 términos principales dentro de cada tema:

terminos_frecuentes <- capitulos_lda_td %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

terminos_frecuentes # Si hay más de 4 títulos, usa la siguiente línea para visulaizarlo (pero tendrás que borrar el #)
# as.data.frame(terminos_frecuentes) # De esta forma permite ver todos los topicos cuando la lista es larga (más de 10)

# El modelo se presta a una visualización:

library(ggplot2)
theme_set(theme_bw())


terminos_frecuentes %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# ¡Estos temas están claramente asociados con los cuatro libros!
# El mayor problema es que algunos de los términos de tópico son, en realidad,
# nombres de los personajes, y eso aunque puede servir para identificar la obra,
# de acuerdo con la "fabulación" de Silge, es inútil para establecer los tópico
# de cada novela. Esto llevaría a la creación de una data.frame con los términos
# extra que hay que eliminar … Está en proceso.

# Clasificación por documento

# Cada capítulo es un "documento" en este análisis. Por lo tanto, es posible que deseemos 
# saber qué temas están asociados con cada documento. 

# Surge una cuestión importante: ¿Podemos juntar los capítulos en los libros correctos?

capitulos_lda_gamma <- tidy(capitulos_lda, matrix = "gamma")
capitulos_lda_gamma

# Configuración de matriz = "gamma" devuelve una versión ordenada con un documento 
# y podemos ver el grado de acierto del aprendizaje no supervisado para distinguir 
# entre los libros considerados.
# Primero se ha de separar el nombre del documento en título y número de capítulo:

capitulos_lda_gamma <- capitulos_lda_gamma %>%
  separate(document, c("titulo", "capitulo"), sep = "_", convert = TRUE)
capitulos_lda_gamma


# Cuando examinamos resultados
ggplot(capitulos_lda_gamma, aes(gamma, fill = factor(topic))) +
  geom_histogram() +
  facet_wrap(~ titulo, nrow = 2)

# Notamos que casi todos los capítulos de Orgullo y prejuicio, 
# La guerra de los mundos y Veinte mil leguas de viaje submarino se identificaron 
# de manera única como un único tópico (libro).

capitulo_clasificaciones <- capitulos_lda_gamma %>%
  group_by(titulo, capitulo) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(gamma)

capitulo_clasificaciones

# Lo siguiente confirma numéricamente lo que se ha visto en el gráfico anterior
# ya que busca el libro de consenso para cada tópico

topicos_libro <- capitulo_clasificaciones %>%
  count(titulo, topic) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = titulo, topic)

topicos_libro

# Ahora vemos qué capítulos fueron identificados erróneamente:

capitulo_clasificaciones %>%
  inner_join(topicos_libro, by = "topic") %>%
  count(titulo, consensus)

# Vemos que solo unos pocos capítulos de Grandes esperanzas fueron clasificados erróneamente. 

# Por asignaciones de palabras: augment

# Un paso importante es asignar cada palabra de cada documento a un tema. 
# Cuantas más palabras en un documento se asignan a ese tema, en general, más peso (gamma) 

asignaciones <- augment(capitulos_lda, data = capitulos_dtm)

asignaciones

# Podemos combinar esto con los títulos del libro para ver qué  palabras fueron clasificadas incorrectamente.

asignaciones <- asignaciones %>%
  separate(document, c("titulo", "capitulo"), sep = "_", convert = TRUE) %>%
  inner_join(topicos_libro, by = c(".topic" = "topic"))

asignaciones

# Para visualizar todos los resultados hay que dar unos pequeños pasos. Si los quieres dímelo.


# Podemos crear una "matriz de confusión"

asignaciones %>% 
  count(titulo, consensus, wt = count) %>%
  spread(consensus, n, fill = 0)

# Como puedes ver solos unas palabras de Grandes esperanzas no se asignaron al libro correcto.

# ¿Cuáles fueron las palabras más comúnmente confundidas?

palabras_equivocadas <- asignaciones %>%
  filter(titulo != consensus)

palabras_equivocadas

palabras_equivocadas %>%
  count(titulo, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

# La última columna indica cuantas ocurrencias hay de cada palabra errónea y cuál es.
# Para ver una lista ampliada usa esta líneas:
as.data.frame(palabras_equivocadas %>%
                count(titulo, consensus, term, wt = count) %>%
                ungroup() %>%
                arrange(desc(n)))

# Observa la palabra "flopson":  esta palabra equivocada no aparece necesariamente en las
# novelas a las que se asignó incorrectamente. De hecho, se puede confirmar que "flopson"
# aparece solo en Great Expectations (Grandes esperanzas).
# No funcionará nunca en español salvo si se tiene Grandes esperanzas en el elenco de textos.
# Lo importante es que el modelo funciona con textos en español y en local, no solo con textos en inglés
# descargados de Gutenberg.
palabra_conteo %>%
  filter(palabra == "flopson")
# pero esta instrucción permite localizar ciertos términos dentro de cada texto y dentro de él, en
# cada capítulo. Véase por ejemplo
palabra_conteo %>%
  filter(palabra == "nemo")
palabra_conteo %>%
  filter(palabra == "pocket")
