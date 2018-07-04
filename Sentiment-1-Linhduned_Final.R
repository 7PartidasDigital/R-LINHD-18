#########################             ANÁLISIS SENTIMIENTOS          ########################
#                                          HARRY POTTER                                     #
#                                                1                                          #
#                                                                                           #
#                    Qué es la estilometría. R como lenguaje para analizar textos           #
#                       Stylometry? R as a computer language to analyze texts               #
#                                    DH@MADRID SUMMER SCHOOL                                #
#                                      10 DE JULIO DE 2018                                  #
#                                                                                           #
#                                                                                           #
#                                            Basado en                                      #
#                           UC Business Analytics R Programming Guide                       #
#                                Text Mining: Sentiment Analysis                            #
#                            http://uc-r.github.io/sentiment_analysis                       #
#                                                                                           #
#                                 Desarrollado y adaptado por                               #
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


# Establece el directorio de trabajo
setwd("~/Desktop/R-LINHD-18")
# Carga liberías necesarias
library(tidyverse)
library(tidytext)

# Lee los textos
load("harry_txt.rda")

# carga sentimietos español
load("datos_esp-small.rda")

# carga sustituto de get_sentiments
source("get_sentiments.R")

# Parepara los textos para poder seguirle la pista a cada palabra de cada capítulo de cada una de las entregas.
titulos <- c("La piedra filosofal", "La cámara secreta", "El prisionero de Azkabán", "El cáliz de fuego", "La órden del Fénix", "El misterio del príncipe", "Las reliquias de la muerte")
libros <-list(piedra_filosofal, camara_secreta, prisionero_azkaban, caliz_fuego, orden_fenix, misterio_principe, reliquias_muerte)
serie <- tibble()
for(i in seq_along(titulos)) {
  limpio <- tibble(capitulo = seq_along(libros[[i]]),
                   texto = libros[[i]]) %>%
    unnest_tokens(palabra, texto) %>%
    mutate(libro = titulos[i]) %>%
    select(libro, everything())
  serie <- rbind(serie, limpio)
}
serie$libro <- factor(serie$libro, levels = rev(titulos))

# Esto dice cuántas palabras positivas o negativas hay (emociones si nrc)
serie %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentimiento)) %>%
  count(sentimiento, sort = TRUE)

# Esto dibuja las gráficas de positivo / negativo
serie %>%
  group_by(libro) %>%
  mutate(recuento_palabras = 1:n(),
         indice = recuento_palabras %/% 500 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(libro, indice = indice , sentimiento) %>%
  ungroup() %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = positivo - negativo,
         libro = factor(libro, levels = titulos)) %>%
  ggplot(aes(indice, sentimiento, fill = libro)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ libro, ncol = 2, scales = "free_x")


# Ahora troceado en secciones de 500 palabras
serie %>%
  group_by(libro) %>%
  mutate(recuento_palabras = 1:n(),
         indice = recuento_palabras %/% 500 + 1) %>%
  inner_join(get_sentiments("bing")) %>%
  count(libro, indice = indice , sentimiento) %>%
  ungroup() %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = positivo - negativo,
         libro = factor(libro, levels = titulos)) %>%
  ggplot(aes(indice, sentimiento, fill = libro)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ libro, ncol = 2, scales = "free_x")

afinn <- serie %>%
  group_by(libro) %>%
  mutate(recuento_palabras = 1:n(),
         indice = recuento_palabras %/% 500 + 1) %>%
  inner_join(get_sentiments("AFINN")) %>%
  group_by(libro, indice) %>%
  summarise(sentimiento = sum(valor)) %>%
  mutate(metodo = "AFINN")

bing_Y_nrc <- bind_rows(serie %>%
                          group_by(libro) %>%
                          mutate(recuento_palabras = 1:n(),
                                 indice = recuento_palabras %/% 500 + 1) %>%
                          inner_join(get_sentiments("bing")) %>%
                          mutate(metodo = "Bing"),
                        serie %>%
                          group_by(libro) %>%
                          mutate(recuento_palabras = 1:n(),
                                 indice = recuento_palabras %/% 500 + 1) %>%
                          inner_join(get_sentiments("nrc") %>%
                                       filter(sentimiento %in% c("positivo", "negativo"))) %>%
                          mutate(metodo = "NRC")) %>%
  count(libro, metodo, indice = indice , sentimiento) %>%
  ungroup() %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = positivo - negativo) %>%
  select(libro, indice, metodo, sentimiento)

bind_rows(afinn, bing_Y_nrc) %>%
  ungroup() %>%
  mutate(blibro = factor(libro, levels = titulos)) %>%
  ggplot(aes(indice, sentimiento, fill = metodo)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_grid(libro ~ metodo)

recuenta_palabras_bing <- serie %>%
  inner_join(get_sentiments("bing")) %>%
  count(palabra, sentimiento, sort = TRUE) %>%
  ungroup()

recuenta_palabras_bing

recuenta_palabras_bing %>%
  group_by(sentimiento) %>%
  top_n(10) %>%
  ggplot(aes(reorder(palabra, n), n, fill = sentimiento)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentimiento, scales = "free_y") +
  labs(y = "Contribución al sentimiento", x = NULL) +
  coord_flip()


# Ahorael texto se va a dividir en oraciones y se va a calcular el sentimiento
# de cada una de las oraciones. Al final se obtendrá una gráfico en el que por
# medio de unos cuadritos podrás ver el mayor o menor valor de cada una de las
# oraciones en cada uno de los capítulos de La piedra filosofal, pero si cambias
# el nombre los objetos --piedra_filosofal-- por cualquiera de los otros de la
# serie tendrás el análisis de ese título.
pf_oraciones <- tibble(capitulo = 1:length(piedra_filosofal),
                       texto = piedra_filosofal) %>%
  unnest_tokens(oracion, texto, token = "sentences")

pf_oraciones <- tibble(capitulo = 1:length(piedra_filosofal),
                       texto = piedra_filosofal) %>%
  unnest_tokens(oracion, texto, token = "sentences")


libro_examinado <- pf_oraciones %>%
  group_by(capitulo) %>%
  mutate(oracion_num = 1:n(),
         indice = round(oracion_num / n(), 2)) %>%
  unnest_tokens(palabra, oracion) %>%
  inner_join(get_sentiments("AFINN")) %>% # Aquí puedes usar cualquiera de los otros diccionarios: nrc o bing
  group_by(capitulo, indice) %>%
  summarise(sentimiento = sum(valor, na.rm = TRUE)) %>%
  arrange(desc(sentimiento))

# Aquí se dibuja el gráfico. Si cambias el título, cámbialo también un poco más abajo,
# Más que nada para que te quede claro qué titulo es.
ggplot(libro_examinado, aes(indice, factor(capitulo, levels = sort(unique(capitulo), decreasing = TRUE)), fill = sentimiento)) +
  geom_tile(color = "white") +
  scale_fill_gradient2() +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Tiempo narrativo", y = "Capítulo") +
  ggtitle("Sentimiento en 'La piedra filosofal'",
          subtitle = "Resumen de la puntuación neta del sentimiento según proegresa cada capítulo") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")
