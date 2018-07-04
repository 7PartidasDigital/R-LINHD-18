####                     Análisis de Sentimientos de Textos Literarios                  ####
#                                  La formas de las historias                              #
#                                                                                          #
#                    Qué es la estilometría. R como lenguaje para analizar textos          #
#                       Stylometry? R as a computer language to analyze texts              #
#                                    DH@MADRID SUMMER SCHOOL                               #
#                                      10 DE JULIO DE 2018                                 #
#                                                                                          #
#                                                                                          #
#                                                                                          #
#                                        Basado en                                         #
#              Julia Silge: https://juliasilge.com/blog/if-i-loved-nlp-less/               #
#                                            y                                             #
#             Danny Murillo Lanza: http://uvadoc.uva.es/handle/10324/25437                 #
#                                                                                          #
#                         Revisado, reescrito y actualizado por                            #
#                               José Manuel Fradejas Rueda                                 #
#                                       julio 2018                                         #
#                                                                                          #
# USO                                                                                      #
# Esta versión permite analizar cualquier texto español. Lo considera en secciones de      #
# 400 palabras, con lo que viene a equivaler a una página de una edición de bolsillo       #
# algo muy variable). Utiliza el lexicón nrc tal y como lo ha implementado M.L. Jockers    #
# en syuzhet 1.0.4 (diciembre 2017). Están en desarrollo las traducciones de los otros     #
# diccionarios utilizados en syuzhet (bing, afinn y syuzhet).                              #
# Sustitúyase TITULO NOVELA en todos los literales en los que aparezca por el título       #
# del texto que se esté analizando.                                                        #
#                                                                                          #
# Nota                                                                                     #
# La cantidad de validación del texto no es aún óptima dado que en el diccionario nrc      #
# implentado en el paquete syuzhet no se tiene encuenta las palabras en plural ni en       #
# femenino y los verbos, salvo que sean participios, aparecen en infinitivo.               #
# Una manera de solventar este problema sería utlizando textos lematizados, o bien         #
# incorporar todas las formas posibles al diccionario.                                     #
#                                                                                          #
#  Proyecto 7PartidasDigital "Edición crítica digital de las Siete Partidas de Alfonso X"  #
#        Proyecto financiado por el MINECO, referencia FFI2016-75014-P AEI-FEDER, EU       #
#                Universidad de Valladolid -- IP José Manuel Fradejas Rueda                #
#                              https://7partidas.hypotheses.org/                           #
#                             https://github.com/7PartidasDigital                          #
#                         Este material se distribuye con una licencia                     #
#                                            MIT                                           #
#                                         v. 1.0.0                                         #

# Establece el directorio de trabajo
setwd("~/Desktop/R-LINHD-18")


# Cargar librerías
library(readr)
library(stringr)
library(syuzhet)
library(dplyr)
library(reshape2)
library(ggplot2)
# Lee el texto.
# Usaremos Los cuatro jinetes del Apocalipsis de V. Blasco Ibáñez, pero tienes otros dos textos más
# para probar: Trafalgar de B.Pérez Galdós, y los Pazos de Ulloa de E. Pardo Bazán
texto_entrada <- readLines("cuatro_jinetes_apocalipsis.txt") # Pon el nombre del fichero adecuado
texto_unido <- paste(texto_entrada, collapse = " ") # Funde el texto e una sola cadena
texto_palabras <- unlist(strsplit(texto_unido, " ")) # Lo divide en palabras
long_maxima <- length(texto_palabras)/400 # Establece el número de páginas en que lo debe dividir
x <- seq_along(texto_palabras)
texto_paginas <- split(texto_palabras, ceiling(x/long_maxima))
# Crea las páginas
texto_paginas <- lapply(texto_paginas, function(x) paste(x, collapse = " "))
# Convierte el texto a minúsculas
texto_paginas <- tolower(as.character(unlist(texto_paginas)))
# Calcula el sentimiento 
texto_nrc <- cbind(NumPag = seq_along(texto_paginas), get_nrc_sentiment(texto_paginas, language = "spanish"))
# cambia el nombre de las columnas para que sean fácilmente comprensibles
colnames(texto_nrc) <- c("NumPag","ira", "expectación","disgusto","miedo","alegría","tristeza","sorpresa","confianza","negativo","positivo")
# Convierte a números negativos la columna negativa
texto_nrc$negativo <- -texto_nrc$negativo
# Toma las columna numero de línea, positiva y negativa y crea una nueva
# tabla de tres columnas en las que los positivos y negativos están en una
# misma columna, pero guarda de donde procede cada positivo y negativo
pos_neg <- texto_nrc %>% select(NumPag, positivo, negativo) %>% 
  melt(id = c("NumPag"))
# renombra las columnas
names(pos_neg) <- c("NumPag", "sentimiento", "valor")



# Desde aquí dibuja varios gráficos 

# Primer gráfico
# Cámbiese el literal de la línea 93 por el título adecuado
ggplot(data = pos_neg, aes(x = NumPag, y = valor, color = sentimiento)) +
  geom_point(size = 4, alpha = 0.5) + theme_minimal() +
  ylab("Sentimento") + 
  ggtitle(expression(paste("Sentimiento positivo y negativo en ", 
                           italic("Los cuatro jinetes del Apocalipsis")))) +
  theme(legend.title=element_blank()) + 
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  theme(legend.justification=c(7,0), legend.position=c(1, -0.05)) +
  scale_color_manual(values = c("aquamarine3", "midnightblue"))


# Segundo gráfico
# Cámbiese el literal de la línea 108 por el título adecuado
ggplot(data = pos_neg, aes(x = NumPag, y = valor, color = sentimiento, fill = sentimiento)) +
  geom_bar(stat = "identity", position = "dodge") + theme_minimal() +
  ylab("Sentimiento") +
  ggtitle(expression(paste("Sentimiento positivo y negativo en ", 
                           italic("Los cuatro jinetes del Apocalipsis")))) +
  theme(legend.title=element_blank()) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  theme(legend.justification=c(6.98,0), legend.position=c(1, -0.05)) +
  scale_fill_manual(values = c("aquamarine3", "midnightblue")) +
  scale_color_manual(values = c("aquamarine3", "midnightblue"))

# Finalizan los dos primeros gráficos
# Recalcula el sentimiento para abordar otros gráficos más interesantes

texto_nrc <- data.frame(cbind(NumPag = seq_along(texto_paginas), 
                                   sentimiento = get_sentiment(texto_paginas, method = "nrc", language = "spanish")))

# Tercer gráfico
### Cámbiese el literal de la línea 129 por el título adecuado
ggplot(data = texto_nrc, aes(x = NumPag, y = sentimiento)) +
  geom_bar(stat = "identity", position = "dodge", color = "midnightblue") + 
  theme_minimal() +
  ylab("Sentimiento") +
  ggtitle(expression(paste("Sentimiento en ", italic("Los cuatro jinetes del Apocalipsis")))) +
  theme(axis.title.x=element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  theme(axis.text.x=element_blank()) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))


# Cuarto gráfico
# Aplica una fórmula de transformación diseñada por Jockers.
# Producirá un Warning. No pasa nada, es un aviso de que
# hay una fórmula mejor y que es la que se debería utilizar.
# Para nuestros propósitos es suficiente.

texto_ft <- as.numeric(get_transformed_values(texto_nrc$sentimiento, 
                                             low_pass_size = 3,
                                             scale_vals = TRUE,
                                             scale_range = FALSE))
texto_ft <- data.frame(cbind(NumPag = seq_along(texto_ft), ft = texto_ft))

# Cuarto gráfico
# Cámbiese el literal de las líneas 157 y 159 por el título adecuado
# Si se quita geom_bar y theme_minimal, solo imprime la línea suavizada, lo que
# se logra anteponiendo una almohadilla a dichas líneas.
# Si se borran las almohadilla de las líneas 158-162 el aspecto del gráfico cambia

ggplot(data = texto_ft, aes(x = NumPag, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8, color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(title ="Forma de la historia en Los cuatro jinetes del Apocalipsis", subtitle="NRC", x = "Tiempo narrativo", y = "Transformación Valorada del Sentimiento") +
  #ylab("Transformación Valorada del Sentimiento") +
  #ggtitle(expression(paste("Sentimientos en ", italic("Los cuatro jinetes del Apocalipsis")))) +
  #theme(axis.title.x=element_blank()) +
  #theme(axis.ticks.x=element_blank()) +
  #theme(axis.text.x=element_blank()) +
  geom_smooth(se=FALSE) # se=FALSE borra lo gris del smooth

