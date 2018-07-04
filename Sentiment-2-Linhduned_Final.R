#########################             ANÁLISIS SENTIMIENTOS          ########################
#                                          HARRY POTTER                                     #
#                                                2                                          #
#                                                                                           #
#                    Qué es la estilometría. R como lenguaje para analizar textos           #
#                       Stylometry? R as a computer language to analyze texts               #
#                                    DH@MADRID SUMMER SCHOOL                                #
#                                      10 DE JULIO DE 2018                                  #
#                                                                                           #
#                                                                                           #
#                                            Basado en                                      #
#                                        Paul van der Laken                                 #
#                                         Harry Plotter:                                    #
#                              Celebrating the 20 year anniversary                          #
#                             with tidytext and the tidyverse in R                          #
#                                                                                           #
# https://paulvanderlaken.com/2017/08/03/harry-plotter-celebrating-the-20-year-anniversary-with-tidytext-the-tidyverse-and-r/
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


# Establece el directorio de trabajo
setwd("~/Desktop/R-LINHD-18")
# Carga las librería necesarias
library(tidytext)
library(plyr)
library(tidyverse)
library(wordcloud)

# Lee los textos
load("harry_txt.rda")

# Carga sentimietos español
load("datos_esp-small.rda")

# Carga sustituto de get_sentiments
source("get_sentiments.R")

# Unas opciones
options(stringsAsFactors = F, # que no lo convierta en factores al cargar
        scipen = 999, # que no convierta los números a notación científica
        max.print = 200) # que no imprima más de 200 valores
theme_set(theme_light()) # Tema por defecto de ggplot = light
fs = 12 # Tamaño por defecto de la fuente

# Transforma los textos dividiéndolos en tokens e
# identifica el capítulo y libro de procedencia
hp_palabras <- list(
  piedra_filosofal = piedra_filosofal,
  camara_secreta = camara_secreta,
  prisionero_azkaban = prisionero_azkaban,
  caliz_fuego = caliz_fuego,
  orden_fenix = orden_fenix,
  misterio_principe = misterio_principe,
  reliquias_muerte = reliquias_muerte
) %>%
  ldply(rbind) %>% # Coloca todos los capítulos en una tabla
  mutate(libro = factor(seq_along(.id), labels = .id)) %>% # Los identifica con el libro correspondiente
  select(-.id) %>% # Borra la columna ID
  gather(key = 'capitulo', value = 'texto', -libro) %>% # Pasa las columnas de capítulo a filas
  filter(!is.na(texto)) %>% # Borra las filas en blanco
  mutate(capitulo = as.integer(capitulo)) %>% # Convierte la columna capítulo a enteros
  unnest_tokens(palabra, texto, token = 'words') # Tokeniza la dataframe



# Dibuja la frecuencia de las 15 palabras más frecuentes (eliminadas las vacías)
hp_palabras %>%
  group_by(libro, palabra) %>%
  anti_join(vacias_esp, by = "palabra") %>% # Borra las palabras vacías
  count() %>% # Hace el recuento de palabras por libro
  arrange(desc(n)) %>% # Sitúa las de mayor frecuencia en la parte alta de la tabla
  group_by(libro) %>% # 
  mutate(top = seq_along(palabra)) %>% # Identifica el rango dentro del grupo
  filter(top <= 15) %>% # Solo tiene en cuenta las 15 de mayor frecuencia
  # Dibuja el gráfico de barras
  ggplot(aes(x = -top, fill = libro)) + 
  geom_bar(aes(y = n), stat = 'identity', col = 'black') +
  # Se asegura que la palabra se escribe dentro de la barra o al lado
  geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
                label = palabra), size = fs/3, hjust = "left") +
  theme(legend.position = 'none', # Borra la leyenda
        text = element_text(size = fs), # Determina el tamaño de la fuente
        axis.text.x = element_text(angle = 45, hjust = 1, size = fs/1.5), # Gira el eje X
        axis.ticks.y = element_blank(), # Borra la marcas del eje Y
        axis.text.y = element_blank()) + # Borra el texto del eje y
  labs(y = "Recuento de palabras", x = "", # Añade nuevas leyendas
       title = "Harry Potter: Palabras más frecuentes a lo largo de toda la saga",
       subtitle = "Aplicando los principios tidyverse y tidytext") +
  facet_grid(. ~ libro) + # Traza el gráfico para cada libro
  coord_flip() # Gira el gráfico


# Calcula el sentimiento con los tres diccionarios
hp_sentimiento <- bind_rows(
  # 1 AFINN 
  hp_palabras %>% 
    inner_join(get_sentiments("AFINN"), by = "palabra") %>%
    filter(valor != 0) %>% # Borra las palabras neutras
    mutate(sentimiento = ifelse(valor < 0, 'negativo', 'positivo')) %>% # Identifica el sentimiento
    mutate(valor = sqrt(valor ^ 2)) %>% # Convierte todos los valores a positivos
    group_by(libro, capitulo, sentimiento) %>% 
    mutate(lexicon = 'AFINN'), # Crea el identificar de lexicón
  # 2 BING 
  hp_palabras %>% 
    inner_join(get_sentiments("bing"), by = "palabra") %>%
    group_by(libro, capitulo, sentimiento) %>%
    mutate(lexicon = 'bing'), # Crea el identificar de lexicón
  # 3 NRC 
  hp_palabras %>% 
    inner_join(get_sentiments("nrc"), by = "palabra") %>%
    group_by(libro, capitulo, sentimiento) %>%
    mutate(lexicon = 'nrc') # Crea el identificar de lexicón
)

# Nube de palabras
hp_sentimiento %>%
  group_by(palabra) %>%
  count() %>% # Resume los recuentos de cada palabra
  mutate(log_n = sqrt(n)) %>% # Calcula la raíz cuadrada para evitar los rasgos atípicos
  with(wordcloud(palabra, log_n, max.words = 100))

# Borra el sentimiento de los perosanjes
hp_sentimiento_sel <- hp_sentimiento %>% filter(!palabra %in% c("harry","moody"))
# Visualiza la palabra más frenceunte para cada sentimiento
hp_sentimiento_sel %>% # Excluye los nombres
  group_by(palabra, sentimiento) %>%
  count() %>% # sResume los recuentos de cada palabra para cada sentimiento
  group_by(sentimiento) %>%
  arrange(sentimiento, desc(n)) %>% # Las más frecuentes primero
  mutate(top = seq_along(palabra)) %>% # Identifica el rangos dentro del grupo
  filter(top <= 15) %>% # Selecciona solo las 15 más frecuentes
  ggplot(aes(x = -top, fill = factor(sentimiento))) + 
  # Crea el gráfico de barras
  geom_bar(aes(y = n), stat = 'identity', col = 'black') +
  # make sure words are printed either in or next to bar
  geom_text(aes(y = ifelse(n > max(n) / 2, max(n) / 50, n + max(n) / 50),
                label = palabra), size = fs/3, hjust = "left") +
  theme(legend.position = 'none', # Borra la leyenda
        text = element_text(size = fs), # Determina el tamñao de la fuente
        axis.text.x = element_text(angle = 45, hjust = 1), # Rota el eje X del texto
        axis.ticks.y = element_blank(), # Borra las marcas del eje Y
        axis.text.y = element_blank()) + # Borra el texto del eje Y
  labs(y = "Número de palabras", x = "", # Añade nuevas leyendas
       title = "Harry Potter: Palabras con carga emotiva a lo largo de la saga",
       subtitle = "Se utiliza tidytext y los diccionarios AFINN, bing y nrc vertidos al español") +
  facet_grid(. ~ sentimiento) + # Traza gráficos para cada sentimiento
  coord_flip() # Rota los ejes


# Visualiza los sentimiento POSTIVO/NEGATIVO a lo largo del tiempo narrativo
plot_sentimiento <- hp_sentimiento_sel %>% # Nombres excluidos
  group_by(lexicon, sentimiento, libro, capitulo) %>%
  summarize(valor = sum(valor), # Resume las puntuaciones AFINN
            count = n(), # Resume las puntuaciones bing y nrc
            # Manda los recuentos de bing y nrc a valor 
            valor = ifelse(is.na(valor), count, valor))  %>%
  filter(sentimiento %in% c('positivo','negativo')) %>%   # Solo conserva sentimiento bipolar
  mutate(valor = ifelse(sentimiento == 'negativo', -valor, valor)) %>% # Guarda los valores negativos
  # Crea el área del gráfico
  ggplot(aes(x = capitulo, y = valor)) +    
  geom_area(aes(fill = valor > 0),stat = 'identity') +
  scale_fill_manual(values = c('red','green')) + # Cambia los colores
  # Añade una línea suavizada sin el error standard
  geom_smooth(method = "loess", se = F, col = "black") + 
  theme(legend.position = 'none', # Borra la leyenda
        text = element_text(size = fs)) + # Cambia tamaño de la fuente
  labs(x = "Capítulo", y = "Puntuación de sentimiento", # Añade nuevas leyendas
       title = "Harry Potter: Sentimiento a lo largo de la saga",
       subtitle = "Se utiliza tidytext y los diccionarios AFINN, bing y nrc vertidos al español") +
  # Separa el gráfico por libro y diccionario y cambia el eje X
  facet_grid(lexicon ~ libro, scale = "free_x")

# Imprime el gráfico
plot_sentimiento


plot_sentimiento + coord_cartesian(ylim = c(-100,50)) # Aumenta el tamaño de los gráfico plot


# Visualiza la valencia emotiva a lo largo del tiempo
hp_sentimiento %>% # Nombres excluidos
  filter(!sentimiento %in% c('negativo','positivo')) %>% # Solo mantiene las emociones de nrc
  group_by(sentimiento, libro, capitulo) %>%
  count() %>% # Resume los recuentos
  # Crea el área del gráfico
  ggplot(aes(x = capitulo, y = n)) +
  geom_area(aes(fill = sentimiento), stat = 'identity') + 
  # Añade un línea negra suavizada sin el error standard
  geom_smooth(aes(fill = sentimiento), method = "loess", se = F, col = 'black') + 
  theme(legend.position = 'none', # Borra leyenda
        text = element_text(size = fs)) + # Cambia el tamaño de la fuente
  labs(x = "Capítulo", y = "Valencia del sentimiento", # Añade nuevas leyendas
       title = "Harry Potter: Emociones a lo largo de la saga",
       subtitle = "Se utiliza tidytext y el diccionario nrc vertido al español") +
  # Separa en gráficos diferentes las emociones por libros
  facet_grid(sentimiento ~ libro, scale = "free_x") 

