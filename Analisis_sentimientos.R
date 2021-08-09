library(readr)
library(readxl)
library(tidyverse)
library(tidyr)
library(stringr)


#Cuando analizamos textos no solo vamos a querer saber cuáles son las palabras 
#que más se utilizan en textos, sean estos comentarios dejados por nuestros clientes,
#solicitudes de reclamo, etc. También es muy útil el saber el tono de los mensajes. 
#Esta técnica es conocida como análisis de sentimientos, la cual se puede hacer muy
#fácilmente con la librería que ya hemos usado syuzhet.

#Y qué mejor lugar para analizar tonos de mensajes que en Twitter. Para ello, 
#vamos a descargar un historial de Tweets de algún personaje Hispanohablante 
#de la página vicinitas.io. 

#Esta página nos permite descargar un excel dada una cuenta pública:
#https://www.vicinitas.io/free-tools/download-user-tweets.

#Para nuestro ejemplo, utilizaremos tweets de la cuenta de la abogada Rosa María 
#Palacios12. Para este ejemplo ya se ha cargado el excel a Github. Descargaremos 
#ese excel directamente desde ahí a nuestra computadora a un archivo temporal y 
#luego lo leeremos usando read_excel().

#url <- "https://dparedesi.github.io/DS-con-R/rmapalacios_user_tweets.xlsx"

# Creamos un nombre & ruta temporal para nuestro archivo. Es un espacio de tabla para
#extraer la data de github y poder meterla en algo mientras, tempfile es una ruta temp
#archivo_temporal <- tempfile()

# Descargamos el archivo en nuestro temporal
#download.file(url, archivo_temporal)

#NO FUNCIONO LO DEL GIT, PERO PUDE DESCARGAR EL EXCEL Y HACERLO DESDE MI PC

# Importamos el excel
publicaciones <- read_excel("C:/Users/user/Documents/R/rmapalacios_user_tweets.xlsx")

# Eliminamos el archivo temporal
#file.remove(archivo_temporal)
#> [1] TRUE

#Hemos creado nuestro objeto publicaciones, el cual tiene en la columna Text los 
#diferentes tweets, retweets y replies, realizados. Si bien podríamos hacer un 
#análisis de datos utilizando las otras columnas, nos vamos a centrar en el 
#contenido y tono de los Tweets. 

#Para ello, vamos a eliminar los Retweets y las repuestas, quedándonos solo con
#los Tweets.

colnames(publicaciones)
colnames(publicaciones)[11] <- "TweetType"

tuits <- publicaciones %>% 
  filter(TweetType == "Tweet") %>% 
  .$Text
#con .$text me queda en una especie d lista d caracteres, si pongo publicaciones 
#se deja en la tabla

#Con lo aprendido haciendo mapas de palabras, creemos un mapa con el contenido 
#de las publicaciones.

tuits_limpio <- tuits %>% 
  removePunctuation() %>% 
  str_to_lower() %>% 
  str_replace_all(., "[[:cntrl:]]", " ") %>% 
  removeWords(., words = stopwords("spanish")) %>% 
  removeWords(., words = c("usted", "pues", "tal", "tan",
                           "así", "dijo", "cómo", "sino", 
                           "entonces", "aunque", "que"))

coleccion <- tuits_limpio %>% 
  VectorSource() %>%
  Corpus()

wordcloud(coleccion, 
          min.freq = 5,
          max.words = 80, 
          random.order = FALSE, 
          colors = brewer.pal(name = "Dark2", n = 8)
)

#Ella es una abogada, con lo que hace mucho sentido que postee contenido de lo 
#que se puede o no se puede. Podríamos ser más rigurosos y buscar conseguir esta
#combinación agregando guiónes bajo si se detecta el patrón, pero por el momento 
#nos vamos a enfocar en el tono.

#Pasando palabras de la coleccion a tipo matriz para ordenar de menor a mayor
#y luego sea mas sencillo crearlo como df

palabras <- coleccion %>% 
  TermDocumentMatrix() %>% 
  as.matrix() %>% 
  rowSums() %>% 
  sort(decreasing = TRUE)

palabras %>% 
  head(20)
str(palabras)

frecuencias <- data.frame(
  palabra = names(palabras),
  frecuencia = palabras
)

#Top 10 de palabras mas dichas x la abogada

frecuencias[1:10,] %>% 
  ggplot() +
  aes(frecuencia, y = reorder(palabra, frecuencia)) +
  geom_bar(stat = "identity", color = "white", fill = "blue") +
  geom_text(aes(label = frecuencia, hjust = 1.5), color = "white") +
  labs(
    x = NULL,
    y = "Palabras más usadas en tweets de Abogada"
  )

#Con nuestro objeto tuits_limpio podemos obtener cuál es el tono utilizando la 
#función get_nrc_sentiment(), la cual nos da un score por cada fila del vector 
#de acuerdo al Léxico de Emociones NRC13. El Léxico de Emociones NRC es una lista
#de palabras y sus asociaciones con ocho emociones básicas (ira, miedo, anticipación,
#confianza, asombro, tristeza, alegría y aversión) y dos sentimientos 
#(negativo y positivo).



resultado <- get_nrc_sentiment(tuits_limpio, language = "spanish")

resultado %>% 
  head(10)
#>    anger anticipation disgust fear joy sadness surprise trust negative positive
#> 1      0            1       0    2   1       3        0     3        5        3
#> 2      4            0       3    5   0       3        0     0        7        0
#> 3      0            0       0    0   0       0        0     0        0        0
#> 4      0            1       1    1   1       1        1     1        1        2
#> 5      0            2       0    1   3       1        0     4        0        4
#> 6      0            0       0    0   0       0        0     0        0        0
#> 7      1            1       0    3   0       1        0     1        3        0
#> 8      0            0       0    0   0       1        0     0        2        3
#> 9      1            0       1    1   0       1        0     0        1        0
#> 10     0            0       0    0   0       0        0     0        0        0

#Podemos realizar algunas transformaciones a este data frame, pero antes vamos a crear 
#una función de traducción. Dado que, aun tenemos que traducir las cabeceras.

trad_emociones <- function(cadena){
  case_when(
    cadena == "anger" ~ "Ira",
    cadena == "anticipation" ~ "Anticipación",
    cadena == "disgust" ~ "Aversión",
    cadena == "fear" ~ "Miedo",
    cadena == "joy" ~ "Alegría",
    cadena == "sadness" ~ "Tristeza",
    cadena == "surprise" ~ "Asombro",
    cadena == "trust" ~ "Confianza",
    cadena == "negative" ~ "Negativo",
    cadena == "positive" ~ "Positivo",
    TRUE ~ cadena
  )
}

#Ahora sí, con nuestra función lista, podemos transformar nuestro objeto resultado 
#para obtener las frecuencias de cada emocion y sentimiento.

# Resumen de las emociones/sentimientos
sentimientos <- resultado %>% 
  gather(sentimiento, cantidad) %>% 
  mutate(sentimiento = trad_emociones(sentimiento)) %>% 
  group_by(sentimiento) %>% 
  summarise(total = sum(cantidad))

sentimientos
#> # A tibble: 10 x 2
#>    sentimiento  total
#>    <chr>        <dbl>
#>  1 Alegría        547
#>  2 Anticipación   903
#>  3 Asombro        421
#>  4 Aversión       810
#>  5 Confianza     1369
#>  6 Ira            803
#>  7 Miedo         1342
#>  8 Negativo      2334
#>  9 Positivo      2117
#> 10 Tristeza      1177

#Vemos que tenemos las 8 emociones más los 2 sentimientos. Obtengamos los índices 
#de los sentimientos positivos y negativos:

index <- sentimientos$sentimiento %in% c("Positivo", "Negativo") 
#Este vector nos servirá para poder visualizar de forma separada las emociones 
#y los sentimientos.

# Visualización de emociones
sentimientos[!index,] %>% 
  ggplot() +
  aes(sentimiento, total) +
  geom_bar(aes(fill = sentimiento), stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab(NULL) +
  ylab("Total") +
  ggtitle("Emociones en los Tweets de Rosa María Palacios")

# Visualización de si son sentimientos positivos o negativos:
sentimientos[index,] %>% 
  ggplot() +
  aes(sentimiento, total) +
  geom_bar(aes(fill = sentimiento), stat = "identity") +
  xlab(NULL) +
  ylab("Total") +
  ggtitle("Sentimientos de los Tweets de Rosa María Palacios")



