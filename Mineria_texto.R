#La minería de texto es el descubrimiento por computadora de información nueva, 
#previamente desconocida, mediante la extracción automática de información de 
#diferentes recursos escritos. Los recursos escritos pueden ser sitios web, 
#libros, chats, comentarios, correos electrónicos, reseñas, artículos, etc

#Es el proceso de derivar información de alta calidad del texto.

#La primera técnica de minería de texto que aprenderemos será la construcción de
#mapas de palabras. Para ello, necesitaremos instalar paquetes desarrollados 
#exclusivamente para la minería de texto (text mining)

#install.packages("syuzhet")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("NLP")
#install.packages("RColorBrewer")
#install.packages("Rcpp")


library(syuzhet) # Funciones get_
library(stringr) # Funciones str_
library(NLP)
library(tm) # Funciones de text mining
library(RColorBrewer)
library(wordcloud)# Crear mapa de nubes
library(Rcpp)



#Importando datos: Mapa de palabras - Nube
# nos permiten identificar rápidamente cuáles son las palabras que más se repiten 
#en un texto

#Analizando el libro: Niebla en txt en una pagina web
url <- "http://www.gutenberg.org/files/49836/49836-0.txt"

obra <- get_text_as_string(url)
#Importaremos el texto usando la función get_text_as_string() de la librería syuzhet
#para importar todo el texto como una cadena. Esta función es muy útil si deseamos 
#importar archivos grandes. 

oraciones <- get_sentences(obra)
#usaremos la función get_sentences(), para crearnos un vector de oraciones a partir 
#del texto inicial


#Limpieza del texto:
#Como ya hemos aprendido anteriormente, no tenemos que ir directo a analizar. 
#Sino, tenemos que limpiar nuestra data. Lo primero que haremos es eliminar las
#primeras filas que no corresponden a la obra.

# Eliminamos primeras filas de notas, prólogo, post-prólogo

oraciones #aqui podemos visualizar cuando comienza "Niebla"
total_lineas <- length(oraciones)
str(total_lineas) #son solo indices de numeros para luego usar los corchetes
linea_empieza <- 115
linea_final <- total_lineas - linea_empieza

texto_limpio <- oraciones[linea_empieza:linea_final]

#A continuación utilizaremos un regex para detectar caracteres especiales de la 
#codificación, como saltos de línea y tabulaciones. Para ello usaremos el regex
#[[:cntrl:]]. 

#Así mismo, convertiremos todas las palabras a minúsculas para facilitar las 
#comparaciones entre palabras. Finalmente, como queremos analizar las palabras, 
#eliminamos todos los signos de puntuación.

texto_limpio <- texto_limpio %>% 
  str_replace_all(., "[[:cntrl:]]", " ") %>% #reemplaza todas tabulaciones y saltos en solo 1 espacio
  str_to_lower() %>% #minusculas
 removePunctuation() %>% 
  str_replace_all(., "—", " ") #todo dato con - dejale un espacio... y si se cortaba la palabra...

#Stop words o palabras vacias

#Por otro lado, la librería “tm”, de text mining en inglés, nos provee funciones y
#vectores para poder limpiar nuestros datos. Ya usamos la función removePunctuation().
#Sin embargo, también tenemos la función stopwords("spanish") nos llama a un vector 
#con palabras vacías, es decir, aquellas con poco valor para el análisis, tales como 
#algunas preposiciones y muletillas. 

#Además, usaremos la función removeWords() para remover todas las palabras que se 
#encuentre en nuestro vector de palabras vacías

texto_limpio <- removeWords(texto_limpio, words = stopwords("spanish"))

#Finalmente, eliminamos los vacíos excesivos, algunos de ellos creados por las 
#transformaciones anteriores.

texto_limpio <- stripWhitespace(texto_limpio)

#Creacion del Corpus

#Para poder crear un mapa de palabras necesitamos aplicar la función VectorSource()
#para convertir cada fila a un documento, y la función Corpus() que nos permitirá
#crear estos documentos como una colección de datos.

coleccion <- texto_limpio %>% 
  VectorSource() %>%
  Corpus()

#Ya estamos listos para crear nuestro mapa de palabras. Para ello usaremos la 
#librería wordcloud() y la función del mismo nombre.

wordcloud(coleccion, 
          min.freq = 5,
          max.words = 80, 
          random.order = FALSE, 
          colors = brewer.pal(name = "Dark2", n = 8)
          )



#2da Limpieza de datos

#En minería de texto frecuentemente vamos a obtener un resultado que aun 
#requiere de limpiar más datos. Por ejemplo, vemos aun palabras como 
#pronombres de poco interés para el análisis. Volveremos a usar la función
#removeWords(), pero esta vez con un vector personalizado de las palabras que 
#deseamos retirar

a_retirar <- c("usted", "pues", "tal", "tan", "así", "dijo", 
               "cómo", "sino", "entonces", "aunque", "don", "doña",
               "â€", "mã¡s", "â¿", "â¡", "quã©","mã-","ã©l", "sã",
               "asã-","â€â¿","â€sã","tãº","â¿quã©")

texto_limpio <- removeWords(texto_limpio, words = a_retirar)

coleccion <- texto_limpio %>% 
  VectorSource() %>%
  Corpus()

wordcloud(coleccion, 
          min.freq = 5,
          max.words = 80, 
          random.order = FALSE, 
          colors = brewer.pal(name = "Dark2", n = 8)
)

#Augusto y Eugenia, como podemos asumir, son los protagonistas de Niebla y gran
#parte de la acción en este libro ocurre en la “casa” de uno u otro protagonista,
#discutiendo las relaciones entre “hombre” y “mujer”.


#Frecuencia de palabras

#Ya tenemos una idea visual de las palabras más utilizadas. Sin embargo, también 
#podríamos saber exactamente cuántas veces apareció una determinada palabra. 

#Para ello tenemos que convertir nuestra colección a una matrix. Para ello usamos
#las funciones juntas TermDocumentMatrix(), as.matrix() y rowSums() que nos 
#dejarán con un vector con la frecuencia de palabras.

palabras <- coleccion %>% 
  TermDocumentMatrix() %>% 
  as.matrix() %>% 
  rowSums() %>% 
  sort(decreasing = TRUE)

palabras %>% 
  head(20)
str(palabras)

#> augusto eugenia   mujer  hombre    casa   ahora    bien   mismo     ser     vez 
#>     365     202     184     133     128     124     122     103     101      91 
#>    ojos    vida      sé     fué   luego     dos   cosas   pobre   madre después 
#>      87      86      82      77      76      74      72      71      71      71

#Con este vector ya es fácil convertirlo a data frame, dado que tenemos
#los nombres y los valores, y visualizarlo.

frecuencias <- data.frame(
  palabra = names(palabras),
  frecuencia = palabras
)

# Visualización de top 10 palabras:
frecuencias[1:10,] %>% 
  ggplot() +
  aes(frecuencia, y = reorder(palabra, frecuencia)) +
  geom_bar(stat = "identity", color = "white", fill = "blue") +
  geom_text(aes(label = frecuencia, hjust = 1.5), color = "white") +
  labs(
    x = NULL,
    y = "Palabras más usadas en la obra"
  )
library(ggplot2)
