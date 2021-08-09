library(rvest)
library(tidyverse)
#install.packages("tibble")
#https://bookdown.org/dparedesi/data-science-con-r/procesamiento-de-cadenas-y-miner%C3%ADa-de-texto.html

#No modifiques tus tablas brutas, para que no tengas q volver a correr codigos


#Luego de exportar datos, tenemos que validar mediante el procesamiento de cadenas y asegurar
#una calidad mínima para poder realizar nuestros análisis.

url <- "https://es.wikipedia.org/wiki/Anexo:Pa%C3%ADses_hispanos_por_poblaci%C3%B3n"
data_en_html <- read_html(url)

tablas_web <- data_en_html %>% html_nodes("table")
#{xml_nodeset (3)}
#[1] <table class="wikitable sortable" style="text-align: right"><tbody>\n<tr>\n<th>N.º</th>\ ...
#[2] <table class="wikitable"><tbody>\n<tr>\n<th>Países</th>\n<th>Población hispana<sup id="c ...
#[3] <table class="wikitable"><tbody>\n<tr>\n<th>Países</th>\n<th>Población de hablantes<sup  ...

#Al inspeccionar la página, y situarme en la tabla que requiero, puedo notar que el nombre 
#de la que busco es "wikitable sortable jquery-tablesorter", por tanto es la primera

sapply(tablas_web,function(x)dim(html_table(x,fill =TRUE)))
#Esta es otra forma de saber cual es la tabla que busco, si vemos, la primera tabla
#tiene 22 filas y 6 columnas, justamente como la que estamos viendo en wikipedia.

tablas_web[1]
#solo me dice el codigo de table class, el doble corchete va a lo primero luego de eso

#Se crea tabla nueva, y se añade html_table para extraer la tabla en si y no solo el codigo
tabla_en_bruto <- tablas_web[[1]] %>% html_table

tabla_en_bruto <- tabla_en_bruto %>% 
  setNames(c("N", "pais", "poblacion", "prop_poblacion", "cambio_medio", "link")) 

tabla_en_bruto <- tabla_en_bruto %>% as_tibble()

tabla_en_bruto %>% head(5)
#> # A tibble: 5 x 6
#>       N pais        poblacion  prop_poblacion cambio_medio link                 
#>   <int> <chr>       <chr>      <chr>          <chr>        <chr>                
#> 1     1 MéxicoMéxi… 132 820 0… 1,65           1 285 000    www.datos.gob.mx/bus…
#> 2     2 Colombia C… 50 339 443 0,66           572 000      www.dane.gov.co      
#> 3     3 España Esp… 46 940 000 0,62           45 000       www.ine.es           
#> 4     4 Argentina   45,195,774 0,59           465 000      www.indec.mecon.ar   
#> 5     5 PerúPerú    33,105,273 0,43           342 000      www.inei.gob.pe

#Siempre se deben observar las clases de las columnas, en población está como carácter
#lo cual se debe corregir porque no podremos hacer cálculos con ello así, tco podemos
#transformar a número al tiro, ya que hay espacios y en otros comas, al igual que cambio_medio
#espacios y comas tambien son caracteres

#Expresiones regulares
#https://www.diegocalvo.es/expresiones-regulares-en-r/
#sirven para ver como se les llama a los espacios \\s x ej, el punto \\.

#################################################################################
#Reemplazando caracteres dentro de vectores (luego dentro de la misma tabla)

#Usamos 
str_replace_all(cadena, patron, reemplazo)
#cuando estamos seguros que en toda la columna pasa lo mismo, usamos este comando

vector_poblacion <- tabla_en_bruto$poblacion
vector_poblacion <- str_replace_all(vector_poblacion,"\\s",",")
# [1] "132,820,000" "50,882,884"  "47,431,246"  "45,195,777"  "33,105,273"  "28,435,943"

#Hemos llevado a propósito todos los valores a estar separados por comas porque ahora 
#podremos usar fácilmente la función 
parse_number(vector) 
#que no solo reemplaza las comas por vacío, sino que remueve todo valor no numérico 
#antes del primer número, como $ por ejemplo, y ademas, lo deja como valor numerico

vector_poblacion <- parse_number(vector_poblacion)
# [1] 132820000  50882884  47431246  45195777  33105273  28435943  19116209  17915567  17643060

#Ahora se puden hacer calculos basicos como:

# Convertir a millones
vector_poblacion <- vector_poblacion/10^6

# Removemos el último valor que es la población del mundo:
largo <- length(vector_poblacion)
vector_poblacion <- vector_poblacion[-largo]

# Visualización
boxplot(vector_poblacion)

#######################################################################################

#Reemplazando caracteres en la tibble

#Para mutar las columnas de nuestra tabla en bruto usaremos la función
mutate_at(columnas, ~funcion)
#usando el operador pipeline %>%, asi mantenemos la info de la tabla bruta
#y en caso de que algo se transformo extraño, podemos volver a verloooo

tabla_en_bruto %>% 
  mutate_at(c(3,5), ~str_replace_all(., "\\s", ","))
#> # A tibble: 22 x 6
#>        N pais        poblacion  prop_poblacion cambio_medio link                
#>    <int> <chr>       <chr>      <chr>          <chr>        <chr>               
#>  1     1 MéxicoMéxi… 132,820,0… 1,65           1,285,000    www.datos.gob.mx/bu…
#>  2     2 Colombia C… 50,339,443 0,66           572,000      www.dane.gov.co     
#>  3     3 España Esp… 46,940,000 0,62           45,000       www.ine.es          
#>  4     4 Argentina   45,195,774 0,59           465,000      www.indec.mecon.ar  
#>  5     5 PerúPerú    33,105,273 0,43           342,000      www.inei.gob.pe     
#>  6     6 VenezuelaV… 28,435,940 0,42           416,000      www.ine.gov.ve      
#>  7     7 Chile Chile 19,116,201 0,25           188,000      www.ine.cl          
#>  8     8 Guatemala … 17,915,568 0,23           496,000      www.ine.gob.gt      
#>  9     9 Ecuador Ec… 17,643,054 0,22           255,000      www.ecuadorencifras…
#> 10    10 Bolivia Bo… 11,501,900 0,15           163,000      www.ine.gob.bo      
#> # … with 12 more rows

#OJO: Hemos quitado de la función str_replace_all el atributo cadena y lo hemos
#reeplazado por un punto . . Y es que ese punto . nos indica que evaluará para 
#cada columna c(3,5) de nuestra tabla. En parse_number tb lo usamos.

tabla_en_bruto %>% 
  mutate_at(c(3,5), ~str_replace_all(., "\\s", ",")) %>% 
  mutate_at(c(3,5), ~parse_number(.))
#> # A tibble: 22 x 6
#>        N pais         poblacion prop_poblacion cambio_medio link                
#>    <int> <chr>            <dbl> <chr>                 <dbl> <chr>               
#>  1     1 MéxicoMéxico 132820000 1,65                1285000 www.datos.gob.mx/bu…
#>  2     2 Colombia Co…  50339443 0,66                 572000 www.dane.gov.co     
#>  3     3 España Espa…  46940000 0,62                  45000 www.ine.es          
#>  4     4 Argentina     45195774 0,59                 465000 www.indec.mecon.ar  
#>  5     5 PerúPerú      33105273 0,43                 342000 www.inei.gob.pe     
#>  6     6 VenezuelaVe…  28435940 0,42                 416000 www.ine.gov.ve      
#>  7     7 Chile Chile   19116201 0,25                 188000 www.ine.cl          
#>  8     8 Guatemala G…  17915568 0,23                 496000 www.ine.gob.gt      
#>  9     9 Ecuador Ecu…  17643054 0,22                 255000 www.ecuadorencifras…
#> 10    10 Bolivia Bol…  11501900 0,15                 163000 www.ine.gob.bo      
#> # … with 12 more rows


