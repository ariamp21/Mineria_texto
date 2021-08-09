#Una expresión regular (o regex como se le conoce en inglés) es un patrón que 
#describe un conjunto de cadenas. 

#install.packages("dslabs")
library(dslabs)
library(tidyverse)
data(heights)

heights %>% 
  head(10)
#>       sex height
#> 1    Male     75
#> 2    Male     70
#> 3    Male     68
#> 4    Male     74
#> 5    Male     61
#> 6  Female     65
#> 7  Female     66
#> 8  Female     62
#> 9  Female     66
#> 10   Male     67

#Estos datos estaban listos para ser analizados. Sin embargo, no fue así como 
#provino de la fuente. Fue asi:

reported_heights %>% 
  head(10)
#>             time_stamp    sex height
#> 1  2014-09-02 13:40:36   Male     75
#> 2  2014-09-02 13:46:59   Male     70
#> 3  2014-09-02 13:59:20   Male     68
#> 4  2014-09-02 14:51:53   Male     74
#> 5  2014-09-02 15:16:15   Male     61
#> 6  2014-09-02 15:16:16 Female     65
#> 7  2014-09-02 15:16:19 Female     66
#> 8  2014-09-02 15:16:21 Female     62
#> 9  2014-09-02 15:16:21 Female     66
#> 10 2014-09-02 15:16:22   Male     67

#Podemos ver que la data se ve bien, pero NO DEBES CONFIARTE QUE ESTÁ 100% BIEN

#Observando valores de la data para procesar y generalizarla

#OPCION 1: Sacar una muestra aleatoria para ver las diferentes formas escritas

estatura <- reported_heights$height
sample(estatura, 100)

#o tb
sample(reported_heights$height, 100)

#>   [1] "69"              "67"              "5' 11\""         "71"             
#>   [5] "6"               "69"              "68"              "63"             
#>   [9] "72"              "6"               "73"              "6"              
#>  [13] "74"              "69"              "69"              "74"             
#>  [17] "71.5"            "6'1\""           "69"              "184"            
#>  [21] "65"              "69"              "69"              "68.5"           
#>  [25] "170 cm"          "68.5"            "72"              "74"             
#>  [29] "75"              "74"              "64"              "157"            
#>  [33] "71"              "67.72"           "5.57"            "70"             
#>  [37] "70"              "63"              "67"              "74"             
#>  [41] "5 feet 6 inches" "68"              "5'12"            "61.32"   
#... 


#OPCION 2: Contar NA, es decir, los que no son solo numeros

x <- as.numeric(estatura)
#> Warning: NAs introduced by coercion

sum(is.na(x))
#> [1] 81


#OPCION 3: Añadir columna para saber la cantidad y forma en q estan escritos,
#asi encontraremos los patrones q se repiten

reported_heights %>%  #Tiene 1095 datos en total
  mutate(estatura_numero = as.numeric(height)) %>% 
  filter(is.na(estatura_numero)) %>% 
  head(10)
#> Warning: NAs introduced by coercion
#>             time_stamp    sex                 height estatura_numero
#> 1  2014-09-02 15:16:28   Male                  5' 4"              NA
#> 2  2014-09-02 15:16:37 Female                  165cm              NA
#> 3  2014-09-02 15:16:52   Male                    5'7              NA
#> 4  2014-09-02 15:16:56   Male                  >9000              NA
#> 5  2014-09-02 15:16:56   Male                   5'7"              NA
#> 6  2014-09-02 15:17:09 Female                   5'3"              NA
#> 7  2014-09-02 15:18:00   Male 5 feet and 8.11 inches              NA
#> 8  2014-09-02 15:19:48   Male                   5'11              NA
#> 9  2014-09-04 00:46:45   Male                  5'9''              NA
#> 10 2014-09-04 10:29:44   Male                 5'10''              NA

#Podríamos optar por eliminar estos 81 datos NAs al no ser significativos
#respecto al total de 1,095 datos. Sin embargo, hay varios de estos datos que 
#siguen un patrón determinado y en vez de ser descartados podrían ser convertidos 
#a la escala que tenemos en el resto de la data.


#Empecemos extrayendo nuestra columna a un solo vector de caracteres con todos
#los valores que no convierten en automático a número o fueron ingresados en 
#pulgadas. Esto lo detectamos si miden más de 5 y hasta 7 pies (de 1.5m a 2.1 metros)


#Se crea un vector nuevo a partir de la bbdd, filtrando aquellos que tienen palabras
#O (suma, no condiciona) quedan como NA porque no son numeros, y los que si son 
#numeros entre 5 a 7

estaturas_error <- reported_heights %>% 
  filter(is.na(as.numeric(height)) | # Los que No c convierten a número
           (!is.na(as.numeric(height)) & as.numeric(height) >= 5 &
              as.numeric(height) <= 7 ) # No son NA, o ingresó en pies y no pulgadas
        ) %>% #Si cierro antes del pipe, me crea una DATAFRAME cn todas las columnas
  .$height    # con este punto le digo, guarda solo el vector con esta columna y todas filas

length(estaturas_error)
#> [1] 168

#Agregando la condición de haber ingresado en pies tenemos 168 errores. 
#No podemos ignorar 15.3% de errores.



#Identificando patroneeeees:

#Usaremos la función 
str_detect(cadena, patron) 
#que nos permitirá detectar si una cadena cumple un determinado patrón.
#El resultado será un valor lógico: TRUE or FALSE que podemos usar como índice 
#para obtener los valores que cumplen en nuestro vector.

indice <- str_detect(estaturas_error, "feet")

estaturas_error[indice] # Cumplen el patrón
#> [1] "5 feet and 8.11 inches" "5 feet 7inches"         "5 feet 6 inches"

estaturas_error[!indice] %>% # No cumplen el platrón
  head(40) 
#>  [1] "6"                      "5' 4\""                 "5.3"                   
#>  [4] "165cm"                  "6"                      "5'7"                   
#>  [7] ">9000"                  "5'7\""                  "5'3\""                 
#> [10] "5.25"                   "5'11"                   "5.5"                   
#> [13] "5'9''"                  "6"                      "6.5"                   
#> [16] "5'10''"                 "5.8"                    "5"                     
#> [19] "5.6"                    "5,3"                    "6'"                    
#> [22] "6"                      "5.9"                    "6,8"                   
#> [25] "5' 10"                  "5.5"                    "6.2"                   
#> [28] "Five foot eight inches" "6.2"                    "5.8"                   
#> [31] "5.1"                    "5.11"                   "5'5\""                 
#> [34] "5'2\""                  "5.75"                   "5,4"                   
#> [37] "7"                      "5.4"                    "6.1"                   
#> [40] "5'3"

#Podemos ver que feet tb esta escrito como foot, por lo tanto se añadiran con
# | para alternar los posibles valores de la cadena

indice <- str_detect(estaturas_error, "feet|ft|foot")

estaturas_error[indice] # Cumplen
#> [1] "5 feet and 8.11 inches" "Five foot eight inches" "5 feet 7inches"        
#> [4] "5ft 9 inches"           "5 ft 9 inches"          "5 feet 6 inches"

#De la misma forma podemos encontrar las variaciones para pulgadas y otros 
#símbolos que podemos remover

indice <- str_detect(estaturas_error, "inches|in|''|\"|cm|and")

estaturas_error[indice] # Cumplen
#>  [1] "5' 4\""                 "165cm"                  "5'7\""                 
#>  [4] "5'3\""                  "5 feet and 8.11 inches" "5'9''"                 
#>  [7] "5'10''"                 "Five foot eight inches" "5'5\""                 
#> [10] "5'2\""                  "5'10''"                 "5'3''"                 
#> [13] "5'7''"                  "5'3\""                  "5'6''"                 
#> [16] "5'7.5''"                "5'7.5''"                "5'2\""                 
#> [19] "5' 7.78\""              "5 feet 7inches"         "5'8\""                 
#> [22] "5'11\""                 "5'7\""                  "5' 11\""               
#> [25] "6'1\""                  "69\""                   "5' 7\""                
#> [28] "5'10''"                 "5ft 9 inches"           "5 ft 9 inches"         
#> [31] "5'11''"                 "5'8\""                  "5 feet 6 inches"       
#> [34] "5'10''"                 "6'3\""                  "5'5''"                 
#> [37] "5'7\""                  "6'4\""                  "170 cm"

#En este caso hemos ingresado '' para detectar a los que ingresaron ese símbolo 
#para denotar pulgadas y \" por si usaron doble comillas. En este último caso 
#hemos utilizado \ para que no nos genere un error al interpretar como cierre de
#la cadena.

estaturas_error <- str_replace_all(estaturas_error, "feet|ft|foot", "'")
estaturas_error <- str_replace_all(estaturas_error, "inches|in|''|\"|cm|and", "")

estaturas_error %>% 
  head(30)
#>  [1] "6"             "5' 4"          "5.3"           "165"          
#>  [5] "6"             "5'7"           ">9000"         "5'7"          
#>  [9] "5'3"           "5 '  8.11 "    "5.25"          "5'11"         
#> [13] "5.5"           "5'9"           "6"             "6.5"          
#> [17] "5'10"          "5.8"           "5"             "5.6"          
#> [21] "5,3"           "6'"            "6"             "5.9"          
#> [25] "6,8"           "5' 10"         "5.5"           "6.2"          
#> [29] "Five ' eight " "6.2"


#Como esfuerzo adicional, podríamos también buscar solucionar que algunas personas
#han escrito palabras en vez de números. Para ello creamos una función que reemplace
#cada palabra por un número y aplicamos al vector:

palabras_a_numero <- function(s){
  str_to_lower(s) %>%  #les deja en minusculas a todas las palabras
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

estaturas_error <- palabras_a_numero(estaturas_error)

estaturas_error %>% 
  head(30)
#Se nota en 5 ' 8 "

#>  [1] "6"          "5' 4"       "5.3"        "165"        "6"         
#>  [6] "5'7"        ">9000"      "5'7"        "5'3"        "5 '  8.11 "
#> [11] "5.25"       "5'11"       "5.5"        "5'9"        "6"         
#> [16] "6.5"        "5'10"       "5.8"        "5"          "5.6"       
#> [21] "5,3"        "6'"         "6"          "5.9"        "6,8"       
#> [26] "5' 10"      "5.5"        "6.2"        "5 ' 8 "     "6.2"

#Anclaje

#Hay una persona que ha ingresado 6' o solo 6. Sería conveniente tener todo de la forma 
#pies más pulgadas. Con lo que deberíamos de tener 6'0. Para lograr ello tenemos
#que crear un regex acorde a esta situación genérica. 

#Usaremos el símbolo ^ para anclar nuestra validación a que “comience con” y el 
#símbolo $ para hacer coincidir con el fin de la cadena.

indice <- str_detect(estaturas_error, "^6'$")
#que comience con 6 y termine con '

estaturas_error[indice] 
# Cumple solo un dato porque nos falta hacerlo más GENERICOOO
#> [1] "6'"

indice <- str_detect(estaturas_error, "^[56]'$")

estaturas_error[indice] # Cumplen
#> [1] "6'"
#aun asi salen pocos, pero ahora se veran mas


#Reemplazando:

#Hemos colocado entre paréntesis para indicar que lo que está dentro es nuestro
#primer valor y usamos \\1 para hacer referencia a ese primer valor. 

#Entonces estamos indicando que escriba el primer valor, luego una comilla ', y 
#luego un cero 0.

estaturas_error <- str_replace_all(estaturas_error, "^([5-7])'$", "\\1'0")
#Para las estaturas de 5 a 7 pies que terminan con '

estaturas_error <- str_replace_all(estaturas_error, "^([5-7])$", "\\1'0")
#Para las estaturas que solo pusieron el numero sin '

estaturas_error %>% 
  head(30)
#>  [1] "6'0"        "5' 4"       "5.3"        "165"        "6'0"       
#>  [6] "5'7"        ">9000"      "5'7"        "5'3"        "5 '  8.11 "
#> [11] "5.25"       "5'11"       "5.5"        "5'9"        "6'0"       
#> [16] "6.5"        "5'10"       "5.8"        "5'0"        "5.6"       
#> [21] "5,3"        "6'0"        "6'0"        "5.9"        "6,8"       
#> [26] "5' 10"      "5.5"        "6.2"        "5 ' 8 "     "6.2"

#Repeticiones del patron:

#? = El patron se repite 0 o 1 vez
#+ = El patron se repite 1 o mas veces
#* = El patron se repite 0 o mas veces

#Por ejemplo, para encontrar todos los casos donde en vez de usar el símbolo de pies '
#ingresaron una coma, un punto, o un espacio usaremos el siguiente patrón:

patron <- "^([4-7])\\s*[,\\.]\\s*(\\d*)$"


#La cadena empieza con un dígito que va del 4 al 7.

#\\s quiere decir que va seguido de un espacio en blanco, pero usamos * para 
#indicar que ese caracter aparezca 0 o más veces.

#Luego de ese espacio buscaremos por cualquiera de los siguientes caracteres: , , 
#un punto \\. (al cual ponemos doble barra invertida porque el punto solo en un
#patrón significa “cualquier valor”).

#Volvemos a usar \\s* para buscar cero o más espacios en blanco.

#Finalmente indicamos que la cadena culmina ahí con un dígito, para denotar que
#busque cualquier dígito usamos \\d, d de dígito. Y agregamos asterísco para que
#nos mantenga uno o más dígitos que encuentre.

#En resumen: empieza con un número, luego símbolos y luego un dígito. Entre los 
#símbolos podrían haber espacios en blanco. Ese es nuestro patrón.


indice <- str_detect(estaturas_error, "^([4-7])\\s*[,\\.]\\s*(\\d*)$")

estaturas_error[indice] # Cumplen
#>  [1] "5.3"   "5.25"  "5.5"   "6.5"   "5.8"   "5.6"   "5,3"   "5.9"   "6,8"  
#> [10] "5.5"   "6.2"   "6.2"   "5.8"   "5.1"   "5.11"  "5.75"  "5,4"   "5.4"  
#> [19] "6.1"   "5.6"   "5.6"   "5.4"   "5.9"   "5.6"   "5.6"   "5.5"   "5.2"  
#> [28] "5.5"   "5.5"   "6.5"   "5,8"   "5.11"  "5.5"   "6.7"   "5.1"   "5.6"  
#> [37] "5.5"   "5.2"   "5.6"   "5.7"   "5.9"   "6.5"   "5.11"  "5 .11" "5.7"  
#> [46] "5.5"   "5.8"   "5.8"   "5.1"   "5.11"  "5.7"   "5.9"   "5.2"   "5.5"  
#> [55] "5.51"  "5.8"   "5.7"   "6.1"   "5.69"  "5.7"   "5.25"  "5.5"   "5.1"  
#> [64] "6.3"   "5.5"   "5.7"   "5.57"  "5.7"


estaturas_error <- str_replace_all(estaturas_error,
                                   "^([4-7])\\s*[,\\.]\\s*(\\d*)$", 
                                   "\\1.\\2'0" 
                                   )

#"\\1.\\2'0": Significa entonces, primero apareceran los numeros del 4-7 q es 
#la 1era parte del patron, luego vendra punto o coma****, luego viene la segunda parte
#con un 2do digito que termina en ' y un cero al lado... algo asi? SII

estaturas_error %>% 
  head(30)
#>  [1] "6'0"        "5' 4"       "5.3'0"      "165"        "6'0"       
#>  [6] "5'7"        ">9000"      "5'7"        "5'3"        "5 '  8.11 "
#> [11] "5.25'0"     "5'11"       "5.5'0"      "5'9"        "6'0"       
#> [16] "6.5'0"      "5'10"       "5.8'0"      "5'0"        "5.6'0"     
#> [21] "5.3'0"      "6'0"        "6'0"        "5.9'0"      "6.8'0"     
#> [26] "5' 10"      "5.5'0"      "6.2'0"      "5 ' 8 "     "6.2'0"


#Otro patrón que vemos ahora es cuando antes o después del símbolo de pies ' hay
#un espacio en blanco. Hagamos el cambio con lo aprendido e incluyamos los casos 
#donde hay decimales:

indice <- str_detect(estaturas_error, 
                     "^([4-7]\\.?\\d*)\\s*'\\s*(\\d+\\.?\\d*)\\s*$")

estaturas_error[indice] %>% # Cumplen
  head(30)
#>  [1] "6'0"        "5' 4"       "5.3'0"      "6'0"        "5'7"       
#>  [6] "5'7"        "5'3"        "5 '  8.11 " "5.25'0"     "5'11"      
#> [11] "5.5'0"      "5'9"        "6'0"        "6.5'0"      "5'10"      
#> [16] "5.8'0"      "5'0"        "5.6'0"      "5.3'0"      "6'0"       
#> [21] "6'0"        "5.9'0"      "6.8'0"      "5' 10"      "5.5'0"     
#> [26] "6.2'0"      "5 ' 8 "     "6.2'0"      "5.8'0"      "5.1'0"

estaturas_error <- str_replace_all(estaturas_error, 
                                   "^([4-7]\\.?\\d*)\\s*'\\s*(\\d+\\.?\\d*)\\s*$",
                                   "\\1'\\2"
                                   )
#Los pone entre parentesis para decir que todo eso de adentro es el 1er valor, luego el 2do,
#y asi sucesivamente.

estaturas_error %>% 
  head(30)
#>  [1] "6'0"    "5'4"    "5.3'0"  "165"    "6'0"    "5'7"    ">9000"  "5'7"   
#>  [9] "5'3"    "5'8.11" "5.25'0" "5'11"   "5.5'0"  "5'9"    "6'0"    "6.5'0" 
#> [17] "5'10"   "5.8'0"  "5'0"    "5.6'0"  "5.3'0"  "6'0"    "6'0"    "5.9'0" 
#> [25] "6.8'0"  "5'10"   "5.5'0"  "6.2'0"  "5'8"    "6.2'0"

sample(estaturas_error, 100)

#Así mismo, tenemos el patrón en que ingresaron: pies + espacio + pulgadas sin
#ningún símbolo. Hagamos el cambio con lo aprendido.

#Por ejemplo "6 04" 

indice <- str_detect(estaturas_error, "^([4-7])\\s+(\\d*)\\s*$")
#Comienza con digitos del 4 al 7, seguido de 1 o mas espacios, (asi solo considera
#este caso de los q tienen espacio), luego la segunda parte tiene digitos cualquiera
#de 0 o mas veces, luego termina con espacio 0 o mas veces


estaturas_error[indice] # Cumplen
#> [1] "5 11" "6 04"

estaturas_error <- str_replace_all(estaturas_error, 
                                   "^([4-7])\\s+(\\d*)\\s*$", "\\1'\\2"
                                   )

estaturas_error %>% 
  head(30)
#>  [1] "6'0"    "5'4"    "5.3'0"  "165"    "6'0"    "5'7"    ">9000"  "5'7"   
#>  [9] "5'3"    "5'8.11" "5.25'0" "5'11"   "5.5'0"  "5'9"    "6'0"    "6.5'0" 
#> [17] "5'10"   "5.8'0"  "5'0"    "5.6'0"  "5.3'0"  "6'0"    "6'0"    "5.9'0" 
#> [25] "6.8'0"  "5'10"   "5.5'0"  "6.2'0"  "5'8"    "6.2'0"

#Estamos listos para poner todos los patrones juntos y lo potente de los patrones
#es que nos pueden servir para futuros ejercicios. Así, crearemos una función donde
#colocaremos cada cambio que podemos hacer a un string.

formatear_errores <- function(cadena){
  cadena %>% 
    str_replace_all("feet|ft|foot", "'") %>% # Cambia pies por '
    str_replace_all("inches|in|''|\"|cm|and", "") %>% # Remueve símbolos
    str_replace_all("^([5-7])'$", "\\1'0") %>% # Agrega 0 a 5', 6' o 7'
    str_replace_all("^([5-7])$", "\\1'0") %>% # Agrega '0 a 5, 6 o 7
    str_replace_all("^([4-7])\\s*[,\\.]\\s*(\\d*)$", "\\1.\\2'0") %>% # Cambiar 5.3' a 5.3'0
    str_replace_all("^([4-7]\\.?\\d*)\\s*'\\s*(\\d+\\.?\\d*)\\s*$", "\\1'\\2") %>% #Quita espacios al medio
    str_replace_all("^([4-7])\\s+(\\d*)\\s*$", "\\1'\\2") %>% # Agrega '
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1.\\2") %>% # Cambia decimales de comas a puntos
    str_trim() #Quita espacios al inicio y fin
}

#Antes de aplicarlo a toda nuestra tabla volvamos a extraer los valores a un vector
#para aplicar las funciones creadas.

estaturas_error <- reported_heights %>% 
  filter(is.na(as.numeric(height)) | # No convierte a número
           (!is.na(as.numeric(height)) & as.numeric(height) >= 5 &
              as.numeric(height) <= 7 ) # o ingresó en pies y no pulgadas
  ) %>% 
  .$height

#Ahora aplicamos las funciones creadas
estaturas_formateadas <- estaturas_error %>% 
  palabras_a_numero() %>% 
  formatear_errores()

#Identificamos cuantos errores nos quedaron por solucionar

patron <- "^([4-7]\\.?\\d*)\\s*'\\s*(\\d+\\.?\\d*)\\s*$"

indice <- str_detect(estaturas_formateadas, patron)

estaturas_formateadas[!indice] # No cumplen con el patrón
#>  [1] "165"       ">9000"     "2'33"      "1.70"      "yyy"       "6*12"     
#>  [7] "69"        "708,661"   "649,606"   "728,346"   "170"       "7,283,465"

#Se corrigieron 156 errores de 168.

#Recien se corrigio todo en base a vectores, ahora lo vamos a hacer en tabla

# Aplicamos las fórmulas creadas
estaturas <- reported_heights %>% 
  mutate(height) %>% #a partir de esa columna, calcula una nueva, no varia la original
  mutate(estatura = palabras_a_numero(height) %>% formatear_errores())

# Obtenemos muestras aleatorias para validar calidad
indices_aleatorios <- sample(1:nrow(estaturas)) 
estaturas[indices_aleatorios, ] %>% 
  head(15)
#>               time_stamp    sex height estatura
#> 906  2016-01-26 20:14:13   Male     78       78
#> 944  2016-02-18 09:55:44   Male     86       86
#> 84   2014-09-02 15:16:46   Male      6      6'0
#> 977  2016-04-12 08:02:20   Male   64.5     64.5
#> 346  2014-10-30 06:18:27   Male     65       65
#> 554  2015-05-04 14:00:58 Female    5.1    5.1'0
#> 298  2014-09-23 16:33:48   Male     71       71
#> 227  2014-09-03 19:59:55   Male     67       67
#> 183  2014-09-02 15:17:39   Male     71       71
#> 725  2015-11-03 11:16:22   Male     71       71
#> 1070 2017-06-19 04:20:32   Male 170 cm      170
#> 989  2016-04-28 03:04:11   Male    175      175
#> 345  2014-10-29 04:28:56   Male     67       67
#> 287  2014-09-15 07:56:56   Male    5.9    5.9'0
#> 422  2015-01-04 14:53:25 Female     68       68

#Aun tenemos que hacer unas conversiones. Sin embargo, como ya siguen un patrón
#determinado podemos utilizar la función 
extract(columna_origen, nuevas_columnas, patron, remover_origen) 
#para poder crear nuevas columnas por cada valor de nuestro patrón.

patron <- "^([4-7]\\.?\\d*)\\s*'\\s*(\\d+\\.?\\d*)\\s*$"
#Empieza con un numero del 4 al 7, luego puede salir un punto 0 o 1 vez
#luego puede salir 0 o mas digitos, eso como primera parte, luego pueden salir
#0 o mas espacios, una comilla y 0 o mas espacios, luego digitos 1 o mas y luego
#punto 0 o mas veces, eso como segunda parte, luego puede tener 0 o mas espacio
#y finaliza

estaturas %>% 
  extract(estatura, c("pies", "pulgadas"), regex = patron, remove = FALSE) %>% 
  head(15)
#>             time_stamp    sex height estatura pies pulgadas
#> 1  2014-09-02 13:40:36   Male     75       75 <NA>     <NA>
#> 2  2014-09-02 13:46:59   Male     70       70 <NA>     <NA>
#> 3  2014-09-02 13:59:20   Male     68       68 <NA>     <NA>
#> 4  2014-09-02 14:51:53   Male     74       74 <NA>     <NA>
#> 5  2014-09-02 15:16:15   Male     61       61 <NA>     <NA>
#> 6  2014-09-02 15:16:16 Female     65       65 <NA>     <NA>
#> 7  2014-09-02 15:16:19 Female     66       66 <NA>     <NA>
#> 8  2014-09-02 15:16:21 Female     62       62 <NA>     <NA>
#> 9  2014-09-02 15:16:21 Female     66       66 <NA>     <NA>
#> 10 2014-09-02 15:16:22   Male     67       67 <NA>     <NA>
#> 11 2014-09-02 15:16:22   Male     72       72 <NA>     <NA>
#> 12 2014-09-02 15:16:23   Male      6      6'0    6        0
#> 13 2014-09-02 15:16:23   Male     69       69 <NA>     <NA>
#> 14 2014-09-02 15:16:26   Male     68       68 <NA>     <NA>
#> 15 2014-09-02 15:16:26   Male     69       69 <NA>     <NA>

#Ahora que ya tenemos los datos que cumplen con el patrón en otras dos columnas,
#y sabemos que son números, podemos convertir todo a número.

estaturas %>% 
  extract(estatura, c("pies", "pulgadas"), regex = patron, remove = FALSE) %>% 
  mutate_at(c("estatura", "pies", "pulgadas"), ~as.numeric(.)) %>% 
  head(15)
#> Warning in ~as.numeric(.): NAs introduced by coercion
#>             time_stamp    sex height estatura pies pulgadas
#> 1  2014-09-02 13:40:36   Male     75       75   NA       NA
#> 2  2014-09-02 13:46:59   Male     70       70   NA       NA
#> 3  2014-09-02 13:59:20   Male     68       68   NA       NA
#> 4  2014-09-02 14:51:53   Male     74       74   NA       NA
#> 5  2014-09-02 15:16:15   Male     61       61   NA       NA
#> 6  2014-09-02 15:16:16 Female     65       65   NA       NA
#> 7  2014-09-02 15:16:19 Female     66       66   NA       NA
#> 8  2014-09-02 15:16:21 Female     62       62   NA       NA
#> 9  2014-09-02 15:16:21 Female     66       66   NA       NA
#> 10 2014-09-02 15:16:22   Male     67       67   NA       NA
#> 11 2014-09-02 15:16:22   Male     72       72   NA       NA
#> 12 2014-09-02 15:16:23   Male      6       NA    6        0
#> 13 2014-09-02 15:16:23   Male     69       69   NA       NA
#> 14 2014-09-02 15:16:26   Male     68       68   NA       NA
#> 15 2014-09-02 15:16:26   Male     69       69   NA       NA


#Ahora que nuestras columnas ya son numéricas podemos hacer operaciones 
#para calcular la estatura.

estaturas %>% 
  extract(estatura, c("pies", "pulgadas"), regex = patron, remove = FALSE) %>% 
  mutate_at(c("estatura", "pies", "pulgadas"), ~as.numeric(.)) %>% 
  mutate(est_arregladas = pies*12 + pulgadas) %>% 
  head(15)

#> Warning in ~as.numeric(.): NAs introduced by coercion
#>             time_stamp    sex height estatura pies pulgadas est_arregladas
#> 1  2014-09-02 13:40:36   Male     75       75   NA       NA             NA
#> 2  2014-09-02 13:46:59   Male     70       70   NA       NA             NA
#> 3  2014-09-02 13:59:20   Male     68       68   NA       NA             NA
#> 4  2014-09-02 14:51:53   Male     74       74   NA       NA             NA
#> 5  2014-09-02 15:16:15   Male     61       61   NA       NA             NA
#> 6  2014-09-02 15:16:16 Female     65       65   NA       NA             NA
#> 7  2014-09-02 15:16:19 Female     66       66   NA       NA             NA
#> 8  2014-09-02 15:16:21 Female     62       62   NA       NA             NA
#> 9  2014-09-02 15:16:21 Female     66       66   NA       NA             NA
#> 10 2014-09-02 15:16:22   Male     67       67   NA       NA             NA
#> 11 2014-09-02 15:16:22   Male     72       72   NA       NA             NA
#> 12 2014-09-02 15:16:23   Male      6       NA    6        0             72
#> 13 2014-09-02 15:16:23   Male     69       69   NA       NA             NA
#> 14 2014-09-02 15:16:26   Male     68       68   NA       NA             NA
#> 15 2014-09-02 15:16:26   Male     69       69   NA       NA             NA

#Finalmente, haremos una validación de si la estatura está en un intervalo y/o si 
#estuvo expresado en centímetros o metros.

# Asumimos para una persona un mínimo 50" (1.2m) y máx 84" (2.1m)
min <- 50
max <- 84

estaturas <- estaturas %>% 
  extract(estatura, c("pies", "pulgadas"), regex = patron, remove = FALSE) %>% 
  mutate_at(c("estatura", "pies", "pulgadas"), ~as.numeric(.)) %>% 
  mutate(est_arregladas = pies*12 + pulgadas) %>% 
  mutate(estatura_final = case_when( #case_when no lo conocia
    !is.na(estatura) & between(estatura, min, max) ~ estatura, #pulgadas  no es na y esta entre un min y max, si es asi manten ese resultado
    !is.na(estatura) & between(estatura/2.54, min, max) ~ estatura/2.54, #cm si al dividirlo esta entre el min y max, pues dividelo de vdd
    !is.na(estatura) & between(estatura*100/2.54, min, max) ~ estatura*100/2.54, #metros
    !is.na(est_arregladas) & pulgadas < 12 & 
      between(est_arregladas, min, max) ~ est_arregladas, #pies'pulgadas
    TRUE ~ as.numeric(NA)))
#> Warning in ~as.numeric(.): NAs introduced by coercion

# Muestra aleatoria:
indices_aleatorios <- sample(1:nrow(estaturas)) 
estaturas[indices_aleatorios, ] %>% 
  select(-time_stamp) %>% # Muestra todas las columnas menos time_stamp
  head(10)
#>         sex height estatura pies pulgadas est_arregladas estatura_final
#> 1068 Female    162   162.00   NA       NA             NA       63.77953
#> 401    Male    178   178.00   NA       NA             NA       70.07874
#> 243    Male     72    72.00   NA       NA             NA       72.00000
#> 109    Male     70    70.00   NA       NA             NA       70.00000
#> 513    Male    5.5       NA  5.5        0             66       66.00000
#> 341    Male   74.8    74.80   NA       NA             NA       74.80000
#> 659    Male     68    68.00   NA       NA             NA       68.00000
#> 128  Female     69    69.00   NA       NA             NA       69.00000
#> 1053   Male     66    66.00   NA       NA             NA       66.00000
#> 718    Male  75.98    75.98   NA       NA             NA       75.98000

#Ya tenemos nuestra muestra validada, solo tendríamos que tomar las columnas
#que necesitamos y comenzar a utilizar el objeto para los análisis que necesitemos.

estaturas_final <- estaturas %>% 
  select(genero = sex, estaturas = estatura_final)

estaturas_final %>% 
  head(10)
#>    genero estaturas
#> 1    Male        75
#> 2    Male        70
#> 3    Male        68
#> 4    Male        74
#> 5    Male        61
#> 6  Female        65
#> 7  Female        66
#> 8  Female        62
#> 9  Female        66
#> 10   Male        67

#casos donde necesitamos transformar nuestra cadena a una fecha en algún formato
#en particular. Para ello, utilizaremos la librería lubridate, incluida en tidyverse, 
#la cual nos provee con diversas funciones para hacer el tratamiento de fechas más
#accesible.

library(lubridate)
#Cuando la cadena de texto se encuentra en el formato de fecha ISO 8601 (AAAA-MM-DD), 
#podemos utilizar directamente la función month(), day(), year().

fechas_char <- c("2010-05-19", "2020-05-06", "2010-02-03")

str(fechas_char)
#>  chr [1:3] "2010-05-19" "2020-05-06" "2010-02-03"

month(fechas_char)
#> [1] 5 5 2

#Sin embargo, no siempre tenemos la fecha en ese formato y lubridate() de otras
#funciones que son más flexibles al momento de coercionar datos

fechas <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
            "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")

str(fechas)
#>  chr [1:7] "20090101" "2009-01-02" "2009 01 03" "2009-1-4" "2009-1, 5" ...

ymd(fechas)
#> [1] "2009-01-01" "2009-01-02" "2009-01-03" "2009-01-04" "2009-01-05"
#> [6] "2009-01-06" "2009-01-07"

#El primer dato ingresado fue un número, pero ya sabemos que lo coerciona a texto. 
#Luego, tenemos diferentes valores ingresados, pero todos siguen un mismo patrón. 
#Primero está el año, luego el mes y luego el día.

#Cuando sabemos que primero está  el año, luego mes y luego día usaremos la función
#ymd() para convertir todas las fechas a formato ISO 8601.


#De la misma forma, tendremos las siguientes funciones que podemos utilizar 
#dependiendo de la forma en que tengamos la fecha de nuestra fuente. En todos 
#los casos nos va a convenir convertir a formato ISO 8601. Por ejemplo aquí podemos
#ver cuándo reconoce correctamente el formato y cuánto el formateo falla.

x <- "28/03/89"
ymd(x)
#> [1] NA
mdy(x)
#> [1] NA
ydm(x)
#> [1] NA
myd(x)
#> [1] NA
dmy(x)
#> [1] "1989-03-28" lo transforma al formato ISO 8601
dym(x)
#> [1] NA

#Finalmente, de la misma forma en que podemos utilizar estas funciones de días, 
#meses y años, también podemos usar par referirnos a horas, minutos y segundos.

# Formato con horas, minutos y segundos
fecha <- "Feb/2/2012 12:34:56"
mdy_hms(fecha)
#> [1] "2012-02-02 12:34:56 UTC"

# Dato adicional: Mostrando la fecha del sistema:
now()
#> [1] "2021-07-08 11:24:12 CDT"








