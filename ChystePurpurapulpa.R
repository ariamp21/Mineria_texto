

Chyste_MC <- read_excel("C:/Users/user/Documents/R/Chyste_pulpapurpura.xlsx")

letra_limpia <- Chyste_MC %>% 
  str_to_lower() %>% 
  str_replace_all(., "[[:cntrl:]]", " ")

resultado <- get_nrc_sentiment(letra_limpia, language = "spanish")

resultado %>% 
  head(10)


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

sentimientos <- resultado %>% 
  gather(sentimiento, cantidad) %>% 
  mutate(sentimiento = trad_emociones(sentimiento)) %>% 
  group_by(sentimiento) %>% 
  summarise(total = sum(cantidad))

sentimientos

index <- sentimientos$sentimiento %in% c("Positivo", "Negativo") 

# Visualización de emociones
sentimientos[!index,] %>% 
  ggplot() +
  aes(sentimiento, total) +
  geom_bar(aes(fill = sentimiento), stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab(NULL) +
  ylab("Total") +
  ggtitle("Emociones en Purpura pulpa - Chyste MC")

# Visualización de si son sentimientos positivos o negativos:
sentimientos[index,] %>% 
  ggplot() +
  aes(sentimiento, total) +
  geom_bar(aes(fill = sentimiento), stat = "identity") +
  xlab(NULL) +
  ylab("Total") +
  ggtitle("Sentimientos de Purpura pulpa - Chyste MC")

