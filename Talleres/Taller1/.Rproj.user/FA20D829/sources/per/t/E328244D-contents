library(tidyverse)

carpetas1 <- read_csv("https://archivo.datos.cdmx.gob.mx/FGJ/carpetas/carpetasFGJ_2023_07.csv")
carpetas2 <- read_csv("https://archivo.datos.cdmx.gob.mx/FGJ/victimas/victimasFGJ_2023_07.csv")

#Some pre-processing
relevant_columns <- c("anio_hecho","mes_hecho","fecha_hecho","hora_hecho",
                      "delito","categoria")

carpetas1 <- carpetas1 %>% select(all_of(relevant_columns))
carpetas1$sexo <- "Desconocido"
carpetas2 <- carpetas2 %>% select(all_of(relevant_columns),sexo)

carpetas <- rbind(carpetas1,carpetas2)

#Tomemos solo eventos del 2000 para acá

crimen_post2017 <- carpetas %>% 
  filter(fecha_hecho > "2017-12-31") %>%
  group_by(day = lubridate::floor_date(fecha_hecho, "day")) %>% summarise(eventos = n())

crimen_post2017.week <- carpetas %>% 
  filter(fecha_hecho > "2017-12-31") %>%
  group_by(semana = lubridate::floor_date(fecha_hecho, "week")) %>% summarise(eventos = n())
ggplot(crimen_post2010.week) + geom_line(aes(x=semana,y=eventos))


#Delitos comunes
delitos.comunes <- c("VIOLENCIA FAMILIAR",
             "FRAUDE",
             "AMENAZAS"
             )

delitos.interesantes <- c("INHUMACION, EXHUMACION Y RESPETO A LOS CADAVERES O RESTOS HUMANOS",
"CONTAMINACIÓN O RESIDUOS")

carpetas.delitos <- carpetas %>% filter(delito %in% delitos.comunes) %>%  filter(fecha_hecho > "2017-12-31") 
carpetas.interesantes <- carpetas %>% filter(delito %in% delitos.interesantes) %>%  filter(fecha_hecho > "2017-12-31") 
carpetas.interesantes %>% group_by(dia=lubridate::floor_date(fecha_hecho,"day"),delito) %>% summarise(numero = n())%>%
  ggplot() + geom_line(aes(x=dia,y=numero, color=delito))
