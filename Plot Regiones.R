library(tidyverse)
library(readxl)
library(hrbrthemes)

feminicidios <- read_excel("Workbook1.xlsx")

Aibonito <- c('Adjuntas', 'Aibonito', 'Barranquitas', 'Coamo', 'Comerio', 'Orocovis')

Aguadilla <- c('Aguada', 'Aguadilla', 'Isabela', 'Moca', 'Rincon', 'San Sebastian')

Arecibo <- c('Arecibo', 'Barceloneta', 'Camuy', 'Ciales', 'Florida', 'Hatillo', 'Manati', 'Morovis', 'Quebradillas')

Bayamon <- c('Bayamon', 'Cataño', 'Corozal', 'Dorado', 'Guaynabo', 'Naranjito', 'Toa Alta', 'Toa Baja', 'Vega Alta', 'Vega Baja')

Caguas <- c('Aguas Buenas', 'Caguas', 'Cidra', 'Gurabo', 'Juncos', 'San Lorenzo')

Carolina <- c('Carolina', 'Canovanas', 'Loiza', 'Trujillo Alto')

Guayama <- c('Arroyo', 'Cayey', 'Guayama', 'Patillas', 'Salinas')

Fajardo <- c('Ceiba', 'Culebra', 'Fajardo', 'Luquillo', 'Maunabo', 'Rio Grande', 'Vieques')

Humacao <- c('HUmacao', 'Las Piedras', 'Naguabo', 'Yabucoa')

Mayaguez <- c('Anasco', 'Cabo Rojo', 'Hormigueros', 'Lajas', 'Las Marias', 'Maricao', 'Mayaguez', 'San German', 'Sabana Grande')

Ponce <- c('Ponce', 'Guanica', 'Guayanilla', 'Juana Diaz', 'Penuelas', 'Santa Isabel', 'Villalba', 'Yauco')

SanJuan <- 'San Juan'

Utuado <- c('Jayuya', 'Lares', 'Utuado')


regiones <- list("Aguadilla" = Aguadilla, "Aibonito" = Aibonito, 
                 "Arecibo" = Arecibo, "Bayamón" = Bayamon, " Caguas" = Caguas, 
                 "Carolina" = Carolina, " Fajardo" = Fajardo, 
                 "Guayama" = Guayama, "Humacao" = Humacao, "Mayagüez" = Mayaguez, 
                 "Ponce" = Ponce, "San Juan" = SanJuan, "Utuado" = Utuado)



por_mun <- feminicidios %>%
  select(rmrip) %>%
  group_by(rmrip) %>%
  count()


lista <- c(1:13)

count = 1
for (x in regiones) {
 
  respuesta <- por_mun %>% 
  filter(rmrip %in% x)
  
  
  lista[count] <- sum(respuesta$n)
  count = count + 1
  
}

df <- data.frame(names(regiones), lista)




barsmun <- ggplot(df, aes(names.regiones., lista, fill= names.regiones.))+
  geom_col(show.legend = FALSE) + theme_ipsum() +
  theme(plot.title = element_text(size = 15))+
  ggtitle("Feminicidios por comandancia del DSPR desde 2014 hasta 2018") +
  ylab("Cantidad\n") + xlab("\nComandancias") + geom_text(label = lista, vjust = -0.5)

barsmun