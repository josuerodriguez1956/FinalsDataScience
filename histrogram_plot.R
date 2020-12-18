
library(readxl)
library(tidyverse)
library(hrbrthemes)
 proyectofinalde3031 <- read_excel("proyectofinalde3031.xlsx")
proyectofinalde3031<-proyectofinalde3031$rage[-248:-257]
test <- sapply(proyectofinalde3031,as.numeric)
dfedades <- as.data.frame(test)
histograma_edades <- ggplot(dfedades, aes(x = test)) + geom_histogram(binwidth = 7, fill="#69b3a2" ) +
  theme_ipsum() + theme(
    plot.title = element_text(size=15)
  ) +  ggtitle("Feminicidios por edad desde el año 2014 hasta 2018") + xlab("edades") + ylab("frecuencia de casos") +
  scale_x_continuous(n.breaks = 15)
##histograma_edades <- hist(x = test) +

histograma_edades


