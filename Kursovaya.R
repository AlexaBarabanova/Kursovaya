
library(sp)
library(rgdal)
library(maptools)
library(dplyr) # манипуляции с данными
library(tidyr) # манипуляции с данными
library(ggplot2) # визуализация данных
library(stargazer)
library(spData)
library(spdep)
library(lmtest)
library(spatialreg)
library(rnaturalearth)
library(rnaturalearthdata)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- read.csv("mappressfreedom.csv", header = TRUE, sep = ";")

world <- ne_countries (scale = "medium", returnclass = "sf")

DataFrame <- merge(world, data, by.x='sov_a3',by.y='iso')

ggplot (data = DataFrame)+
  geom_sf(aes(fill = frpressh2016))+
  xlab("Долгота")+ylab("Широта")+scale_fill_gradient2(na.value = "white")+
  ggtitle ("Распределение индекса свободы прессы Freedom House в 2016 г.")











