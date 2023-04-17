
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
library(plm)
library(GGally)
library(raster)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.csv("finaldata.csv", header = TRUE, sep = ";")
panel <- pdata.frame(data, index = c("iso", "year"), row.names = TRUE)
average <- read.csv("avgdata.csv", header = TRUE, sep = ";")
map <- read.csv("map.csv", header = TRUE, sep = ";")
world_sp <- sf::st_read("gadm/TM_WORLD_BORDERS-0.3.shp")
freedomap <- merge(world_sp, map, by.x='ISO3',by.y='iso')  


panel$gdppc <- log(panel$gdppc)
panel$pop <- log(panel$pop)
panel$fdi <- log(panel$fdi)

average$gdppc <- log(average$gdppc)
average$pop <- log(average$pop)
average$fdi <- log(average$fdi)

# График индекса свободы прессы ------------------------------------------------
ggplot(data = freedomap)+
  geom_sf(aes(fill = change))+
  xlab("Долгота")+ylab("Широта")+scale_fill_gradient2(na.value = "grey")+
  ggtitle ("Изменение индекса свободы прессы Freedom House с 2000 по 2016 гг.",
           subtitle = paste0("(", length(unique(freedomap$NAME)), " countries)"))

# Описательные статистики ------------------------------------------------------
stargazer(panel, 
          type = "html",
          out = "descrstat.htm",
          title = "Описательные статистики",
          notes = "Число стран = 187",
          notes.align = 'r',
          digits = 2,
          omit.summary.stat = "n",
          median = TRUE)


m.fe1 <- plm(frpressfh ~ gdppc+pop+fdi+intusers+crises+democ, data = panel, model = "within")
m.fe2 <- plm(frpressrsf ~ gdppc+pop+fdi+intusers+crises+democ, data = panel, model = "within")

stargazer(m.fe1,m.fe2, type = "html",
          out = "pols.htm",
          title = "Results of POLS Model")

## Матрица корреляции переменных------------------------------------------------

ggcorr(panel[,6:11],
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       low = "steelblue",
       mid = "white",
       high = "darkred")

# Объединение пространственных и панельных данных ------------------------------
#world_sp <- readOGR("gadm", "TM_WORLD_BORDERS-0.3")
#plot(world_sp)
#DataFrame_sp <- merge(world_sp, average, by.x='ISO3',by.y='iso')



world <- sf::st_read("gadm/TM_WORLD_BORDERS-0.3.shp")
DataFrame <- merge(world, average, by.x='ISO3',by.y='iso')
sf_use_s2(FALSE)

# Матрица смежности ------------------------------------------------------------
DataFrame$country.id<-DataFrame$NAME
DataFrame <- as(DataFrame, "Spatial")
View(DataFrame@data)

queen.nb<-poly2nb(DataFrame,queen=TRUE, row.names=DataFrame$NAME) 
listw1<-nb2listw(queen.nb, style="W", zero.policy = TRUE) #convert nb to listw type
attr(queen.nb, "country.id")<- as.character(DataFrame$country.id)
summary(queen.nb)
plot(DataFrame, col='white', border='grey50', lwd=1)
plot(queen.nb, coordinates(DataFrame), add=TRUE)
title(main = "Соседи по смежности (правило ферзя)")

# Тесты Морана -----------------------------------------------------------------







