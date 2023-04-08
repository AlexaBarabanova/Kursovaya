
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


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- read.csv("finaldata.csv", header = TRUE, sep = ";")
panel <- pdata.frame(data, index = c("iso", "year"), row.names = TRUE)

panel$gdppc <- log(panel$gdppc)
panel$pop <- log(panel$pop)
panel$fdi <- log(panel$fdi)

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
ggcorr(panel, 
       nbreaks = 6,
       label = TRUE,
       label_size = 2,
       color = "grey50")


# Объединение пространственных и панельных данных ------------------------------
world <- ne_countries (scale = "medium", returnclass = "sf")
DataFrame <- merge(world, data, by.x='su_a3',by.y='iso')

# График индекса свободы прессы ------------------------------------------------
ggplot (data = DataFrame)+
  geom_sf(aes(fill = frpressh2016))+
  xlab("Долгота")+ylab("Широта")+scale_fill_gradient2(na.value = "grey")+
  ggtitle ("Распределение индекса свободы прессы Freedom House в 2016 г.")

# Матрица смежности ------------------------------------------------------------









