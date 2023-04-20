# Курсовая работа, учебный год 2022-2023
# Образовательная программа "Экономика", НИУ ВШЭ, г. Санкт-Петербург
# Тема: "Пространственная взаимозависимость стран по уровню свободы СМИ"
# Выполнила Барабанова Александра и Киричек Татьяна


#Загрузка пакетов --------------------------------------------------------------
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
library(sandwich)
library(broom)
library(ggpubr)

#Директория сохранения ---------------------------------------------------------
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

## Матрица корреляции переменных------------------------------------------------

ggcorr(panel[,6:11],
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       low = "steelblue",
       mid = "white",
       high = "darkred")

## Отдельные датафреймы для годов ----------------------------------------------
data <- data[-c(3101:3120,1761:1780), ]
panel <- pdata.frame(data, index = c("iso", "year"), row.names = TRUE)

panel$gdppc <- log(panel$gdppc)
panel$pop <- log(panel$pop)
panel$fdi <- log(panel$fdi)

data2000 = panel %>% dplyr::filter(year == '2000')
data2001 = panel %>% dplyr::filter(year == '2001')
data2002 = panel %>% dplyr::filter(year == '2002')
data2003 = panel %>% dplyr::filter(year == '2003')
data2004 = panel %>% dplyr::filter(year == '2004')
data2005 = panel %>% dplyr::filter(year == '2005')
data2006 = panel %>% dplyr::filter(year == '2006')
data2007 = panel %>% dplyr::filter(year == '2007')
data2008 = panel %>% dplyr::filter(year == '2008')
data2009 = panel %>% dplyr::filter(year == '2009')
data2010 = panel %>% dplyr::filter(year == '2010')
data2011 = panel %>% dplyr::filter(year == '2011')
data2012 = panel %>% dplyr::filter(year == '2012')
data2013 = panel %>% dplyr::filter(year == '2013')
data2014 = panel %>% dplyr::filter(year == '2014')
data2015 = panel %>% dplyr::filter(year == '2015')
data2016 = panel %>% dplyr::filter(year == '2016')
data2017 = panel %>% dplyr::filter(year == '2017')
data2018 = panel %>% dplyr::filter(year == '2018')
data2019 = panel %>% dplyr::filter(year == '2019')

map2019 <- merge(world_sp, data2019, by.x='ISO3',by.y='iso')
map2019$gdppc <- exp(map2019$gdppc)
map2019$pop <- exp(map2019$pop)
map2019$fdi <- exp(map2019$fdi)

# Другие графики ---------------------------------------------------------------
## Свобода прессы RSF
p1 <- ggplot(data = map2019)+
  geom_sf(aes(fill = frpressrsf))+
  xlab("Долгота")+ylab("Широта")+ 
  scale_fill_viridis_c(option = "C", trans = "sqrt")+
  ggtitle ("Распределение индекса свободы прессы RSF в 2019г.",
           subtitle = paste0("(", length(unique(map2019$NAME)), " countries)"))
## Логарифм ВВП на душу
p2 <- ggplot(data = map2019)+
  geom_sf(aes(fill = gdppc))+
  xlab("Долгота")+ylab("Широта")+ 
  scale_fill_viridis_c(option = "D", trans = "sqrt")+
  ggtitle ("Распределение ВВП на душу населения в 2019г.",
           subtitle = paste0("(", length(unique(map2019$NAME)), " countries)"))
## Численность населения
p3 <- ggplot(data = map2019)+
  geom_sf(aes(fill = pop))+
  xlab("Долгота")+ylab("Широта")+ 
  scale_fill_viridis_c(option = "F", trans = "sqrt")+
  ggtitle ("Распределение численности населения в 2019г.",
           subtitle = paste0("(", length(unique(map2019$NAME)), " countries)"))
## ПИИ
p4 <- ggplot(data = map2019)+
  geom_sf(aes(fill = fdi))+
  xlab("Долгота")+ylab("Широта")+ 
  scale_fill_viridis_c(option = "C", trans = "sqrt")+
  ggtitle ("Распределение ПИИ в 2019г.",
           subtitle = paste0("(", length(unique(map2019$NAME)), " countries)"))
## Доля Интернет-пользователей
p5 <- ggplot(data = map2019)+
  geom_sf(aes(fill = intusers))+
  xlab("Долгота")+ylab("Широта")+ 
  scale_fill_viridis_c(option = "C", trans = "sqrt")+
  ggtitle ("Распределение пользователей Интернета в 2019г.",
           subtitle = paste0("(", length(unique(map2019$NAME)), " countries)"))
## Число переворотов и кризисов правительства
p6 <- ggplot(data = map2019)+
  geom_sf(aes(fill = crises))+
  xlab("Долгота")+ylab("Широта")+ 
  scale_fill_viridis_c(option = "H", trans = "sqrt")+
  ggtitle ("Распределение кризисов власти в 2019г.",
           subtitle = paste0("(", length(unique(map2019$NAME)), " countries)"))
## Тип режима
p7 <- ggplot(data = map2019)+
  geom_sf(aes(fill = democ))+
  xlab("Долгота")+ylab("Широта")+ 
  scale_fill_viridis_c(option = "B", trans = "sqrt")+
  ggtitle ("Распределение демократии в 2019г.",
           subtitle = paste0("(", length(unique(map2019$NAME)), " countries)"))
distribution <- ggarrange(p1, p2, p3, p4, p5, p6, p7, ncol = 2, nrow=4)
distribution

# Объединение пространственных и усредненных данных ------------------------------
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

# OLS Model --------------------------------------------------------------------
average1 <- average[complete.cases(average[ , ]), ]

DataFrameRes <- merge(world, average1, by.x='ISO3',by.y='iso')
sf_use_s2(FALSE)
DataFrameRes$country.id<-DataFrameRes$NAME
DataFrameRes <- as(DataFrameRes, "Spatial")
queen.nbres<-poly2nb(DataFrameRes,queen=TRUE, row.names=DataFrameRes$NAME) 
listw2<-nb2listw(queen.nbres, style="W", zero.policy = TRUE) #convert nb to listw type
attr(queen.nbres, "country.id")<- as.character(DataFrame$country.id)
summary(queen.nbres)

res1 <- DataFrameRes$frpressfh ~ DataFrameRes$gdppc + DataFrameRes$pop + DataFrameRes$fdi + DataFrameRes$intusers + DataFrameRes$crises + DataFrameRes$democ
res2 <- DataFrameRes$frpressrsf ~ DataFrameRes$gdppc + DataFrameRes$pop + DataFrameRes$fdi + DataFrameRes$intusers + DataFrameRes$crises + DataFrameRes$democ

ols1 <- lm(res1, data = DataFrameRes)
ols2 <- lm(res2, data = DataFrameRes)
cf1 <- coeftest(ols1, df=Inf, vcov=vcovHC, type = "HC0")
cf2 <- coeftest(ols2, df=Inf, vcov=vcovHC, type = "HC0")

stargazer(cf2,cf1, type = "html",
          out = "ols.htm",
          title = "Результаты регрессий методом МНК")


# Тесты Морана -----------------------------------------------------------------
## Усредненные данные
# H0 - no spatial autocorrelation
moran.test(DataFrame$frpressfh, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected  p-value = 2.183e-15 Moran I=0.495180178

#moran.plot(DataFrame$frpressfh, listw1, zero.policy=FALSE, xlab="Свобода прессы Freedom House", ylab="Пространственный лаг свободы прессы FH")
#title = "Диаграмма Морана"

moran.test(DataFrame$frpressrsf, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 2.2e-16 Moran I=0.520363392

#moran.plot(DataFrame$frpressrsf, listw1, zero.policy=FALSE, xlab="Свобода прессы RSF", ylab="Свобода прессы RSF с пространственным отставанием")

moran.test(DataFrame$gdppc, listw1, zero.policy=TRUE)
# H0 is rejected p-value = 2.2e-16 Moran I=0.603638532

#moran.plot(DataFrame$gdppc, listw1, zero.policy=FALSE)

moran.test(DataFrame$pop, listw1, zero.policy=TRUE)
# H0 is rejected only on 5% p-value = 0.01538 Moran I=0.130290766

#moran.plot(DataFrame$pop, listw1, zero.policy=FALSE)

moran.test(DataFrame$fdi, listw1, zero.policy=TRUE)
# H0 is rejected p-value = 2.341e-05 Moran I=0.251539345

#moran.plot(DataFrame$fdi, listw1, zero.policy=FALSE)

moran.test(DataFrame$intusers, listw1, zero.policy=TRUE)
# H0 is NOT rejected p-value = 0.3326 Moran I=0.020727519

#moran.plot(DataFrame$intusers, listw1, zero.policy=FALSE)

moran.test(DataFrame$crises, listw1, zero.policy=TRUE)
# H0 is NOT rejected p-value = 0.162 Moran I=0.054907587 

#moran.plot(DataFrame$crises, listw1, zero.policy=FALSE)

moran.test(DataFrame$democ, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 2.2e-16 Moran I=0.583000581  

#moran.plot(DataFrame$democ, listw1, zero.policy=FALSE)


## Остатки OLS модели
resid1 <- ols1$residuals
summary(resid1)
plot(coordinates(DataFrameRes),pch=21, bg="green", cex=0.05*(resid1-min(resid1)), xlab = "Долгота", ylab = "Широта", main = "Распределение остатков МНК-модели") 
# spatial autocorrelation of residuals
moran.test(resid1, listw2, zero.policy=TRUE)
moran.plot(resid1, listw2, zero.policy=FALSE, xlab="Остатки", ylab="Остатки с пространственным лагом", main = "Распределение остатков МНК-модели")
# choose the spatial model
lm.morantest(ols1, listw2, zero.policy=TRUE)
# H0 is rejected p-value = 0.09025 Moran I=0.069848605
lm.LMtests(ols1, listw2, zero.policy=TRUE, test=c("LMerr", "LMlag", "RLMerr", "RLMlag")) 

resid2 <- ols2$residuals
summary(resid2)
plot(coordinates(DataFrame),pch=21, bg="green", cex=0.05*(resid2-min(resid2)), xlab = "Долгота", ylab = "Широта", main = "Распределение остатков МНК-модели")
# spatial autocorrelation of residuals
moran.test(resid2, listw2, zero.policy=TRUE)
moran.plot(resid2, listw2, zero.policy=FALSE, xlab="Остатки", ylab="Остатки с пространственным лагом", main = "Распределение остатков МНК-модели")
# choose the spatial model
lm.morantest(ols2, listw2, zero.policy=TRUE)
lm.LMtests(ols2, listw2, zero.policy=TRUE, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))

# SEM (Spatial error model):  y = X beta + u, u = lambda Wu + e --------------
res3 <- DataFrame$frpressrsf ~ DataFrame$gdppc + DataFrame$pop + DataFrame$fdi + DataFrame$intusers + DataFrame$crises + DataFrame$democ

sem <- errorsarlm(res3, listw=listw1, data=DataFrame, zero.policy=TRUE)
stargazer(sem, type = "html",
          out = "sem.htm",
          title = "Результаты SER модели") 
summary(sem)

# SDM (Spatial Durbun model): Spatially Lagged Y and Spatially Lagged X: y=rho Wy+X beta+WXT+e --------------
sdm <- lagsarlm(res2, listw=listw2, data=DataFrameRes, Durbin=TRUE, zero.policy=TRUE)
imp<-impacts(sdm, listw=listw2)
imp


## Панельные данные ------------------------------------------------------------

moran.test(data2000$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 1.478e-13 Moran I=0.461890132
moran.test(data2001$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 1.354e-14 Moran I=0.481777172
moran.test(data2002$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 3.845e-14 Moran I=0.473157924
moran.test(data2003$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 1.124e-13 Moran I=0.462655814
moran.test(data2004$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 8.296e-14 Moran I=0.465150020
moran.test(data2005$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 1.12e-13 Moran I=0.462538700
moran.test(data2006$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 1.07e-13 Moran I=0.462787030
moran.test(data2007$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 1.692e-14 Moran I=0.478311858 
moran.test(data2008$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 2.61e-14 Moran I=0.474710175
moran.test(data2009$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 2.09e-14 Moran I=0.476547193
moran.test(data2010$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 4.525e-14 Moran I=0.470113145
moran.test(data2011$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 3.4e-14 Moran I=0.472436182
moran.test(data2012$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 6.261e-14 Moran I=0.467235747
moran.test(data2013$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 3.408e-14 Moran I=0.472314120
moran.test(data2014$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 1.313e-14 Moran I=0.480308097
moran.test(data2015$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 1.053e-14 Moran I=0.482136825
moran.test(data2016$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 1.11e-15 Moran I=0.485555514
moran.test(data2017$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 1.478e-13 Moran I=0.500366071
moran.test(data2018$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 1.11e-15 Moran I=0.485555514
moran.test(data2019$pop, listw1, zero.policy=TRUE, na.action=na.omit)
# H0 is rejected p-value = 1.478e-13 Moran I=0.500366071

## POLS ------------------------------------------------------------------------

m.fe1 <- plm(frpressfh ~ gdppc+pop+fdi+intusers+crises+democ, data = panel, model = "within")
m.fe2 <- plm(frpressrsf ~ gdppc+pop+fdi+intusers+crises+democ, data = panel, model = "within")

stargazer(m.fe1,m.fe2, type = "html",
          out = "pols.htm",
          title = "Results of POLS Model")

## Остатки панельной регрессионной модели
res1 <- ols1$residuals
summary(res1)
plot(coordinates(DataFrame),pch=21, bg="green", cex=10*(res1-min(res1))) 

# spatial autocorrelation of residuals
moran.test(res1, listw1, zero.policy=TRUE)
moran.plot(res1, listw1, zero.policy=FALSE, xlab="Residuals", ylab="Spatially lagged residuals")

# choose the spatial model
lm.morantest(ols1, listw1, zero.policy=TRUE)
lm.LMtests(ols1, listw1, zero.policy=TRUE, test=c("LMerr", "LMlag", "RLMerr", "RLMlag")) 



















