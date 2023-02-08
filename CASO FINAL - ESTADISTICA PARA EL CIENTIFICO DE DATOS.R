#-------------------------------------------------
#     CASO FINAL ESTADISTICA DEL CIENTIFICO DE DATOS
#
#   TEMA: ANÁLISIS DEL COSTO DE LA VIVIENDA EN BOSTON
#
# INTEGRANTES:
#
#   - Maria Fernanda Montufar
#   - Maria auxiliadora Montalvo
#   - Marco Rodriguez
#   - Victor Donoso
#-------------------------------------------------

#-------------------------------------------------
# 1)  INSTALACION DE PAQUETES NECESARIOS PARA EL CASO       
#-------------------------------------------------

#install.packages("readr", dependencias = TRUE)
#install.packages("ggplot2", dependencias = TRUE)
#install.packages("corrplot", dependencias = TRUE)
#install.packages("mlbench", dependencias = TRUE)
#install.packages("Amelia", dependencias = TRUE)
#install.packages("plotly", dependencias = TRUE)
#install.packages("reshape2", dependencias = TRUE)
#install.packages("lattice", dependencias = TRUE)
#install.packages("dplyr", dependencias = TRUE)
#install.packages("caTools", dependencias = TRUE)
#install.packages("ipred", dependencias = TRUE)
#install.packages("tibble", dependencias = TRUE)
#install.packages("caret", dependencias = TRUE)
#install.packages("PerformanceAnalytics", dependencias = TRUE)
install.packages("agricolae", dependencias = TRUE)

#-------------------------------------------------
# 2) LLAMADA DE LAS LIBRERIAS      
#-------------------------------------------------

library(PerformanceAnalytics)
library(readr)
library(ggplot2)
library(corrplot) 
library(mlbench) 
library(Amelia) 
library(plotly) 
library(reshape2) 
library(lattice)
library(dplyr)
library(caTools) 
library(ipred)
library(tibble)
library(caret)
library(agricolae)

#-------------------------------------------------
# 3) CARGA DE DATOS    
#-------------------------------------------------

# RUTA DONDE SE ENCUENTRA EL ARCHIVO###

path <- "C:/Users/59398/Downloads/boston.csv"

BostonHousing<-read.csv(path)

#VISUALIZACION DE DATOS

View(BostonHousing)

# ASIGNACION A VARIABLE HOUSING

housing <- BostonHousing 

# VISUALIZAR LOS CAMPOS Y LOS TIPOS DE DATOS

str(housing) 

#-------------------------------------------------
# 4) VISUALIZAR ALGUNAS OBSERVACIONES CON SUS ENCABEZADOS   
#-------------------------------------------------

head(housing)

#-------------------------------------------------
# 5)	LIMPIEZA DE DATOS  
#-------------------------------------------------

# VISUALIZAR OBSERVACIONES CARGADAS CORRECTAMENTE VS LAS OBSERVACIONES SIN VALORES PERDIDOS

missmap(housing,col=c('yellow','blue'),y.at=1,y.labels='',legend=TRUE)

#-------------------------------------------------
# 6)	ANÁLISIS EXPLORATORIO DE LOS DATOS
#-------------------------------------------------

summary(housing)

# CORRELACION

corrplot(cor(select(housing,-chas))) 
correlacion<-corrplot(cor(select(housing,-chas)))
correlacion

# GRAFICO DE LA DENSIDAD VARIALE INDENDIETE MEDV

housing %>% 
  ggplot(aes(medv)) +
  stat_density() + 
  theme_bw()

# GRAFICO DE LA RELACION DE LAS VARIABLES VS VARIBLE MEDV

housing %>%
  select(c(crim, rm, age, rad, tax, lstat, medv,indus,nox,ptratio,zn)) %>%
  melt(id.vars = "medv") %>%
  ggplot(aes(x = value, y = medv, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Median House Price ($1000s)") +
  theme_minimal()

#-------------------------------------------------
# 7) ESTADISTICA DESCRIPTIVA
#-------------------------------------------------

# VALORES DE MEDV RESPECTO A FRECUENCIA, FRECUENCIA ACUMULADA, FRECUENCIA RELATIVA Y FRECUENCIA RELATIVA ACUMULADA.

tbFreqmedv=table.freq(hist(housing$medv,plot=FALSE))
tbFreqmedv

# VALORES DE NOX RESPECTO A FRECUENCIA, FRECUENCIA ACUMULADA, FRECUENCIA RELATIVA Y FRECUENCIA RELATIVA ACUMULADA.

tbFreqnox=table.freq(hist(housing$nox,plot=FALSE))
tbFreqnox


# SE OBTIENE LOS RANGOS DE LA VARIABLE MEDV

range(housing$medv,na.rm=TRUE)

# [1]  5 50

# INTERVALOS DE LA VARIABLE MEDV

summary(housing$nox)

intervalosm=cut(housing$medv,breaks=seq(5,50
                ,length=nclass.Sturges(housing$medv))
                ,include.lowest=TRUE)
histogram(intervalosm)

# SE OBTIENE LOS RANGOS DE LA VARIABLE NOX

range(housing$nox,na.rm=TRUE)

#[1] 0.385 0.871

# INTERVALOS DE LA VARIABLE NOX

intervalosn=cut(housing$nox,breaks=seq(0.385, 0.871
                ,length=nclass.Sturges(housing$nox))
                ,include.lowest=TRUE)
histogram(intervalosn)

# GRAFICO DE LA RELAICON ENTRE LAS VARIABLES MEDV Y NOX

gg<-ggplot(housing, aes(x=housing$medv, y=housing$nox))+
  geom_point(aes(col=housing$medv, size=housing$nox))+
  geom_smooth(method="loess", se=F)+
  xlim(c(5,50))+
  ylim(c(0.385,0.871))+
  labs(subtitle = "b", 
       y="housing$nox", 
       x="housing$medv",
       title = "Relacion medv vs nox ",
       caption="Source: housing")
plot(gg)

#-------------------------------------------------
# 8) ENTRENAMIENTO DEL MODELO
#-------------------------------------------------

# DATAFRAME DE ENTRENAMIENTO

set.seed(123) 

# SE TOMARÁ COMO REFERENCIA AL 75% DEL DATASET ORIGINAL QUE SE DETALLA A CONTINUACIÓN:


split <- sample.split(housing,SplitRatio =0.75)
train <- subset(housing,split==TRUE)
test <- subset(housing,split==FALSE)

#-------------------------------------------------
# 9) REGRESIÓN LINEAL
#-------------------------------------------------

# RESULTADOS DE LA REGRESION MULTIPLE

model <- lm(medv ~ -1 + nox +  rm+tax, data = train) 
summary(model)  

# ANÁLISIS DE LOS RESIDUOS

res <- residuals(model)
res <- as.data.frame(res)

ggplot(res,aes(res))+  geom_histogram(fill='blue',alpha=0.5)

# DISPERSION Y TENDENCIAN RESIDUAL

plot(model)


test$predicted.medv <- predict(model,test)

# GRAFICO MEDV (VALOR DE LA PROPIEDAD) VS LA PREDICCIÓN DEL VALOR

pl1 <-test %>% 
  ggplot(aes(medv,predicted.medv)) +
  geom_point(alpha=0.5,color="#d06aff") + 
  stat_smooth(aes(colors='blue')) +
  xlab('Valor del Precio Actual') +
  ylab('Predicción del valor') 
ggplotly(pl1)

#-------------------------------------------------
# 10) ANOVA
#-------------------------------------------------

anova(model)  
