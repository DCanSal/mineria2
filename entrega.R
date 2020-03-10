#Lo primero que hemos de hacer es reubicarnos en la carpeta de trabajo
setwd('D:/Universidad/master/documentacion/Master BDBA/mineria/SegundaParte/')
#estas son todas las librerias que necesitaremos para llevar a cabo la tarea
library(readxl)
library(pastecs)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(ggplot2)


#para realizar la matriz de covarianzas nos deshacemos de la primera columna con los 
#nombres de las provincias.
provincias <- read_excel("Provincias.xlsx")
datos      <- provincias[,-1]
#por probar y sacar los descriptivos. Luego podemos eliminarlo. 
Est        <-stat.desc(datos, basic = F)

#La primera parte del ejercicio requiere que hagamos la matriz de correlacion
#de coeficientes Pearson
matrizR       <- cor(datos, method = "pearson")
corrPlotDatos <- corrplot(matrizR, type="upper", order="hclust",tl.col="black", tl.srt=90)
#en este corrplot se aprecia que las correlaciones negativas se encuentran en:
#Natalidad - Mortalidad
#TasaActividad - Mortalidad
#TasaParo - Mortalidad
#TasaParo - IPC

#A continuacion hemos de realizar un analisis de componentes principales. Para este
#caso hemos de calclular 7 componentes.

fit       <- PCA(datos, scale.unit=T, ncp = 7, graph = T)
visualfit <- head(fit)

fviz_eig(fit, addlabels = T)
#en este grafico se aprecia claramente que un modelo con 3 componentes suma un 87 por cierto
#de la explicacion del modelo.

visualfit["var"]
#en el apartado $var$cor
#Dim 1: poblacion 0.9, NumEmpresas 0.99, Industria 0.96, Construccion 0.99, CTH 0.99 Infor 0.95, AFS 0.99
#APT 0.98, Ocupados 0.99, PIB 0.98, TVF 0.98
#Dim 2: Mortalidad -0.84, Natalidad 0.79, TasaParo 0.73
#Dim 3: CANE 0.83
#la suma de los tres primeros componentes da un modelo de 87%
#la suma de los cuatro primeros componentes da un modelo de 92,2%

fitLimpio       <- PCA(datos, scale.unit=T, ncp = 3, graph = T)
visualFitLimpio <- head(fitLimpio)

#3 Expresion para calcular la primera componente
visualFitLimpio["svd"]
#CP1 = 0.29POB - -0.10MORT + 0.04NAT + 0.1IPC + 0.29NUMEM ... + 0.17VS
#Tabla de las variables de las componentes principales y cuales son las mas correlacionadas


visualfit["var"]
#en el apartado $var$cor
#Dim 1: poblacion 0.9, NumEmpresas 0.99, Industria 0.96, Construccion 0.99, CTH 0.99 Infor 0.95, AFS 0.99
#APT 0.98, Ocupados 0.99, PIB 0.98, TVF 0.98
#Dim 2: Mortalidad -0.84, Natalidad 0.79, TasaParo 0.73
#Dim 3: CANE 0.83


head(fitLimpio["eig"])
#la suma de los tres primeros componentes da un modelo de 87%
#la suma de los cuatro primeros componentes da un modelo de 92,2%
#priorizaremos la menor cantidad de componentes y nos quedaremos con los tres primeros componentes

var<-get_pca_var(fitLimpio)
print(var$cos2)
corrplot(var$cos2,is.corr=FALSE)
#para constatar que variable esta menos explicada hacemos la media de todas las filas
#y las mostramos en orden descendente. La peor explicada, es VS.
sort(apply(var$cos2,1,mean), decreasing = T)

#Porcentaje de variabilidad explicada por las tres CP
fviz_cos2(fitLimpio,choice="var",axes=1:3)