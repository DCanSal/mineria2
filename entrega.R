#Lo primero que hemos de hacer es reubicarnos en la carpeta de trabajo
setwd('D:/Universidad/master/documentacion/Master BDBA/mineria/SegundaParte/')
#estas son todas las librerias que necesitaremos para llevar a cabo la tarea
library(readxl)
library(pastecs)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(cluster)
library(NbClust)



#para realizar la matriz de covarianzas nos deshacemos de la primera columna con los 
#nombres de las provincias.
provincias <- as.data.frame(read_excel("Provincias.xlsx"))
rownames(provincias) <- provincias[,1]
datos      <- provincias[,-1]

#por probar y sacar los descriptivos. Luego podemos eliminarlo. 
Est        <- stat.desc(datos, basic = F)

#La primera parte del ejercicio requiere que hagamos la matriz de correlacion
#de coeficientes Pearson


matrizR       <- cor(datos, method = "pearson") #matriz de correlaciones
corrPlotDatos <- corrplot(matrizR, type="upper", order="hclust",tl.col="black", tl.srt=90)
#en este corrplot se aprecia que las correlaciones inversas se encuentran en:
#Natalidad - Mortalidad
#TasaActividad - Mortalidad
#TasaParo - Mortalidad
#TasaParo - IPC

#A continuacion hemos de realizar un analisis de componentes principales. Para este
#caso hemos de calclular 7 componentes.

fit       <- PCA(datos, scale.unit=T, ncp = 7, graph = T) #Analisis de componentes, ncp = 7
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

#Volvemos a realizar la matriz de correlaciones con 3 componentes, que son los que estimamos suficientes.

fitLimpio       <- PCA(datos, scale.unit=T, ncp = 3, graph = T)
visualFitLimpio <- head(fitLimpio)

#3 Expresion para calcular la primera componente
visualFitLimpio["svd"]
#CP1 = 0.29POB - -0.10MORT + 0.04NAT + 0.1IPC + 0.29NUMEM ... + 0.17VS
#Tabla de las variables de las componentes principales y cuales son las mas correlacionadas


visualFitLimpio["var"] #Tabla con las correlaciones de las Variables con las Componentes Principales.
#en el apartado $var$cor
#Dim 1: poblacion 0.98, NumEmpresas 0.99, Industria 0.93, Construccion 0.98, CTH 0.98 Infor 0.90, AFS 0.98
#APT 0.96, Ocupados 0.99, PIB 0.97, TVF 0.97
#Dim 2: Mortalidad -0.84, Natalidad 0.79, TasaParo 0.73
#Dim 3: CANE 0.83





#Comentar los graficos que representan las variables en los planos formados por las componentes explicando lo que
#representan
graficosDim12 <- fviz_pca_var(fitLimpio, axes = c(1, 2), col.var="cos2", gradient.cols = c("#00AFBB", "#E7B800","#FC4E07"), repel = TRUE )
#Dim 1: CTH, Poblacion, TVF, InfoIndustria, APT, PIB, Construccion
#VS en menor medida. 
graficosDim13 <- fviz_pca_var(fitLimpio, axes = c(1, 3), col.var="cos2", gradient.cols = c("#00AFBB", "#E7B800","#FC4E07"), repel = TRUE )
#Dim 2: Natalidad, Mortalidad, Tasa Paro
graficosDim23 <- fviz_pca_var(fitLimpio, axes = c(2, 3), col.var="cos2", gradient.cols = c("#00AFBB", "#E7B800","#FC4E07"), repel = TRUE )
#Dim 3: CANE


# d. mostrar la tabla y graficos que nos muestran la proporcion de la varianza de cada variable
#que es explicado por cada componente. ¿Cual de las variables esta peor explicada?
var<-get_pca_var(fitLimpio)
print(var$cos2)
corrplot(var$cos2,is.corr=FALSE)
#para constatar que variable esta menos explicada hacemos la media de todas las filas
#y las mostramos en orden descendente. La peor explicada, es VS.
sort(apply(var$cos2,1,mean), decreasing = T)

#Porcentaje de variabilidad explicada por las tres CP. También
#se ve claramente que VS es la variable peor explicada.
fviz_cos2(fitLimpio,choice="var",axes=1:3)

#queremos los graficos y tablas de la varianza explicada en cada componente y que variable
#se debe. ¿Que variables contribuyen mas en cada componente? para esta parte de la tarea hemos 
#de pedir los mismos graficos y tablas solo que con la lista "contrib" de var$contrib
print(var$contrib)
corrplot(var$contrib,is.corr=FALSE)

fviz_contrib(fitLimpio,choice="var",axes=1,top=10) 
fviz_contrib(fitLimpio,choice="var",axes=2,top=10)
fviz_contrib(fitLimpio,choice="var",axes=3,top=10) 


fviz_pca_ind(fit,axes = c(1, 2), col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) 

fviz_pca_ind(fit,axes = c(1, 3), col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) #se aprecia lo mismo que en el 1 y 2.

fviz_pca_ind(fit,axes = c(2, 3), col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) #las provincias 5 y 3 son destacadas.
ind <- fitLimpio["ind"]


#en estas tablas se aprecian por orden los sujetos mas representados por cada dimension y luego 
#obtenemos los nombres.

sort(ind$ind$cos2[,1], decreasing = T) 
sort(ind$ind$cos2[,2], decreasing = T) 
sort(ind$ind$cos2[,3], decreasing = T) 



### Tengo que construir un indice que sea una combinacion lineal de todas las variables.
### Si los componentes principales son una combinacion lineal normalizada de las variables,
### un analisis PCA en el que solo tengamos una componente se podria considerar una combinacion
### lineal de todas las variables de la que podríamos obtener un indice.
fitUnit       <- PCA(datos, scale.unit=T, ncp = 1, graph = T) #Analisis de componentes, ncp = 1
fitUnit["ind"]$ind$cos2 #para Madrid y para Melilla. 





#### CLUSTERS



#comenzamos normalizando las puntuaciones para medir las distancias.
datos_st <- scale(datos) #la funcion scale de ahora en adelante convendra mantenerla cerca
d_st <- dist(datos_st, method = "euclidean")
as.matrix(d_st)[1:6,1:6]


res.hc <- hclust(d_st, method = "ward.D2")
fviz_dend(res.hc, cex = 0.5) #A partir de este dendograma hemos de tomar decisiones para las agrupaciones


#Creemos que la decisión más conveniente son 6 grupos por ahora
grp <- cutree(res.hc, k = 6)
table(grp)


fviz_dend(res.hc, k = 6, # queremos 6 grupos
          cex = 0.5,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#33FFDD","#A833FF"),
          color_labels_by_k = TRUE, # colores
          rect = TRUE # Acuerdate de que estos son los rectangulos
)

k_colors_c <- c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#33FFDD","#A833FF", "#FF001A", "#FFDB76")

fviz_cluster(list(data = datos_st, cluster = grp),
             palette = k_colors_c,
             ellipse.type = "convex",
             repel = T,
             show.clust.cent = F,
             ggtheme = theme_minimal()
)
# A la vista de la representacion grafica queda claro que he escogido demasiados grupos y probamos
# a agruparlos en cuatro. 


grp <- cutree(res.hc, k = 4)
fviz_dend(res.hc,
          k = 4,
          k_colors = k_colors_c,
          color_labels_by_k = T,
          rect = T
)

#lo representamos graficamente.
fviz_cluster(list(data = datos_st, cluster = grp),
             palette = k_colors_c,
             ellipse.type = "convex",
             repel = T,
             show.clust.cent = F,
             ggtheme = theme_minimal()
)


#La representacion grafica sigue requiriendo de una mayor separacion
grp <- cutree(res.hc, k = 5)
fviz_dend(res.hc,
          k = 4,
          k_colors = k_colors_c,
          color_labels_by_k = T,
          rect = T
)

#lo representamos graficamente.
fviz_cluster(list(data = datos_st, cluster = grp),
             palette = k_colors_c,
             ellipse.type = "convex",
             repel = T,
             show.clust.cent = F,
             ggtheme = theme_minimal()
)







### HASTA AQUI TODO FUNCIONA. AHORA VAMOS A HACER PRUEBAS. 

#Ahora hemos de aplicar los criterios de Silhoutte y Elbow.

#Metodo Elbow incluida la grafica
fviz_nbclust(datos_st,
             kmeans,
             method = "wss") +
  geom_vline(xintercept = 6, # A la vista del criterio de Elbow, el numero optimo de grupos es 6
             linetype = 2) +
  labs(subtitle = "metodo Elbow")

#Metodo Silhoutte
fviz_nbclust(datos_st,
             kmeans,
             method = "silhouette") +
  labs(subtitle = "metodo Silhoutte") #Segun el metodo silhoutte el numero optimo
# de grupos son 2. Parecen muy pocos clusters y optaremos por la primera opcion



### AHORA EMPEZAMOS LAS PRUEBAS DE BONDAD DE LOS CLUSTERS


set.seed(123)
km.res <- kmeans(datos_st, 5)
head(km.res$cluster, 20)

fviz_cluster(km.res, datos) ### Para visualizarlo
