#-------------------------------------------------------------------------------
# Instalación de paquetes
#-------------------------------------------------------------------------------
# Escribir Excel
install.packages("writexl")
# Lector de Excel
install.packages("readxl")
# Gráficos
install.packages("ggplot2")
# Estadísticos descriptivos
install.packages("psych")
# Correlaciones
install.packages("corrplot")
# Tratamiento de data frame
install.packages("dplyr")
# Componentes principales
install.packages("factoextra") 
# Ajuste de distribuciones
install.packages("MASS") 
# Teoría de valores extremos
install.packages("evir")
# Extreme Value Mixture Modelling
install.packages("evmix")
# Extreme Risk Coefficient of Variation
install.packages("ercv")
# EnvStats
install.packages("EnvStats")
#-------------------------------------------------------------------------------
# Carga de paquetes
#-------------------------------------------------------------------------------
# Escribir Excel
library("writexl")
# Lector de Excel
library("readxl")
# Gráficos
library("ggplot2")
# Estadísticos descriptivos
library("psych")
# Correlaciones
library("corrplot")
# Tratamiento de data frame
library("dplyr")
# Componentes principales
library("factoextra") 
# Ajuste de distribuciones
library("MASS") 
# Teoría de valores extremos
library("evir")
# Extreme Value Mixture Modelling
library("evmix")
# Extreme Risk Coefficient of Variation
library("ercv")
# EnvStats
library("EnvStats")
#-------------------------------------------------------------------------------
# Importación de base de datos
#-------------------------------------------------------------------------------
# Lectura de la base de datos
datos<-read_excel("FC01_G03_BBDD.xlsx")
# Convertir en data frame
datos<-data.frame(datos)
# Variables numéricas
datos_numericos<-datos[, sapply(datos,is.numeric)]
# Encabezado
head(datos)
# Resumen
summary(datos)
# Coste daños corporales (en miles)
daños_corporales<-as.matrix(datos$InjuryAmount[datos$InjuryAmount>0]/1000)
#-------------------------------------------------------------------------------
# 3. Análisis metodológico escogido
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.1. Análisis descriptivo univariado y bivariado de la base de datos
#-------------------------------------------------------------------------------
# Tipo de variable
str(datos)
#-------------------------------------------------------------------------------
# 3.1.1 Univariado
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.1.1.1 Variables numéricas
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.1.1.1.1 ClaimNb
#-------------------------------------------------------------------------------
# Estadísticos
describe(datos$ClaimNb)
#
# Barplot
ClaimNb_barplot<-barplot(round(prop.table(table(datos$ClaimNb))*100,1),
              main = "ClaimNb",
              xlab = "Número de reclamos",
              ylab = "Frecuencia relativa (%)",
              col = "lightblue",
              ylim = c(0,120))
# Añadir etiquetas sobre cada barra
text(x = ClaimNb_barplot, 
     y = round(prop.table(table(datos$ClaimNb))*100,1)+5,
     labels = round(prop.table(table(datos$ClaimNb))*100,1))
#-------------------------------------------------------------------------------
# 3.1.1.1.2 Exposure
#-------------------------------------------------------------------------------
# Estadísticos
describe(datos$Exposure)
#
# k-means para agrupar (solo variable Exposure)
# Normalizar la variable
Exposure_scaled<-scale(datos$Exposure)
# Método del codo para determinar el número óptimo de clústers
set.seed(123)
Exposure_wss<-sapply(1:10, function(k){
  kmeans(Exposure_scaled,centers=k,nstart=10)$tot.withinss
})
# Gráfico del codo
plot(1:10, Exposure_wss, type = "b", pch = 19,
     xlab = "Número de clusters (k)",
     ylab = "Suma de cuadrados dentro del cluster",
     main = "Método del codo",
     col="lightblue")
# Añadir segunda línea al título
mtext("(Exposure)", side = 3, line = 0.5, cex = 1)
# Aplicación del método de k-means
set.seed(123)
Exposure_kmeans_result<-kmeans(Exposure_scaled,centers=4,nstart=25)
# Resultados en escala original
Exposure_media<-mean(datos$Exposure)
Exposure_desv<-sd(datos$Exposure)
Exposure_centroides_original<-Exposure_kmeans_result$centers*Exposure_desv+Exposure_media
# Añadir los clusters al data frame y analizar tamaños y rangos
datos$ExposureCluster<-Exposure_kmeans_result$cluster
# Mostrar tamaño de cada grupo
print(table(datos$ExposureCluster))
# Mostrar rango (mínimo y máximo) por cluster
ExposureCluster_rango<- aggregate(datos$Exposure, by = list(Cluster = datos$ExposureCluster), range)
print(ExposureCluster_rango)
#
# Barplot
# Calcular frecuencias relativas
ExposureCluster_freq_abs <- table(datos$ExposureCluster)
ExposureCluster_freq_rel <- prop.table(ExposureCluster_freq_abs) * 100
# Ordenar por el mínimo del rango
ExposureCluster_orden<- order(ExposureCluster_rango$x[,1])
ExposureCluster_rango<-ExposureCluster_rango[ExposureCluster_orden, ]
ExposureCluster_freq_rel<-ExposureCluster_freq_rel[ExposureCluster_orden]
# Crear etiquetas con los rangos
ExposureCluster_labels <- apply(round(ExposureCluster_rango$x,1), 1, function(r) {
  paste0("(", r[1], ", ", r[2], "]")
})
# Barplot ordenado
Exposure_barplot<-barplot(ExposureCluster_freq_rel,
                        names.arg = ExposureCluster_labels,
                        col = "lightblue",
                        ylim = c(0, 120),
                        main = "Clusters (Exposure)",
                        xlab = "Tiempo de vigencia y exposición al riesgo (años)",
                        ylab = "Frecuencia relativa (%)")
# Añadir segunda línea al título
mtext("k-means", side = 3, line = 0.5, cex = 1)
# Añadir etiquetas numéricas sobre cada barra
text(x = Exposure_barplot,
     y = ExposureCluster_freq_rel,
     labels = round(ExposureCluster_freq_rel, 1),
     pos = 3)
#-------------------------------------------------------------------------------
# 3.1.1.1.3 CarAge
#-------------------------------------------------------------------------------
# Estadísticos
describe(datos$CarAge)
#
# k-means para agrupar
# Normalizar la variable
CarAge_scaled<-scale(datos$CarAge)
# Método del codo para determinar el número óptimo de clústers
set.seed(123)
CarAge_wss<-sapply(1:10, function(k){
  kmeans(CarAge_scaled,centers=k,nstart=10)$tot.withinss
})
# Gráfico del codo
plot(1:10, CarAge_wss, type = "b", pch = 19,
     xlab = "Número de clusters (k)",
     ylab = "Suma de cuadrados dentro del cluster",
     main = "Método del codo",
     col="lightblue")
# Añadir segunda línea al título
mtext("(CarAge)", side = 3, line = 0.5, cex = 1)
# Aplicación del método de k-means
set.seed(123)
CarAge_kmeans_result<-kmeans(CarAge_scaled,centers=4,nstart=25)
# Resultados en escala original
CarAge_media<-mean(datos$CarAge)
CarAge_desv<-sd(datos$CarAge)
CarAge_centroides_original<-CarAge_kmeans_result$centers*CarAge_desv+CarAge_media
# Añadir los clusters al data frame y analizar tamaños y rangos
datos$CarAgeCluster<-CarAge_kmeans_result$cluster
# Mostrar tamaño de cada grupo
print(table(datos$CarAgeCluster))
# Mostrar rango (mínimo y máximo) por cluster
CarAgeCluster_rango<- aggregate(datos$CarAge, by = list(Cluster = datos$CarAgeCluster), range)
print(CarAgeCluster_rango)
#
# Barplot
# Calcular frecuencias relativas
CarAgeCluster_freq_abs <- table(datos$CarAgeCluster)
CarAgeCluster_freq_rel <- prop.table(CarAgeCluster_freq_abs) * 100
# Ordenar por el mínimo del rango
CarAgeCluster_orden<- order(CarAgeCluster_rango$x[,1])
CarAgeCluster_rango<-CarAgeCluster_rango[CarAgeCluster_orden, ]
CarAgeCluster_freq_rel<-CarAgeCluster_freq_rel[CarAgeCluster_orden]
# Crear etiquetas con los rangos
CarAgeCluster_labels <- apply(CarAgeCluster_rango$x, 1, function(r) {
  paste0("(", r[1], ", ", r[2], "]")
})
# Barplot ordenado
CarAge_barplot<-barplot(CarAgeCluster_freq_rel,
                          names.arg = CarAgeCluster_labels,
                          col = "lightblue",
                          ylim = c(0, 120),
                          main = "Clusters (CarAge)",
                          xlab = "Antigüedad del vehículo (años)",
                          ylab = "Frecuencia relativa (%)")
# Añadir segunda línea al título
mtext("k-means", side = 3, line = 0.5, cex = 1)
# Añadir etiquetas numéricas sobre cada barra
text(x = CarAge_barplot,
     y = CarAgeCluster_freq_rel,
     labels = round(CarAgeCluster_freq_rel, 1),
     pos = 3)
#-------------------------------------------------------------------------------
# 3.1.1.1.4 DriverAge
#-------------------------------------------------------------------------------
# Estadísticos
describe(datos$DriverAge)
#
# k-means para agrupar
# Normalizar la variable
DriverAge_scaled<-scale(datos$DriverAge)
# Método del codo para determinar el número óptimo de clústers
set.seed(123)
DriverAge_wss<-sapply(1:10, function(k){
  kmeans(DriverAge_scaled,centers=k,nstart=10)$tot.withinss
})
# Gráfico del codo
plot(1:10, DriverAge_wss, type = "b", pch = 19,
     xlab = "Número de clusters (k)",
     ylab = "Suma de cuadrados dentro del cluster",
     main = "Método del codo",
     col="lightblue")
# Añadir segunda línea al título
mtext("(DriverAge)", side = 3, line = 0.5, cex = 1)
# Aplicación del método de k-means
set.seed(123)
DriverAge_kmeans_result<-kmeans(DriverAge_scaled,centers=4,nstart=25)
# Resultados en escala original
DriverAge_media<-mean(datos$DriverAge)
DriverAge_desv<-sd(datos$DriverAge)
DriverAge_centroides_original<-DriverAge_kmeans_result$centers*DriverAge_desv+DriverAge_media
# Añadir los clusters al data frame y analizar tamaños y rangos
datos$DriverAgeCluster<-DriverAge_kmeans_result$cluster
# Mostrar tamaño de cada grupo
print(table(datos$DriverAgeCluster))
# Mostrar rango (mínimo y máximo) por cluster
DriverAgeCluster_rango<- aggregate(datos$DriverAge, by = list(Cluster = datos$DriverAgeCluster), range)
print(DriverAgeCluster_rango)
#
# Barplot
# Calcular frecuencias relativas
DriverAgeCluster_freq_abs <- table(datos$DriverAgeCluster)
DriverAgeCluster_freq_rel <- prop.table(DriverAgeCluster_freq_abs) * 100
# Ordenar por el mínimo del rango
DriverAgeCluster_orden<- order(DriverAgeCluster_rango$x[,1])
DriverAgeCluster_rango<-DriverAgeCluster_rango[DriverAgeCluster_orden, ]
DriverAgeCluster_freq_rel<-DriverAgeCluster_freq_rel[DriverAgeCluster_orden]
# Crear etiquetas con los rangos
DriverAgeCluster_labels <- apply(DriverAgeCluster_rango$x, 1, function(r) {
  paste0("(", r[1], ", ", r[2], "]")
})
# Barplot ordenado
DriverAge_barplot<-barplot(DriverAgeCluster_freq_rel,
        names.arg = DriverAgeCluster_labels,
        col = "lightblue",
        ylim = c(0, 120),
        main = "Clusters (DriverAge)",
        xlab = "Edad del conductor (años)",
        ylab = "Frecuencia relativa (%)")
# Añadir segunda línea al título
mtext("k-means", side = 3, line = 0.5, cex = 1)
# Añadir etiquetas numéricas sobre cada barra
text(x = DriverAge_barplot,
     y = DriverAgeCluster_freq_rel,
     labels = round(DriverAgeCluster_freq_rel, 1),
     pos = 3)
#-------------------------------------------------------------------------------
# 3.1.1.1.5 Density
#-------------------------------------------------------------------------------
# Estadísticos
describe(datos$Density)
#
# k-means para agrupar
# Normalizar la variable
Density_scaled<-scale(datos$Density)
# Método del codo para determinar el número óptimo de clústers
set.seed(123)
Density_wss<-sapply(1:10, function(k){
  kmeans(Density_scaled,centers=k,nstart=10)$tot.withinss
})
# Gráfico del codo
plot(1:10, Density_wss, type = "b", pch = 19,
     xlab = "Número de clusters (k)",
     ylab = "Suma de cuadrados dentro del cluster",
     main = "Método del codo",
     col="lightblue")
# Añadir segunda línea al título
mtext("(Density)", side = 3, line = 0.5, cex = 1)
# Aplicación del método de k-means
set.seed(123)
Density_kmeans_result<-kmeans(Density_scaled,centers=4,nstart=25)
# Resultados en escala original
Density_media<-mean(datos$Density)
Density_desv<-sd(datos$Density)
Density_centroides_original<-Density_kmeans_result$centers*Density_desv+Density_media
# Añadir los clusters al data frame y analizar tamaños y rangos
datos$DensityCluster<-Density_kmeans_result$cluster
# Mostrar tamaño de cada grupo
print(table(datos$DensityCluster))
# Mostrar rango (mínimo y máximo) por cluster
DensityCluster_rango<- aggregate(datos$Density, by = list(Cluster = datos$DensityCluster), range)
print(DensityCluster_rango)
#
# Barplot
# Calcular frecuencias relativas
DensityCluster_freq_abs <- table(datos$DensityCluster)
DensityCluster_freq_rel <- prop.table(DensityCluster_freq_abs) * 100
# Ordenar por el mínimo del rango
DensityCluster_orden<- order(DensityCluster_rango$x[,1])
DensityCluster_rango<-DensityCluster_rango[DensityCluster_orden, ]
DensityCluster_freq_rel<-DensityCluster_freq_rel[DensityCluster_orden]
# Crear etiquetas con los rangos
DensityCluster_labels <- apply(DensityCluster_rango$x, 1, function(r) {
  paste0("(", r[1], ", ", r[2], "]")
})
# Barplot ordenado
Density_barplot<-barplot(DensityCluster_freq_rel,
                           names.arg = DensityCluster_labels,
                           col = "lightblue",
                           ylim = c(0, 120),
                           main = "Clusters (Density)",
                           xlab = "Número de habitantes en la ciudad del conductor (km2)",
                           ylab = "Frecuencia relativa (%)")
# Añadir segunda línea al título
mtext("k-means", side = 3, line = 0.5, cex = 1)
# Añadir etiquetas numéricas sobre cada barra
text(x = Density_barplot,
     y = DensityCluster_freq_rel,
     labels = round(DensityCluster_freq_rel, 1),
     pos = 3)
#-------------------------------------------------------------------------------
# 3.1.1.1.6 ClaimAmount
#-------------------------------------------------------------------------------
# Estadísticos
describe(datos$ClaimAmount)
#
# Barplot
# Vamos a centrarnos en el ClaimAmount que excede el 0. Habíamos visto que del 
# total de pólizas, el 96% no habían registrado ningún siniestro, por lo que el
# gráfico saldria al 96% en cero y no se podría ver a detalle lo que pasa más allá.
# ClaimAmount > 0
ClaimAmount_positivos<-datos$ClaimAmount[datos$ClaimAmount > 0]
# ClaimAmount > 0 y ordenados en orden ascendente
ClaimAmount_positivos_ordenados<-sort(ClaimAmount_positivos)
# Calcular el punto de corte para dividir en dos ClaimAmount > 0 (G1:99%, G2:1%)
ClaimAmount_puntocorte<-length(ClaimAmount_positivos_ordenados)
ClaimAmount_puntocorte<-floor(ClaimAmount_puntocorte*0.99)
# ClaimAmount > 0: Grupo 1
ClaimAmount_positivos_G1<-ClaimAmount_positivos_ordenados[1:ClaimAmount_puntocorte]/1000
head(ClaimAmount_positivos_G1)
tail(ClaimAmount_positivos_G1)
# ClaimAmount > 0: Grupo 2
ClaimAmount_positivos_G2<-ClaimAmount_positivos_ordenados[(ClaimAmount_puntocorte+1):length(ClaimAmount_positivos_ordenados)]/1000
head(ClaimAmount_positivos_G2)
tail(ClaimAmount_positivos_G2)
# Validación (debe ser 0)
length(ClaimAmount_positivos)-length(ClaimAmount_positivos_G1)-length(ClaimAmount_positivos_G2)
# G1: 
# Crear histograma sin graficar
ClaimAmount_positivos_G1_h<-hist(ClaimAmount_positivos_G1,
          breaks = 5,  # Ajusta el número de intervalos
          plot = FALSE)
# Calcular frecuencias relativas en porcentaje
ClaimAmount_positivos_G1_frecuencias_relativas<-(ClaimAmount_positivos_G1_h$counts/sum(ClaimAmount_positivos_G1_h$counts)) * 100
# Crear barplot con formato personalizado
ClaimAmount_positivos_G1_barplot<-barplot(ClaimAmount_positivos_G1_frecuencias_relativas,
              names.arg = paste0("(", round(ClaimAmount_positivos_G1_h$breaks[-length(ClaimAmount_positivos_G1_h$breaks)], 1), ", ", round(ClaimAmount_positivos_G1_h$breaks[-1], 1), "]"),
              col = "lightblue",
              ylim = c(0, 120),
              main = "ClaimAmount > 0",
              xlab = "Costo total del reclamo (miles euros)",
              ylab = "Frecuencia relativa (%)")
# Añadir segunda línea al título
mtext("99% de los datos", side = 3, line = 0.5, cex = 1)
# Añadir etiquetas centradas sobre las barras
text(x = ClaimAmount_positivos_G1_barplot,
     y = ClaimAmount_positivos_G1_frecuencias_relativas,
     labels = round(ClaimAmount_positivos_G1_frecuencias_relativas, 1),
     pos = 3, cex = 0.9)
# Promedio
mean(ClaimAmount_positivos_G1)*1000
# G2:
# Crear histograma sin graficar
ClaimAmount_positivos_G2_h<-hist(ClaimAmount_positivos_G2,
                                 breaks = 5,  # Ajusta el número de intervalos 
                                 plot = FALSE)
# Calcular frecuencias relativas en porcentaje
ClaimAmount_positivos_G2_frecuencias_relativas<-(ClaimAmount_positivos_G2_h$counts/sum(ClaimAmount_positivos_G2_h$counts)) * 100
# Generar etiquetas originales
ClaimAmount_positivos_G2_labels_originales <- paste0("(", round(ClaimAmount_positivos_G2_h$breaks[-length(ClaimAmount_positivos_G2_h$breaks)], 1), ", ",
                            round(ClaimAmount_positivos_G2_h$breaks[-1], 1), "]")
# Modificar solo el primer label: Para que no muestre desde 0, sino desde el valor mínimo
ClaimAmount_positivos_G2_labels_originales[1] <- paste0("(", round(min(ClaimAmount_positivos_G2), 1), ", ", round(ClaimAmount_positivos_G2_h$breaks[2], 1), "]")
# Crear barplot con formato personalizado
ClaimAmount_positivos_G2_barplot<-barplot(ClaimAmount_positivos_G2_frecuencias_relativas,
                                          names.arg = ClaimAmount_positivos_G2_labels_originales,
                                          col = "lightblue",
                                          ylim = c(0, 120),
                                          main = "ClaimAmount > 0",
                                          xlab = "Costo total del reclamo (miles euros)",
                                          ylab = "Frecuencia relativa (%)")
# Añadir segunda línea al título
mtext("1% de los datos", side = 3, line = 0.5, cex = 1)
# Añadir etiquetas centradas sobre las barras
text(x = ClaimAmount_positivos_G2_barplot,
     y = ClaimAmount_positivos_G2_frecuencias_relativas,
     labels = round(ClaimAmount_positivos_G2_frecuencias_relativas, 1),
     pos = 3, cex = 0.9)
# Promedio
mean(ClaimAmount_positivos_G2[ClaimAmount_positivos_G2<=5000])*1000
#-------------------------------------------------------------------------------
# 3.1.1.1.7 InjuryAmount
#-------------------------------------------------------------------------------
# Estadísticos
describe(datos$InjuryAmount)
#
# Barplot
# Vamos a centrarnos en el InjuryAmount que excede el 0. Habíamos visto que del 
# total de pólizas, el 96% no habían registrado ningún siniestro, por lo que el
# gráfico saldria al 96% en cero y no se podría ver a detalle lo que pasa más allá.
# InjuryAmount > 0
InjuryAmount_positivos<-datos$InjuryAmount[datos$InjuryAmount > 0]
length(InjuryAmount_positivos)/length(datos$InjuryAmount)
# InjuryAmount > 0 y ordenados en orden ascendente
InjuryAmount_positivos_ordenados<-sort(InjuryAmount_positivos)
# Calcular el punto de corte para dividir en dos InjuryAmount > 0 (G1:99%, G2:1%)
InjuryAmount_puntocorte<-length(InjuryAmount_positivos_ordenados)
InjuryAmount_puntocorte<-floor(InjuryAmount_puntocorte*0.99)
# InjuryAmount > 0: Grupo 1
InjuryAmount_positivos_G1<-InjuryAmount_positivos_ordenados[1:InjuryAmount_puntocorte]/1000
head(InjuryAmount_positivos_G1)
tail(InjuryAmount_positivos_G1)
# InjuryAmount > 0: Grupo 2
InjuryAmount_positivos_G2<-InjuryAmount_positivos_ordenados[(InjuryAmount_puntocorte+1):length(InjuryAmount_positivos_ordenados)]/1000
head(InjuryAmount_positivos_G2)
tail(InjuryAmount_positivos_G2)
# Validación (debe ser 0)
length(InjuryAmount_positivos)-length(InjuryAmount_positivos_G1)-length(InjuryAmount_positivos_G2)
# G1: 
# Crear histograma sin graficar
InjuryAmount_positivos_G1_h<-hist(InjuryAmount_positivos_G1,
                                 breaks = 5,  # Ajusta el número de intervalos
                                 plot = FALSE)
# Calcular frecuencias relativas en porcentaje
InjuryAmount_positivos_G1_frecuencias_relativas<-(InjuryAmount_positivos_G1_h$counts/sum(InjuryAmount_positivos_G1_h$counts)) * 100
# Crear barplot con formato personalizado
InjuryAmount_positivos_G1_barplot<-barplot(InjuryAmount_positivos_G1_frecuencias_relativas,
                                          names.arg = paste0("(", round(InjuryAmount_positivos_G1_h$breaks[-length(InjuryAmount_positivos_G1_h$breaks)], 1), ", ", round(InjuryAmount_positivos_G1_h$breaks[-1], 1), "]"),
                                          col = "lightblue",
                                          ylim = c(0, 120),
                                          main = "InjuryAmount > 0",
                                          xlab = "Costo total del reclamo (miles euros)",
                                          ylab = "Frecuencia relativa (%)")
# Añadir segunda línea al título
mtext("99% de los datos", side = 3, line = 0.5, cex = 1)
# Añadir etiquetas centradas sobre las barras
text(x = InjuryAmount_positivos_G1_barplot,
     y = InjuryAmount_positivos_G1_frecuencias_relativas,
     labels = round(InjuryAmount_positivos_G1_frecuencias_relativas, 1),
     pos = 3, cex = 0.9)
# Promedio
mean(InjuryAmount_positivos_G1)*1000
# G2:
# Crear histograma sin graficar
InjuryAmount_positivos_G2_h<-hist(InjuryAmount_positivos_G2,
                                 breaks = 5,  # Ajusta el número de intervalos 
                                 plot = FALSE)
# Calcular frecuencias relativas en porcentaje
InjuryAmount_positivos_G2_frecuencias_relativas<-(InjuryAmount_positivos_G2_h$counts/sum(InjuryAmount_positivos_G2_h$counts)) * 100
# Generar etiquetas originales
InjuryAmount_positivos_G2_labels_originales <- paste0("(", round(InjuryAmount_positivos_G2_h$breaks[-length(InjuryAmount_positivos_G2_h$breaks)], 1), ", ",
                                                     round(InjuryAmount_positivos_G2_h$breaks[-1], 1), "]")
# Modificar solo el primer label: Para que no muestre desde 0, sino desde el valor mínimo
InjuryAmount_positivos_G2_labels_originales[1] <- paste0("(", round(min(InjuryAmount_positivos_G2), 1), ", ", round(InjuryAmount_positivos_G2_h$breaks[2], 1), "]")
# Crear barplot con formato personalizado
InjuryAmount_positivos_G2_barplot<-barplot(InjuryAmount_positivos_G2_frecuencias_relativas,
                                          names.arg = InjuryAmount_positivos_G2_labels_originales,
                                          col = "lightblue",
                                          ylim = c(0, 120),
                                          main = "InjuryAmount > 0",
                                          xlab = "Costo daños corporales (miles euros)",
                                          ylab = "Frecuencia relativa (%)")
# Añadir segunda línea al título
mtext("1% de los datos", side = 3, line = 0.5, cex = 1)
# Añadir etiquetas centradas sobre las barras
text(x = InjuryAmount_positivos_G2_barplot,
     y = InjuryAmount_positivos_G2_frecuencias_relativas,
     labels = round(InjuryAmount_positivos_G2_frecuencias_relativas, 1),
     pos = 3, cex = 0.9)
# Promedio
mean(InjuryAmount_positivos_G2[InjuryAmount_positivos_G2<=5000])*1000
#-------------------------------------------------------------------------------
# 3.1.1.1.7 PropertyAmount
#-------------------------------------------------------------------------------
# Estadísticos
describe(datos$PropertyAmount)
#
# Barplot
# Vamos a centrarnos en el PropertyAmount que excede el 0. Habíamos visto que del 
# total de pólizas, el 96% no habían registrado ningún siniestro, por lo que el
# gráfico saldria al 96% en cero y no se podría ver a detalle lo que pasa más allá.
# PropertyAmount > 0
PropertyAmount_positivos<-datos$PropertyAmount[datos$PropertyAmount > 0]
length(PropertyAmount_positivos)/length(datos$PropertyAmount)
# PropertyAmount > 0 y ordenados en orden ascendente
PropertyAmount_positivos_ordenados<-sort(PropertyAmount_positivos)
# Calcular el punto de corte para dividir en dos PropertyAmount > 0 (G1:99%, G2:1%)
PropertyAmount_puntocorte<-length(PropertyAmount_positivos_ordenados)
PropertyAmount_puntocorte<-floor(PropertyAmount_puntocorte*0.99)
# PropertyAmount > 0: Grupo 1
PropertyAmount_positivos_G1<-PropertyAmount_positivos_ordenados[1:PropertyAmount_puntocorte]/1000
head(PropertyAmount_positivos_G1)
tail(PropertyAmount_positivos_G1)
# PropertyAmount > 0: Grupo 2
PropertyAmount_positivos_G2<-PropertyAmount_positivos_ordenados[(PropertyAmount_puntocorte+1):length(PropertyAmount_positivos_ordenados)]/1000
head(PropertyAmount_positivos_G2)
tail(PropertyAmount_positivos_G2)
# Validación (debe ser 0)
length(PropertyAmount_positivos)-length(PropertyAmount_positivos_G1)-length(PropertyAmount_positivos_G2)
# G1: 
# Crear histograma sin graficar
PropertyAmount_positivos_G1_h<-hist(PropertyAmount_positivos_G1,
                                  breaks = 5,  # Ajusta el número de intervalos
                                  plot = FALSE)
# Calcular frecuencias relativas en porcentaje
PropertyAmount_positivos_G1_frecuencias_relativas<-(PropertyAmount_positivos_G1_h$counts/sum(PropertyAmount_positivos_G1_h$counts)) * 100
# Crear barplot con formato personalizado
PropertyAmount_positivos_G1_barplot<-barplot(PropertyAmount_positivos_G1_frecuencias_relativas,
                                           names.arg = paste0("(", round(PropertyAmount_positivos_G1_h$breaks[-length(PropertyAmount_positivos_G1_h$breaks)], 1), ", ", round(PropertyAmount_positivos_G1_h$breaks[-1], 1), "]"),
                                           col = "lightblue",
                                           ylim = c(0, 120),
                                           main = "PropertyAmount > 0",
                                           xlab = "Costo total del reclamo (miles euros)",
                                           ylab = "Frecuencia relativa (%)")
# Añadir segunda línea al título
mtext("99% de los datos", side = 3, line = 0.5, cex = 1)
# Añadir etiquetas centradas sobre las barras
text(x = PropertyAmount_positivos_G1_barplot,
     y = PropertyAmount_positivos_G1_frecuencias_relativas,
     labels = round(PropertyAmount_positivos_G1_frecuencias_relativas, 1),
     pos = 3, cex = 0.9)
# Promedio
mean(PropertyAmount_positivos_G1)*1000
# G2:
# Crear histograma sin graficar
PropertyAmount_positivos_G2_h<-hist(PropertyAmount_positivos_G2,
                                  breaks = 5,  # Ajusta el número de intervalos 
                                  plot = FALSE)
# Calcular frecuencias relativas en porcentaje
PropertyAmount_positivos_G2_frecuencias_relativas<-(PropertyAmount_positivos_G2_h$counts/sum(PropertyAmount_positivos_G2_h$counts)) * 100
# Generar etiquetas originales
PropertyAmount_positivos_G2_labels_originales <- paste0("(", round(PropertyAmount_positivos_G2_h$breaks[-length(PropertyAmount_positivos_G2_h$breaks)], 1), ", ",
                                                      round(PropertyAmount_positivos_G2_h$breaks[-1], 1), "]")
# Modificar solo el primer label: Para que no muestre desde 0, sino desde el valor mínimo
PropertyAmount_positivos_G2_labels_originales[1] <- paste0("(", round(min(PropertyAmount_positivos_G2), 1), ", ", round(PropertyAmount_positivos_G2_h$breaks[2], 1), "]")
# Crear barplot con formato personalizado
PropertyAmount_positivos_G2_barplot<-barplot(PropertyAmount_positivos_G2_frecuencias_relativas,
                                           names.arg = PropertyAmount_positivos_G2_labels_originales,
                                           col = "lightblue",
                                           ylim = c(0, 120),
                                           main = "PropertyAmount > 0",
                                           xlab = "Costo daños materiales (miles euros)",
                                           ylab = "Frecuencia relativa (%)")
# Añadir segunda línea al título
mtext("1% de los datos", side = 3, line = 0.5, cex = 1)
# Añadir etiquetas centradas sobre las barras
text(x = PropertyAmount_positivos_G2_barplot,
     y = PropertyAmount_positivos_G2_frecuencias_relativas,
     labels = round(PropertyAmount_positivos_G2_frecuencias_relativas, 1),
     pos = 3, cex = 0.9)
# Promedio
mean(PropertyAmount_positivos_G2[PropertyAmount_positivos_G2<=100000])*1000
#-------------------------------------------------------------------------------
# 3.1.1.2 Variables categóricas
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.1.1.2.1 Power
#-------------------------------------------------------------------------------
# Estadísticos
# Tabla
Power_freq_abs<-table(datos$Power)
# Número de categorías
Power_categorias<-length(Power_freq_abs)
# Moda
Power_moda<-names(Power_freq_abs)[which.max(Power_freq_abs)]  
#
# Pie 
# Frecuencias absolutas ordenadas
Power_freq_abs_ord<-sort(Power_freq_abs, decreasing = TRUE)
# Frecuencias relativas ordenadas
Power_freq_rel<-100*Power_freq_abs_ord/sum(Power_freq_abs_ord)
# Frecuencias relativas acumuladas ordenadas      
Power_freq_rel_cum<-cumsum(Power_freq_rel)
# Frecuencias a no incluir en "Otros"
Power_freq_rel_top<-which(Power_freq_rel_cum<= 90)
# Frecuencias a incluir en "Otros"
Power_freq_rel_otros<-sum(Power_freq_abs_ord[-Power_freq_rel_top])
# Vector final
Power_freq_rel_final<-c(Power_freq_abs_ord[Power_freq_rel_top], Otros=Power_freq_rel_otros)
# Calcular porcentajes
Power_porcentajes<-round(100 *Power_freq_rel_final/sum(Power_freq_rel_final), 1)
# Crear etiquetas con nombre + porcentaje
Power_etiquetas<-paste(names(Power_freq_rel_final),"-",Power_porcentajes, "%")
# Gráfico
pie(Power_freq_rel_final,
    main = "Power",
    init.angle = 90,
    col = colorRampPalette(c("#C6DBEF","#4292C6","#08306B"))(length(Power_freq_rel_final)),
    labels=Power_etiquetas)
# Añadir segunda línea al título
mtext("(d:menos potente a o:más potente); ", side = 3, line = 0.5, cex = 1)
#
# Cluster
# Categorizar Power
Power_categorias<-letters[4:15]  # d hasta o
# Agregar la columna PowerNum al data frame
datos$PowerNum<-match(datos$Power,Power_categorias)
# k-means para agrupar (solo variable Power)
# Normalizar la variable
PowerNum_scaled<-scale(datos$PowerNum)
# Método del codo para determinar el número óptimo de clústers
set.seed(123)
PowerNum_wss<-sapply(1:10, function(k){
  kmeans(PowerNum_scaled,centers=k,nstart=10)$tot.withinss
})
# Gráfico del codo
plot(1:10, PowerNum_wss, type = "b", pch = 19,
     xlab = "Número de clusters (k)",
     ylab = "Suma de cuadrados dentro del cluster",
     main = "Método del codo",
     col="lightblue")
# Añadir segunda línea al título
mtext("(Power)", side = 3, line = 0.5, cex = 1)
# Aplicación del método de k-means
set.seed(123)
PowerNum_kmeans_result<-kmeans(PowerNum_scaled,centers=4,nstart=25)
# Resultados en escala original
PowerNum_media<-mean(datos$PowerNum)
PowerNum_desv<-sd(datos$PowerNum)
PowerNum_centroides_original<-PowerNum_kmeans_result$centers*PowerNum_desv+PowerNum_media
# Añadir los clusters al data frame y analizar tamaños y rangos
datos$PowerNumCluster<-PowerNum_kmeans_result$cluster
# Mostrar tamaño de cada grupo
print(table(datos$PowerNumCluster))
# Mostrar rango (mínimo y máximo) por cluster
PowerNumCluster_rango<- aggregate(datos$PowerNum, by = list(Cluster = datos$PowerNumCluster), range)
print(PowerNumCluster_rango)
# Reemplazar números de cluster por letras
datos<-datos %>%
  mutate(PowerNumCluster = recode(PowerNumCluster,
                                   `1` = "d-e",
                                   `2` = "i-k",
                                   `3` = "l-o",
                                   `4` = "f-h"))
#-------------------------------------------------------------------------------
# 3.1.1.2.2 Brand
#-------------------------------------------------------------------------------
# Estadísticos
# Tabla
Brand_freq_abs<-table(datos$Brand)
# Número de categorías
Brand_categorias<-length(Brand_freq_abs)
# Moda
Brand_moda<-names(Brand_freq_abs)[which.max(Brand_freq_abs)]  
#
# Pie 
# Frecuencias absolutas ordenadas
Brand_freq_abs_ord<-sort(Brand_freq_abs, decreasing = TRUE)
# Frecuencias relativas ordenadas
Brand_freq_rel<-100*Brand_freq_abs_ord/sum(Brand_freq_abs_ord)
# Frecuencias relativas acumuladas ordenadas      
Brand_freq_rel_cum<-cumsum(Brand_freq_rel)
# Frecuencias a no incluir en "Otros"
Brand_freq_rel_top<-which(Brand_freq_rel_cum<= 90)
# Frecuencias a incluir en "Otros"
Brand_freq_rel_otros<-sum(Brand_freq_abs_ord[-Brand_freq_rel_top])
# Vector final
Brand_freq_rel_final<-c(Brand_freq_abs_ord[Brand_freq_rel_top], Otros=Brand_freq_rel_otros)
# Calcular porcentajes
Brand_porcentajes<-round(100 *Brand_freq_rel_final/sum(Brand_freq_rel_final), 1)
# Crear etiquetas con nombre + porcentaje
Brand_etiquetas<-paste(names(Brand_freq_rel_final),"-",Brand_porcentajes, "%")
# Gráfico
pie(Brand_freq_rel_final,
    main = "Brand",
    init.angle = 90,
    col = colorRampPalette(c("#C6DBEF","#4292C6","#08306B"))(length(Brand_freq_rel_final)),
    labels=Brand_etiquetas)
#
# Cluster
datos<-datos %>%
  mutate(BrandCluster = case_when(
    Brand %in% c("Fiat","Mercedes, Chrysler or BMW","Opel, General Motors or Ford","Renault, Nissan or Citroen","Volkswagen, Audi, Skoda or Seat","other") ~ "a",
    Brand %in% c("Japanese (except Nissan) or Korean") ~ "b",
    TRUE ~ "Otros"
  ))
#-------------------------------------------------------------------------------
# 3.1.1.2.3 Gas
#-------------------------------------------------------------------------------
# Estadísticos
# Tabla
Gas_freq_abs<-table(datos$Gas)
# Número de categorías
Gas_categorias<-length(Gas_freq_abs)
# Moda
Gas_moda<-names(Gas_freq_abs)[which.max(Gas_freq_abs)]  
#
# Pie 
# Frecuencias absolutas ordenadas
Gas_freq_abs_ord<-sort(Gas_freq_abs, decreasing = TRUE)
# Frecuencias relativas ordenadas
Gas_freq_rel<-100*Gas_freq_abs_ord/sum(Gas_freq_abs_ord)
# Frecuencias relativas acumuladas ordenadas      
Gas_freq_rel_cum<-cumsum(Gas_freq_rel)
# Frecuencias a no incluir en "Otros"
Gas_freq_rel_top<-which(Gas_freq_rel_cum<= 100)
# Frecuencias a incluir en "Otros"
Gas_freq_rel_otros<-sum(Gas_freq_abs_ord[-Gas_freq_rel_top])
# Vector final
Gas_freq_rel_final<-c(Gas_freq_abs_ord[Gas_freq_rel_top], Otros=Gas_freq_rel_otros)
# Calcular porcentajes
Gas_porcentajes<-round(100 *Gas_freq_rel_final/sum(Gas_freq_rel_final), 1)
# Crear etiquetas con nombre + porcentaje
Gas_etiquetas<-paste(names(Gas_freq_rel_final),"-",Gas_porcentajes, "%")
# Gráfico
pie(Gas_freq_rel_final,
    main = "Gas",
    init.angle = 90,
    col = colorRampPalette(c("#C6DBEF","#4292C6","#08306B"))(length(Gas_freq_rel_final)),
    labels=Gas_etiquetas)
#-------------------------------------------------------------------------------
# 3.1.1.2.4 Region
#-------------------------------------------------------------------------------
# Estadísticos
# Tabla
Region_freq_abs<-table(datos$Region)
# Número de categorías
Region_categorias<-length(Region_freq_abs)
# Moda
Region_moda<-names(Region_freq_abs)[which.max(Region_freq_abs)]  
#
# Pie 
# Frecuencias absolutas ordenadas
Region_freq_abs_ord<-sort(Region_freq_abs, decreasing = TRUE)
# Frecuencias relativas ordenadas
Region_freq_rel<-100*Region_freq_abs_ord/sum(Region_freq_abs_ord)
# Frecuencias relativas acumuladas ordenadas      
Region_freq_rel_cum<-cumsum(Region_freq_rel)
# Frecuencias a no incluir en "Otros"
Region_freq_rel_top<-which(Region_freq_rel_cum<= 90)
# Frecuencias a incluir en "Otros"
Region_freq_rel_otros<-sum(Region_freq_abs_ord[-Region_freq_rel_top])
# Vector final
Region_freq_rel_final<-c(Region_freq_abs_ord[Region_freq_rel_top], Otros=Region_freq_rel_otros)
# Calcular porcentajes
Region_porcentajes<-round(100 *Region_freq_rel_final/sum(Region_freq_rel_final), 1)
# Crear etiquetas con nombre + porcentaje
Region_etiquetas<-paste(names(Region_freq_rel_final),"-",Region_porcentajes, "%")
# Gráfico
pie(Region_freq_rel_final,
    main = "Region",
    init.angle = 90,
    col = colorRampPalette(c("#C6DBEF","#4292C6","#08306B"))(length(Region_freq_rel_final)),
    labels=Region_etiquetas)
# Cluster
datos<-datos %>%
  mutate(RegionCluster = case_when(
    Region %in% c("Aquitaine", "Ile-de-France", "Limousin","Nord-Pas-de-Calais","Poitou-Charentes") ~ "a",
    Region %in% c("Basse-Normandie", "Bretagne", "Centre","Haute-Normandie","Pays-de-la-Loire") ~ "b",
    TRUE ~ "Otros"
  ))
# Escribir en Excel
write_xlsx(datos, "FC01_G03_BBDD_v02.xlsx")
#-------------------------------------------------------------------------------
# 3.1.2 Multivariado
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.1.2.1 Variables numéricas
#-------------------------------------------------------------------------------
# Correlación Pearson
datos_numericos_matriz_cor<-cor(datos_numericos,use = "complete.obs",method = "pearson")
round(datos_numericos_matriz_cor,4)
# Gráfico
corrplot(datos_numericos_matriz_cor,
         method = "circle",      # círculos
         type = "lower",         # solo la parte inferior
         tl.col = "black",         # color de las etiquetas
         tl.cex = 0.8,           # tamaño de las etiquetas
         col = colorRampPalette(c("red", "grey90", "lightblue"))(200)) # paleta de colores
#-------------------------------------------------------------------------------
# 3.1.2.2 Variables categóricas
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.1.2.2.1 Power y ClaimNb
#-------------------------------------------------------------------------------
# Descriptivos
table(datos$Power)
# Power "d", "e" y "f"
ggplot(data = subset(datos, Power %in% c("d", "e", "f")),
       aes(x = Power, y = ClaimNb)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,4)+
  theme_minimal() +
  labs(title = "",
       x = "Power", y = "ClaimNb")
# Power "g", "h" y "i"
ggplot(data = subset(datos, Power %in% c("g", "h", "i")),
       aes(x = Power, y = ClaimNb)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,4)+
  theme_minimal() +
  labs(title = "",
       x = "Power", y = "ClaimNb")
# Power "j", "k" y "l"
ggplot(data = subset(datos, Power %in% c("j", "k", "l")),
       aes(x = Power, y = ClaimNb)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,4)+
  theme_minimal() +
  labs(title = "",
       x = "Power", y = "ClaimNb")
# Power "m", "n" y "o"
ggplot(data = subset(datos, Power %in% c("m", "n", "o")),
       aes(x = Power, y = ClaimNb)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,4)+
  theme_minimal() +
  labs(title = "",
       x = "Power", y = "ClaimNb")
#-------------------------------------------------------------------------------
# 3.1.2.2.2 Power y InjuryAmount
#-------------------------------------------------------------------------------
# Descriptivos
table(datos$Power)
# Power "d", "e" y "f"
ggplot(data = subset(datos, Power %in% c("d", "e", "f")),
       aes(x = Power, y = InjuryAmount/1000)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,20000)+
  theme_minimal() +
  labs(title = "",
       x = "Power", y = "InjuryAmount (Miles euros)")
# Power "g", "h" y "i"
ggplot(data = subset(datos, Power %in% c("g", "h", "i")),
       aes(x = Power, y = InjuryAmount/1000)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,20000)+
  theme_minimal() +
  labs(title = "",
       x = "Power", y = "InjuryAmount (Miles euros)")
# Power "j", "k" y "l"
ggplot(data = subset(datos, Power %in% c("j", "k", "l")),
       aes(x = Power, y = InjuryAmount/1000)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,20000)+
  theme_minimal() +
  labs(title = "",
       x = "Power", y = "InjuryAmount (Miles euros)")
# Power "m", "n" y "o"
ggplot(data = subset(datos, Power %in% c("m", "n", "o")),
       aes(x = Power, y = InjuryAmount/1000)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,20000)+
  theme_minimal() +
  labs(title = "",
       x = "Power", y = "InjuryAmount (Miles euros)")
#-------------------------------------------------------------------------------
# 3.1.2.2.3 Brand y ClaimNb
#-------------------------------------------------------------------------------
# Descriptivos
table(datos$Brand)
# Brand "Fiat", "Japanese (except Nissan) or Korean" y "Mercedes, Chrysler or BMW"
ggplot(data = subset(datos, Brand %in% c("Fiat", "Japanese (except Nissan) or Korean", "Mercedes, Chrysler or BMW")),
       aes(x = Brand, y = ClaimNb)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,4)+
  theme_minimal() +
  labs(title = "",
       x = "Brand", y = "ClaimNb")
# Brand "Opel, General Motors or Ford", "Renault, Nissan or Citroen" y "Volkswagen, Audi, Skoda or Seat"
ggplot(data = subset(datos, Brand %in% c("Opel, General Motors or Ford", "Renault, Nissan or Citroen", "Volkswagen, Audi, Skoda or Seat")),
       aes(x = Brand, y = ClaimNb)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,4)+
  theme_minimal() +
  labs(title = "",
       x = "Brand", y = "ClaimNb")
# Brand "other" 
ggplot(data = subset(datos, Brand %in% c("other")),
       aes(x = Brand, y = ClaimNb)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,4)+
  theme_minimal() +
  labs(title = "",
       x = "Brand", y = "ClaimNb")
#-------------------------------------------------------------------------------
# 3.1.2.2.4 Brand y InjuryAmount
#-------------------------------------------------------------------------------
# Descriptivos
table(datos$Brand)
# Brand "Fiat", "Japanese (except Nissan) or Korean" y "Mercedes, Chrysler or BMW"
ggplot(data = subset(datos, Brand %in% c("Fiat", "Japanese (except Nissan) or Korean", "Mercedes, Chrysler or BMW")),
       aes(x = Brand, y = InjuryAmount/1000)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,20000)+
  theme_minimal() +
  labs(title = "",
       x = "Brand", y = "InjuryAmount (Miles euros)")
# Brand "Opel, General Motors or Ford", "Renault, Nissan or Citroen" y "Volkswagen, Audi, Skoda or Seat"
ggplot(data = subset(datos, Brand %in% c("Opel, General Motors or Ford", "Renault, Nissan or Citroen", "Volkswagen, Audi, Skoda or Seat")),
       aes(x = Brand, y = InjuryAmount/1000)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,20000)+
  theme_minimal() +
  labs(title = "",
       x = "Brand", y = "InjuryAmount (Miles euros)")
# Brand "other"
ggplot(data = subset(datos, Brand %in% c("other")),
       aes(x = Brand, y = InjuryAmount/1000)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,20000)+
  theme_minimal() +
  labs(title = "",
       x = "Brand", y = "InjuryAmount (Miles euros)")
#-------------------------------------------------------------------------------
# 3.1.2.2.5 Gas y ClaimNb
#-------------------------------------------------------------------------------
# Descriptivos
table(datos$Gas)
# Gas "Diesel" y "Regular"
ggplot(data = subset(datos, Gas %in% c("Diesel", "Regular")),
       aes(x = Gas, y = ClaimNb)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,4)+
  theme_minimal() +
  labs(title = "",
       x = "Gas", y = "ClaimNb")
#-------------------------------------------------------------------------------
# 3.1.2.2.6 Gas y InjuryAmount
#-------------------------------------------------------------------------------
# Descriptivos
table(datos$Gas)
# Gas "Diesel", "Regular"
ggplot(data = subset(datos, Gas %in% c("Diesel", "Regular")),
       aes(x = Gas, y = InjuryAmount/1000)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,20000)+
  theme_minimal() +
  labs(title = "",
       x = "Gas", y = "InjuryAmount (Miles euros)")
#-------------------------------------------------------------------------------
# 3.1.2.2.7 Region y ClaimNb
#-------------------------------------------------------------------------------
# Descriptivos
table(datos$Region)
# Region "Aquitaine", "Basse-Normandie" y "Bretagne"
ggplot(data = subset(datos, Region %in% c("Aquitaine", "Basse-Normandie", "Bretagne")),
       aes(x = Region, y = ClaimNb)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,4)+
  theme_minimal() +
  labs(title = "",
       x = "Region", y = "ClaimNb")
# Region "Centre", "Haute-Normandie" y "Ile-de-France"
ggplot(data = subset(datos, Region %in% c("Centre", "Haute-Normandie", "Ile-de-France")),
       aes(x = Region, y = ClaimNb)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,4)+
  theme_minimal() +
  labs(title = "",
       x = "Region", y = "ClaimNb")
# Region "Limousin", "Nord-Pas-de-Calais" y "Pays-de-la-Loire"
ggplot(data = subset(datos, Region %in% c("Limousin", "Nord-Pas-de-Calais", "Pays-de-la-Loire")),
       aes(x = Region, y = ClaimNb)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,4)+
  theme_minimal() +
  labs(title = "",
       x = "Region", y = "ClaimNb")
# Region "Poitou-Charentes"
ggplot(data = subset(datos, Region %in% c("Poitou-Charentes")),
       aes(x = Region, y = ClaimNb)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,4)+
  theme_minimal() +
  labs(title = "",
       x = "Region", y = "ClaimNb")
#-------------------------------------------------------------------------------
# 3.1.2.2.8 Region y InjuryAmount
#-------------------------------------------------------------------------------
# Descriptivos
table(datos$Region)
# Region "Aquitaine", "Basse-Normandie" y "Bretagne"
ggplot(data = subset(datos, Region %in% c("Aquitaine", "Basse-Normandie", "Bretagne")),
       aes(x = Region, y = InjuryAmount/1000)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,20000)+
  theme_minimal() +
  labs(title = "",
       x = "Region", y = "InjuryAmount (Miles euros)")
# Region "Centre", "Haute-Normandie" y "Ile-de-France"
ggplot(data = subset(datos, Region %in% c("Centre", "Haute-Normandie", "Ile-de-France")),
       aes(x = Region, y = InjuryAmount/1000)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,20000)+
  theme_minimal() +
  labs(title = "",
       x = "Region", y = "InjuryAmount (Miles euros)")
# Region "Limousin", "Nord-Pas-de-Calais" y "Pays-de-la-Loire"
ggplot(data = subset(datos, Region %in% c("Limousin", "Nord-Pas-de-Calais", "Pays-de-la-Loire")),
       aes(x = Region, y = InjuryAmount/1000)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,20000)+
  theme_minimal() +
  labs(title = "",
       x = "Region", y = "InjuryAmount (Miles euros)")
# Region "Poitou-Charentes"
ggplot(data = subset(datos, Region %in% c("Poitou-Charentes")),
       aes(x = Region, y = InjuryAmount/1000)) +
  geom_jitter(width = 0.15, height = 0.15, alpha = 0.6, color = "lightblue") +
  ylim(-0.5,20000)+
  theme_minimal() +
  labs(title = "",
       x = "Region", y = "InjuryAmount (Miles euros)")
#-------------------------------------------------------------------------------
# 3.2. Modelización seleccionada y objetivos a alcanzar
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.2.1 Número de siniestros (ClaimNb)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.2.1.1 Modelo de regresión lineal clásico
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.2.1.2 Modelo Poisson
#-------------------------------------------------------------------------------
ClaimNb_Poisson_01<-glm( ClaimNb ~ offset(log(Exposure)), data=datos, family=poisson)
ClaimNb_Poisson_02<-glm( ClaimNb ~ offset(log(Exposure)) + DriverAge, data=datos, family=poisson)
ClaimNb_Poisson_03<-glm( ClaimNb ~ offset(log(Exposure)) + DriverAge + Density, data=datos, family=poisson)
ClaimNb_Poisson_04<-glm( ClaimNb ~ offset(log(Exposure)) + DriverAge + Density + CarAge, data=datos, family=poisson)
ClaimNb_Poisson_05<-glm( ClaimNb ~ offset(log(Exposure)) + DriverAge + Density + CarAge + RegionCluster, data=datos, family=poisson)
ClaimNb_Poisson_06<-glm( ClaimNb ~ offset(log(Exposure)) + DriverAge + Density + CarAge + RegionCluster + Gas, data=datos, family=poisson)
ClaimNb_Poisson_07<-glm( ClaimNb ~ offset(log(Exposure)) + DriverAge + Density + CarAge + RegionCluster + Gas + BrandCluster, data=datos, family=poisson)
ClaimNb_Poisson_08<-glm( ClaimNb ~ offset(log(Exposure)) + DriverAge + Density + CarAge + RegionCluster + Gas + BrandCluster +  PowerNumCluster, data=datos, family=poisson)
summary(ClaimNb_Poisson_01)
summary(ClaimNb_Poisson_02)
summary(ClaimNb_Poisson_03)
summary(ClaimNb_Poisson_04)
summary(ClaimNb_Poisson_05)
summary(ClaimNb_Poisson_06)
summary(ClaimNb_Poisson_07)
summary(ClaimNb_Poisson_08)
anova(ClaimNb_Poisson_01,ClaimNb_Poisson_02,ClaimNb_Poisson_03,ClaimNb_Poisson_04,ClaimNb_Poisson_05,ClaimNb_Poisson_06,ClaimNb_Poisson_07,ClaimNb_Poisson_08)
# e^B
round(exp(ClaimNb_Poisson_08$coefficients),4)
# IRR
round((exp(ClaimNb_Poisson_08$coefficients)-1)*100,4)
# ¿Hay sobredispersión?
ClaimNb_Poisson_mean<-mean(datos$ClaimNb)
ClaimNb_Poisson_var<-var(datos$ClaimNb)
# Relación entre deviance y grados de libertad
ClaimNb_Poisson_deviance_ratio<-deviance(ClaimNb_Poisson_08)/df.residual(ClaimNb_Poisson_08)
# Predicción
datos$ClaimNb_Poisson_Pred<-predict(ClaimNb_Poisson_08,type="response")
datos$ClaimNb_Poisson_Pred<-round(datos$ClaimNb_Poisson_Pred,2)
# Barplot
ClaimNb_Poisson_Pred_barplot<-barplot(round(prop.table(table(datos$ClaimNb_Poisson_Pred))*100,1),
                         main = "Predicción (ClaimNb)",
                         xlab = "Número de reclamos",
                         ylab = "Frecuencia relativa (%)",
                         col = "lightblue",
                         ylim = c(0,120))
# Añadir segunda línea al título
mtext("Poisson", side = 3, line = 0.5, cex = 1)
# Diágnosis del modelo
# Extraer residuos
ClaimNb_Poisson_Pred_residuos<-residuals(ClaimNb_Poisson_08,type="deviance")
# Histograma
hist(ClaimNb_Poisson_Pred_residuos,
     main="Residuos de desviación",
     xlab = "Valor de los residuos",
     ylab="Frecuencia absoluta",
     col="lightblue",
     breaks=20)
# Añadir segunda línea al título
mtext("Poisson", side = 3, line = 0.5, cex = 1)
#-------------------------------------------------------------------------------
# 3.2.1.3 Modelos lineales generalizados (GLM)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.2.1.3.1 Elección binaria - logit y probit
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.2.1.3.2 Elección multinomial
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.2.1.3.3 Elección multinomial ordenada
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.2.1.3.4 Elección multinomial anidada
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.2.1.4 Cluster jerárquico aglomerativo
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.2.1.5 Cluster jerárquico divisivo
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.2.1.6 Cluster no jerárquico - k means
#-------------------------------------------------------------------------------
# Cluster Exposure
round(Exposure_kmeans_result$centers,1)
round(Exposure_centroides_original,1)
# Mostrar tamaño de cada grupo
print(table(datos$ExposureCluster))
# Mostrar rango (mínimo y máximo) por cluster
print(round(ExposureCluster_rango,1))
#
# Cluster CarAge
round(CarAge_kmeans_result$centers,1)
round(CarAge_centroides_original,1)
# Mostrar tamaño de cada grupo
print(table(datos$CarAgeCluster))
# Mostrar rango (mínimo y máximo) por cluster
print(round(CarAgeCluster_rango,1))
#
# Cluster DriverAge
round(DriverAge_kmeans_result$centers,1)
round(DriverAge_centroides_original,1)
# Mostrar tamaño de cada grupo
print(table(datos$DriverAgeCluster))
# Mostrar rango (mínimo y máximo) por cluster
print(round(DriverAgeCluster_rango,1))
#
# Cluster Density
round(Density_kmeans_result$centers,1)
round(Density_centroides_original,1)
# Mostrar tamaño de cada grupo
print(table(datos$DensityCluster))
# Mostrar rango (mínimo y máximo) por cluster
print(round(DensityCluster_rango,1))
#
# Cluster Power
round(PowerNum_kmeans_result$centers,1)
round(PowerNum_centroides_original,1)
# Mostrar tamaño de cada grupo
print(table(datos$PowerNumCluster))
# Mostrar rango (mínimo y máximo) por cluster
print(round(PowerNumCluster_rango,1))
#-------------------------------------------------------------------------------
# 3.2.1.7 Análisis PCA
#-------------------------------------------------------------------------------
temporal<-cbind(Exposure_scaled,CarAge_scaled,DriverAge_scaled,Density_scaled,PowerNum_scaled)
temporal_correlaciones<-as.matrix(cor(temporal))
rownames(temporal_correlaciones)<-c("Exposure","CarAge","DriverAge","Density","Power")
colnames(temporal_correlaciones)<-c("Exposure","CarAge","DriverAge","Density","Power")
cor(temporal_correlaciones)
# Gráfico
corrplot(temporal_correlaciones,
         method = "circle",      # círculos
         type = "lower",         # solo la parte inferior
         tl.col = "black",         # color de las etiquetas
         tl.cex = 0.8,           # tamaño de las etiquetas
         col = colorRampPalette(c("red", "grey90", "lightblue"))(200)) # paleta de colores
princomp(temporal)
summary(princomp(temporal))
fviz_eig(princomp(temporal), addlabels = TRUE)
fviz_pca_var(princomp(temporal))
fviz_cos2(princomp(temporal), choice = "var", axes = 1:2)
fviz_pca_var(princomp(temporal), col.var = "cos2",
             gradient.cols = c("lightblue", "grey90", "red"),
             repel = TRUE)
#-------------------------------------------------------------------------------
# 3.2.2 Coste del siniestro
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.2.2.1 Modelización no paramétrica
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.2.2.2 Modelización paramétrica
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.2.2.3 Teoría de valores extremos
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.2.2.4 Distribuciones compuestas
#-------------------------------------------------------------------------------
# Cambio de variable 
L<-daños_corporales
L<-as.matrix(L)
# Hillplot
hp<-evmix::hillplot(L)
# El threshold es 1616.
#
# ME
meplot(L)
#
# CV plot
ercv::cvplot(L)
# Cola pesada, encima de 1.
#
# Weibull
# Ajuste de los daños corporales a una distribución Weibull, almacenamos:
# - Vector con los parámetros estimados: shape y escale.
# - Errores estándar de los parámetros.
# - Log-verosimilitud del ajuste.
res<-fitdistr(L,"weibull")
# Almacenamos algunos parámetros de la distribución Weibull ajustada
fWei<-c(res$estimate,res$loglik,AIC(res),BIC(res))
# Convertir en matriz
fWei<-t(matrix(unlist(fWei),5,1))
# Cambiar los nombres de las columnas y filas
colnames(fWei)<-c("shape","scale","loglik","AIC","BIC")
rownames(fWei)<-c("Losses")
# VaR 99.5%
round(qweibull(0.995, shape = res$estimate[1], scale = res$estimate[2]),0)
# Ver los resultados
fWei
round(fWei,2)
round(fWei,0)
#
# Lognormal
# Ajuste de los daños corporales a una distribución Lognormal, almacenamos:
# - Vector con los parámetros estimados: media y desviación estándar del logaritmo natural de los datos.
# - Errores estándar de los parámetros.
# - Log-verosimilitud del ajuste.
res<-fitdistr(L,"lognormal")
# Almacenamos algunos parámetros de la distribución lognormal ajustada
fln<-c(res$estimate,res$loglik,AIC(res),BIC(res))
# Convertir en matriz
fln<-t(matrix(unlist(fln),5,1))
# Cambiar los nombres de las columnas y filas
colnames(fln)<-c("mean","sd","loglik","AIC","BIC")
rownames(fln)<-c("Losses")
# VaR 99.5%
round(qlnorm(0.995, meanlog = res$estimate[1], sdlog = res$estimate[2]),0)
# Ver los resultados
fln
round(fln,2)
round(fln,0)
#
# Pareto (Valores Extremos)
# Tamaño del objeto daños_corporales
n<-length(L)
# Ordenar el objeto de mayor a menor
L<-sort(L, decreasing = T)
# Guardar los 1616 (Threshold) daños_corporales más altos
L99<-L[1:1616]
# Ajuste de los daños corporales extremos a una distribución Pareto
epar<-epareto(L99, method = "mle")
# Parámetros de la distribución Pareto ajustada a los datos
epar$parameters
# Guardar el parámetro shape
Hpar<-epar$parameters[2]
# Guardar el parámetro location
k<-epar$parameters[1]
# Probabilidad acumulada en el cuerpo de la distribución compuesta
fr<-(n-1616)/n
# [1] 0.9001298
# Nivel de confianza del VaR
q<-0.995
# Probabilidad acumulada que buscamos para llegar al VaR
alfa<-q-fr
# [1] 0.09487022
# Probabilidad acumulada en la cola de la distribución
1-fr
# [1] 0.09987022
# Nivel de confianza que buscamos en la cola de la distribución para llegar al VaR
conf<-alfa/(1-fr)
# [1] 0.949935
# VaR
VaR.Par<-as.numeric(k)/((1-conf)**(1/as.numeric(Hpar)))
VaR.Par
#
# Pareto: Cálculo de la densidad de la distribución
# Donde:
# - x: vector de valores donde se evalúa la densidad.
# - theta: parámetro de escala (scale) de la distribución Pareto.
# - a: parámetro de forma (shape) de la distribución Pareto.
# Comentarios:
# - La distribución Pareto solo está definida para x>theta. Por eso se crea un vector
#   lógico que valdrá 1 ó 0 (x>theta)
# - Corresponde a la función de densidad: (a*theta**a)/(x**(a+1))
fpareto<-function(x,theta,a){
  rr<-(x>theta)*(a*theta**a)/(x**(a+1))
  return(rr)
}
#
# Compuesta Weibull-Pareto: Cálculo de la densidad de la distribución
# Donde:
# - x: vector de valores donde se evalúa la densidad.
# - alpha: parámetro shape de la Weibull.
# - sigma: parámetro scale de la Weibull.
# - r: peso (probabilidad) asignada a la parte Weibull truncada.
# - theta: umbral que separa la zona Weibull (≤ θ) de la zona Pareto (> θ).
# - a: parámetro shape de Pareto (a > 0).
# Comentarios:
# - Para valores de x<=theta, usamos la distribución Weibull.
# - Para valores de x>theta, usamos la distribución Pareto.
# - (x<=theta) actúa como indicador, vale 1 si x<=theta y 0 si x>theta.
# - dweibull(x,alpha,sigma) es la densidad Weibull con parámetros alpha y sigma.
# - pweibull(theta, alpha, sigma) es la probabilidad acumulada de Weibull hasta theta.
# - El factor r/pweibull() reescala la densidad Weibull para que la probabilidad 
#   total que quede hasta theta sea exactamente r.
# - fpareto(x,theta,a) es la densidad Pareto con umbral theta y parámetro de forma a.
# - Multiplicamos por (1-r) para que la probabilidad total en la cola sea (1-r).
fwepar<-function(x,alpha,sigma,r,theta,a){
  rr<-((x<=theta)*(r/pweibull(theta,alpha,sigma))*dweibull(x,alpha,sigma))+((x>theta)*(1-r)*fpareto(x,theta,a))
  return(rr)
}
#
# Compuesta Lognormal-Pareto: Cálculo de la densidad de la distribución
flnpar<-function(x,m,s,r,theta,a){
  rr<-((x<=theta)*r*dlnorm(x,m,s)/plnorm(theta,m,s))+((x>theta)*(1-r)*fpareto(x,theta,a))
  return(rr)
}
#
# Cálculo del Log-verosimilitud del modelo Weibull-Pareto dado un conjunto de 
# datos y valores fijos de r y θ.
lik_WP<-function(par,x,r,theta){
  alpha<-par[1]
  sigma<-par[2]
  a<-par[3]
  rr<-sum(log(fwepar(x,alpha,sigma,r,theta,a)))
  return(rr)
}
#
# Cálculo del Log-verosimilitud del modelo Lognormal-Pareto dado un conjunto de 
# datos y valores fijos de r y θ.
lik_lnP<-function(par,x,r,theta){
  m<-par[1]
  s<-par[2]
  a<-par[3]
  rr<-sum(log(flnpar(x,m,s,r,theta,a)))
  return(rr)
}
#
# Ajuste a una distribución compuesta Weibull-Pareto para daños_corporales, buscando
# el mejor peso "r" alrededor del valor empírico observado y maximizando la log-verosimilitud
# Parámetro de forma de la distribución Pareto
sh1<-epar$parameters[2]
# Vector inicial para la optimización con los parámetros de la distribución Weibull y Pareto
par1<-c(fWei[1:2],sh1)
# Fijar el umbral (theta). Punto de corte donde la distribución pasa de Weibull a Pareto
th1<-epar$parameters[1]
# Copiar el vector de datos a un nuevo objeto
x1<-L
# Número de observaciones del cuerpo de la distribución compuesta
n1<-sum(x1<=th1)
# Proporción empírica a la izquierda. Este r1 es un punto de partida "natural" para
# el peso de "r"
r1<-n1/n
# Rejilla de valores candidatos para "r" centrada en "r1" y con ancho ±0.2 (paso 0.01)
gr1<-seq((r1-0.2),(r1+0.2),by=0.01)
# Tamaño de la rejilla
ng1<-length(gr1)
# Si el extremo inferior de la rejilla cae por debajo de 0, lo recorta a 0.01 y 
# refina el paso a 0.001 para mejorar resolución cerca del borde
if(gr1[1]<0){gr1[(gr1<0)]<-0.01
gr1<-seq(min(gr1),max(gr1),by=0.001)}
# Si el extremo superior excede 1, lo recorta a 0.99 y también refina la rejilla
# Garantizar que 0 < r < 1, condición necesaria para que la mezcla sea válida.
if(gr1[ng1]>1){gr1[(gr1>1)]<-0.99
gr1<-seq(min(gr1),max(gr1),by=0.001)}
# Reserva una matriz de resultados con 6 columnas para cada valor de r
# Contiene:
# - theta usado (th1)
# - Parámetro "shape" de Weibull óptimo para ese "r".
# - Parámetro "scale" de Weibull óptimo para ese "r".
# - Parámetro "shape" de Pareto óptimo para ese "r".
# - r de la rejilla
# - log-verosimilitud máxima alcanzada para ese "r"
parlik1_WP<-matrix(0,length(gr1),6)
for(j in 1:length(gr1)){
  r1<-gr1[j] # Fija r1
  opt_parWep1<-optim(par1,fn=lik_WP,x=x1,r=r1,theta=th1,control=list(fnscale=-1)) # Maximiza la log-verosimilitud respecto a los parámetros de la distribución manteniendo fijos "r" y "theta"
  parlik1_WP[j,1]<-th1 # Guardar theta
  parlik1_WP[j,2:4]<-opt_parWep1$par # Guardar el vector de parámetros óptimos de las distribuciones
  parlik1_WP[j,5]<-r1 # Guardar r
  parlik1_WP[j,6]<-lik_WP(opt_parWep1$par,x1,r1,th1) # Guardar log-verosimilitud
}
# Selecciona la fila con la mayor verosimilitud
optimWP1<-as.matrix(parlik1_WP[which(parlik1_WP[,6]==max(parlik1_WP[,6])),])
# AIC y BIC del mejor ajuste
AIC1<-2*4-2*optimWP1[6]
BIC1<-4*log(length(x1))-2*optimWP1[6]
# Armar la salida final en una fila
f_weipar<-t(as.matrix(c(optimWP1,AIC1,BIC1)))
colnames(f_weipar)<-c("theta","shape","rate","alpha","r","loglik","AIC","BIC")
rownames(f_weipar)<-c("Losses")
# VaR
q<-0.995
alfa<-q-f_weipar[5]
conf<-alfa/(1-f_weipar[5])
VaR.WeiPar<-f_weipar[1]/((1-conf)**(1/f_weipar[4]))
# Resultados
f_weipar
round(f_weipar,2)
round(f_weipar,0)
#
# Gráfico
# Ingresar manualmente los parámetros para 3 curvas
# Cada curva: alpha (Weibull shape), sigma (Weibull scale), r (peso), theta (umbral), a (Pareto shape)
params <- list(
  curva1 = list(alpha = 0.5854359, sigma = 5.528376, r = 0.9001916, theta = 21.09075, a = 1.114799))
# Crear un grid de x
x_min <- 0
x_max <- 30
x_grid <- seq(x_min, x_max, length.out = 100)
# Graficar las tres curvas
plot(x_grid, fwepar(x_grid, params$curva1$alpha, params$curva1$sigma, params$curva1$r, params$curva1$theta, params$curva1$a),
     type = "l", lwd = 2, col = "lightblue",
     main = "Weibull–Pareto",
     xlab = "InjuryAmount (miles euros)", 
     ylab = "Densidad de probabilidad",
     ylim=c(0,0.35))
# Añadir segunda línea al título
mtext("(Sin condición de continuidad)", side = 3, line = 0.5, cex = 1)
#
# Ajuste a una distribución compuesta Lognormal-Pareto para daños_corporales, buscando
# el mejor peso "r" alrededor del valor empírico observado y maximizando la log-verosimilitud
# Parámetro de forma de la distribución Pareto
sh1<-epar$parameters[2]
# Vector inicial para la optimización con los parámetros de la distribución Lognormal y Pareto
par1<-c(fln[1:2],sh1)
# Fijar el umbral (theta). Punto de corte donde la distribución pasa de Lognormal a Pareto
th1<-epar$parameters[1]
# Copiar el vector de datos a un nuevo objeto
x1<-L
# Número de observaciones del cuerpo de la distribución compuesta
n1<-sum(x1<=th1)
# Proporción empírica a la izquierda. Este r1 es un punto de partida "natural" para
# el peso de "r"
r1<-n1/n
# Rejilla de valores candidatos para "r" centrada en "r1" y con ancho ±0.2 (paso 0.01)
gr1<-seq((r1-0.2),(r1+0.2),by=0.01)
# Tamaño de la rejilla
ng1<-length(gr1)
# Si el extremo inferior de la rejilla cae por debajo de 0, lo recorta a 0.01 y 
# refina el paso a 0.001 para mejorar resolución cerca del borde
if(gr1[1]<0){gr1[(gr1<0)]<-0.01
gr1<-seq(min(gr1),max(gr1),by=0.001)}
# Si el extremo superior excede 1, lo recorta a 0.99 y también refina la rejilla
# Garantizar que 0 < r < 1, condición necesaria para que la mezcla sea válida.
if(gr1[ng1]>1){gr1[(gr1>1)]<-0.99
gr1<-seq(min(gr1),max(gr1),by=0.001)}
# Reserva una matriz de resultados con 6 columnas para cada valor de r
# Contiene:
# - theta usado (th1)
# - Parámetro "meanlog" de Lognormal óptimo para ese "r".
# - Parámetro "sdlog" de Lognormal óptimo para ese "r".
# - Parámetro "shape" de Pareto óptimo para ese "r".
# - r de la rejilla
# - log-verosimilitud máxima alcanzada para ese "r"
parlik1_LnP<-matrix(0,length(gr1),6)
for(j in 1:length(gr1)){
  r1<-gr1[j]
  opt_parLnp1<-optim(par1,fn=lik_lnP,x=x1,r=r1,theta=th1,control=list(fnscale=-1))
  parlik1_LnP[j,1]<-th1
  parlik1_LnP[j,2:4]<-opt_parLnp1$par
  parlik1_LnP[j,5]<-r1
  parlik1_LnP[j,6]<-lik_lnP(opt_parLnp1$par,x1,r1,th1)
}
# Selecciona la fila con la mayor verosimilitud
optimLnP1<-as.matrix(parlik1_LnP[which(parlik1_LnP[,6]==max(parlik1_LnP[,6])),])
# AIC y BIC del mejor ajuste
AIC1<-2*3-2*optimLnP1[6]
BIC1<-3*log(length(x1))-2*optimLnP1[6]
# Armar la salida final en una fila
f_lnpar<-t(as.matrix(c(optimLnP1,AIC1,BIC1)))
colnames(f_lnpar)<-c("theta","mu","sigma","alpha","r","loglik","AIC","BIC")
rownames(f_lnpar)<-c("Losses")
# VaR
q<-0.995
alfa<-q-f_lnpar[5]
conf<-alfa/(1-f_lnpar[5])
VaR.lnpar<-f_lnpar[1]/((1-conf)**(1/f_lnpar[4]))
# Resultados
f_lnpar
round(f_lnpar,2)
round(f_lnpar,0)
#
# Condición de Continuidad Lognormal-Pareto:
rcon<-fpareto(f_lnpar[1]+0.00000001,f_lnpar[1],f_lnpar[4])/
  ((dlnorm(f_lnpar[1]+0.00000001,f_lnpar[2],f_lnpar[3])/plnorm(f_lnpar[1]+0.00000001,f_lnpar[2],f_lnpar[3]))+fpareto(f_lnpar[1]+0.00000001,f_lnpar[1],f_lnpar[4]))# Fitting Weib-Par univariate marginals
rcon
r1<-rcon
opt_cont1<-optim(par1,fn=lik_lnP,x=x1,r=r1,theta=th1,control=list(fnscale=-1))
AIC1<-2*2-2*opt_cont1$value
# VaR
q<-0.995
alfa<-q-rcon
conf<-alfa/(1-rcon)
VaR.lnpar<-f_lnpar[1]/((1-conf)**(1/f_lnpar[4]))

# Gráfico
# Ingresar manualmente los parámetros para 3 curvas
# Cada curva: alpha (Lognormal shape), sigma (Lognormal scale), r (peso), theta (umbral), a (Pareto shape)
params <- list(
  curva1 = list(mu = 3.017956, sigma = 3.372175, r = 0.9001916, theta = 21.09075, a = 1.114728),
  curva2 = list(mu = 3.017956, sigma = 3.372175, r = 0.8259629, theta = 21.09075, a = 1.114728))
# Crear un grid de x
x_min <- 0
x_max <- 30
x_grid <- seq(x_min, x_max, length.out = 100)
# Graficar la curva 1
plot(x_grid, flnpar(x_grid, params$curva1$mu, params$curva1$sigma, params$curva1$r, params$curva1$theta, params$curva1$a),
     type = "l", lwd = 2, col = "lightblue",
     main = "Lognormal–Pareto",
     xlab = "InjuryAmount (miles euros)", 
     ylab = "Densidad de probabilidad",
     ylim=c(0,0.35))
# Añadir segunda línea al título
mtext("(Sin condición de continuidad)", side = 3, line = 0.5, cex = 1)
# Graficar la curva 2
plot(x_grid, flnpar(x_grid, params$curva2$mu, params$curva2$sigma, params$curva2$r, params$curva2$theta, params$curva2$a),
     type = "l", lwd = 2, col = "lightblue",
     main = "Lognormal–Pareto",
     xlab = "InjuryAmount (miles euros)", 
     ylab = "Densidad de probabilidad",
     ylim=c(0,0.35))
# Añadir segunda línea al título
mtext("(Condición de continuidad)", side = 3, line = 0.5, cex = 1)
#-------------------------------------------------------------------------------
# 3.2.2.5 Distribuciones multivariadas
#-------------------------------------------------------------------------------











#-------------------------------------------------------------------------------
# Definición de las variables numéricas
namesNum <-c("ClaimNb", "Exposure", "CarAge", "DriverAge",
             "Density", "ClaimAmount", "InjuryAmount", "PropertyAmount")
# Definición de las variables categóricas
namesChar<-c("Power","Brand","Gas","Region")
# Convertir variables categóricas a factor
datos <- datos %>%
  mutate(
    Power  = factor(Power),
    Brand  = factor(Brand),
    Gas    = factor(Gas),
    Region = factor(Region)
  )

# Filtrar pólizas con exposición válida
datos <- datos %>% filter(Exposure > 0)


apply(datos[namesNum],2,summary)
apply(datos[namesNum],2,var)
apply(datos[namesNum],2,skewness)
apply(datos[namesNum],2,kurtosis)
apply(datos[namesNum],2,boxplot)
apply(datos[namesNum],2,hist)
apply(datos[namesChar],2,table)

pairs <- combn(namesChar, 2, simplify = FALSE)
for (p in pairs) {
  cat("\nTabla de:", p[1], "vs", p[2], "\n")
  print(table(datos[[p[1]]], datos[[p[2]]]))
}




cor_matrix<-cor(datos[namesNum], use = "complete.obs")
heatmap(cor_matrix, 
        Rowv = NA, Colv = NA, 
        col = colorRampPalette(c("red","white","green"))(20), 
        scale = "none")




for (catVar in namesChar) {
  for (numVar in namesNum) {
    p <- ggplot(datos, aes_string(x = catVar, y = numVar)) +
      geom_boxplot(fill = "lightblue") +
      labs(title = paste("Boxplot de", numVar, "por", catVar),
           x = catVar, y = numVar) +
      theme_minimal()
    print(p)
  }
}

#Tarda mucho este scatterplor en ejecutarse
#pairs(datos[namesNum], main="Scatterplots entre variables numéricas")



apply(datos[,-datos$InjuryAmount],2,function(x) lm(datos$InjuryAmount~x))


names_without_output <- c("ClaimNb","Exposure","CarAge","DriverAge",
              "Density","ClaimAmount","PropertyAmount",
              "Power","Brand","Gas","Region")  # No esta InjuryAmount
datos1<-datos[,-1]#Quitamos PolicyID

modelSimple<- function(output, data=datos1, input_names=names_without_output) {
  
  
  
  models <- lapply(input_names, function(x) {
    
    formula <- as.formula(paste(output, "~", x))
    
    lm(formula, data = datos)
    
  })
  
  return(models)
  
}

#Modelo Simple para Coste de Daños Personales. Cuantificación de Riesgos

modelSimple_InjuryAmount<-modelSimple(output="InjuryAmount",datos1,names_without_output)

modelSimple_InjuryAmount

summary_modelSimple_InjuryAmount<-lapply(modelSimple_InjuryAmount, summary)

summary_modelSimple_InjuryAmount

#Modelo General para Coste de Daños Personales. Cuantificación de Riesgos

modelGeneral_InjuryAmount<-lm(InjuryAmount~.,data=datos1)

summary(modelGeneral_InjuryAmount)



#Modelo Simple para Número de Siniestros. Modelos Estadísticos

modelSimple_ClaimNb<-modelSimple(output="ClaimNb",datos1,names_without_output)

modelSimple_ClaimNb

summary_modelSimple_ClaimNb<-lapply(modelSimple_ClaimNb, summary)

summary_modelSimple_ClaimNb

#Modelo General para Número de Siniestros. Modelos Estadísticos

modelGeneral_ClaimNb<-lm(ClaimNb~.,data=datos1)

summary(modelGeneral_ClaimNb)


#Descargamos los datos modificados
#library(openxlsx)
#nombre_archivo <- "datos.xlsx" 
#write.xlsx(x = datos, path = nombre_archivo)





#Parte de Cuantificación
###############################################
#   5. AJUSTE DEL MODELO POISSON (FRECUENCIA)
###############################################

fit_glm_2 <- glm(
  ClaimNb ~ DriverAge + CarAge + Power + Brand + Gas + Region + Density,
  data   = datos,
  family = poisson(link = "log"),
  offset = log(Exposure)
)

summary(fit_glm_2)

###############################################
#   6. SOBREDISPERSIÓN
###############################################

mean_claim <- mean(datos$ClaimNb)
var_claim  <- var(datos$ClaimNb)
cat("Media ClaimNb:", mean_claim, "  Varianza ClaimNb:", var_claim, "\n")

deviance_ratio <- deviance(fit_glm_2) / df.residual(fit_glm_2)
cat("Razón Deviance/GL:", deviance_ratio, "\n")

if (deviance_ratio > 1.5) {
  cat("Hay indicios de sobredispersión.\n")
} else {
  cat("No hay evidencia de sobredispersión.\n")
}

###############################################
#   7. TEST CHI-CUADRADO DE AJUSTE GLOBAL
###############################################

with(
  fit_glm_2,
  cbind(
    res.deviance = deviance,
    df           = df.residual,
    p            = pchisq(deviance, df.residual, lower.tail = FALSE)
  )
)

###############################################
#   8. ERRORES ROBUSTOS
###############################################

cov_poisson_rob <- sandwich::vcovHC(fit_glm_2, type = "HC0")
std.err <- sqrt(diag(cov_poisson_rob))

resultados_robustos <- cbind(
  Estimate     = coef(fit_glm_2),
  `EE robusto` = std.err,
  `Pr(>|z|)`   = 2 * pnorm(abs(coef(fit_glm_2)/std.err), lower.tail = FALSE)
)

resultados_robustos

###############################################
#   9. MODELO QUASIPOISSON
###############################################

mod_quasi <- glm(
  ClaimNb ~ DriverAge + CarAge + Power + Brand + Gas + Region + Density,
  data   = datos,
  family = quasipoisson(link = "log"),
  offset = log(Exposure)
)

summary(mod_quasi)

###############################################
#   10. MODELO BINOMIAL NEGATIVO
###############################################

mod_nb <- glm.nb(
  ClaimNb ~ DriverAge + CarAge + Power + Brand + Gas + Region + Density + offset(log(Exposure)),
  data = datos
)

summary(mod_nb)

# Comparación Quasi vs Binomial Negativa
anova(mod_quasi, mod_nb, test = "F")

###############################################
#   11. PREDICCIONES DEL MODELO
###############################################

datos$pred <- predict(fit_glm_2, type = "response")

ggplot(datos, aes(x = pred)) +
  geom_histogram(color = "black", bins = 40) +
  theme_bw() +
  ggtitle("Predicciones del número esperado de siniestros")

###############################################
#   12. DIAGNÓSTICO
###############################################

par(mfrow = c(2,2))
plot(fit_glm_2)

# Residuos de deviance
res_dev <- residuals(fit_glm_2, type = "deviance")

hist(res_dev, col="lightblue", main="Residuos de deviance")
qqnorm(res_dev); qqline(res_dev, col="red")

# Distancia de Cook
cooks_d <- cooks.distance(fit_glm_2)
plot(cooks_d, type="h", main="Distancia de Cook")
abline(h = 4 / (nrow(datos)-length(coef(fit_glm_2))), col="red")







#######
#CUANTIFICACIÓN DE RIESGOS
injury_cost<-datos$InjuryAmount
kernel<-density(injury_cost);kernel
kernel2<-density(injury_cost,bw="SJ")
kernel3<-density(injury_cost,kernel="epanechnikov",bw="SJ")
sum(injury_cost>0)
