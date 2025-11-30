# Cambios en los outputs

library(usethis)
#usethis::edit_git_config()
#usethis::create_github_token()

#git clone https://github.com/rcde-val/Coste-Da-os-Corporales.git
#cd Coste-Da-os-Corporales
#File->Open Project y seleccionar .Rproj

#git add .
#git commit -m "Primer commit"

#git branch -M master   # asegura que tu rama se llama main
#git push -u origin master
# git push una vez que ya lo hayamos linkeado con el master, vamos a usar solo push

#-------------------------------------------------------------------------------
# Paquetes
#-------------------------------------------------------------------------------
# Definición de paquetes
paquetes <- c("tidyverse", "readxl", "usethis", "ggplot2", "moments","dplyr","sandwich","MASS","car","broom","performance")
# Instalación de paquetes no instalados (de ser requerido)
for (pkg in paquetes) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
install.packages("readxl") # Lector de Excel
install.packages("ggplot2") # Gráficos
install.packages("psych") # Estadísticos descriptivos
# Carga de paquetes
for (pkg in paquetes) {
  library(pkg, character.only = TRUE)
}
library("readxl") # Lector de Excel
library("ggplot2") # Gráficos
library("psych") # Estadísticos descriptivos 
#-------------------------------------------------------------------------------
# Importación de base de datos
#-------------------------------------------------------------------------------
# Lectura de la base de datos
datos <- read_excel("FC01_G03_BBDD.xlsx")
# Convertir en data frame
datos<-data.frame(datos)
# Encabezado
head(datos)
# Resumen
summary(datos)
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
# k-means para agrupar
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
     main = "Método del codo")
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
     main = "Método del codo")
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
# 3.1.1.1.4 DriveAge
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
     main = "Método del codo")
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
     main = "Método del codo")
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
table(datos$Power)
        






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
