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
install.packages("writexl") # Escribir Excel
install.packages("readxl") # Lector de Excel
install.packages("ggplot2") # Gráficos
install.packages("psych") # Estadísticos descriptivos
install.packages("corrplot") # Correlaciones
install.packages("dplyr") # Tratamiento de data frame
install.packages("factoextra") # Componentes principales
# Carga de paquetes
for (pkg in paquetes) {
  library(pkg, character.only = TRUE)
}
library("writexl") # Escribir Excel
library("readxl") # Lector de Excel
library("ggplot2") # Gráficos
library("psych") # Estadísticos descriptivos
library("corrplot") # Correlaciones
library("dplyr") # Tratamiento de data frame
library("factoextra") # Componentes principales
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
ClaimNb_Poisson_07<-glm( ClaimNb ~ offset(log(Exposure)) + DriverAge + Density + CarAge + RegionCluster + Gas + PowerNumCluster, data=datos, family=poisson)
ClaimNb_Poisson_08<-glm( ClaimNb ~ offset(log(Exposure)) + DriverAge + Density + CarAge + RegionCluster + Gas + PowerNumCluster +  BrandCluster, data=datos, family=poisson)
summary(ClaimNb_Poisson_01)
summary(ClaimNb_Poisson_02)
summary(ClaimNb_Poisson_03)
summary(ClaimNb_Poisson_04)
summary(ClaimNb_Poisson_05)
summary(ClaimNb_Poisson_06)
summary(ClaimNb_Poisson_07)
summary(ClaimNb_Poisson_08)
anova(ClaimNb_Poisson_01,ClaimNb_Poisson_02,ClaimNb_Poisson_03,ClaimNb_Poisson_04,ClaimNb_Poisson_05,ClaimNb_Poisson_06,ClaimNb_Poisson_07,ClaimNb_Poisson_08)
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
cor(temporal)
princomp(temporal)
summary(princomp(temporal))
fviz_eig(princomp(temporal), addlabels = TRUE)
fviz_pca_var(princomp(temporal))
fviz_cos2(princomp(temporal), choice = "var", axes = 1:2)
fviz_pca_var(princomp(temporal), col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)
#-------------------------------------------------------------------------------
# 3.2.2 Costo del siniestro
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
