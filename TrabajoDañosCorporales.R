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

# Definición de paquetes
paquetes <- c("tidyverse", "readxl", "usethis", "ggplot2", "moments")
# Instalación de paquetes no instalados (de ser requerido)
for (pkg in paquetes) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
# Carga de paquetes
for (pkg in paquetes) {
  library(pkg, character.only = TRUE)
}
# Lectura de la base de datos
datos <- read_excel("CASO 5_ daños corporales autos.xlsx")
# Convertir en data frame
datos<-data.frame(datos)
head(datos)
summary(datos)

# Definición de las variables numéricas
namesNum <-c("ClaimNb", "Exposure", "CarAge", "DriverAge",
             "Density", "ClaimAmount", "InjuryAmount", "PropertyAmount")
# Definición de las variables categóricas
namesChar<-c("Power","Brand","Gas","Region")



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
library(openxlsx)
nombre_archivo <- "datos.xlsx" 
write.xlsx(x = datos, path = nombre_archivo)
