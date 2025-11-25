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


paquetes <- c("tidyverse", "readxl", "usethis", "ggplot2", "moments")

for (pkg in paquetes) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

for (pkg in paquetes) {
  library(pkg, character.only = TRUE)
}

datos <- read_excel("CASO 5_ daños corporales autos.xlsx")
datos<-data.frame(datos)
head(datos)
summary(datos)


namesNum <-c("ClaimNb", "Exposure", "CarAge", "DriverAge",
             "Density", "ClaimAmount", "InjuryAmount", "PropertyAmount")
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


pairs(datos[namesNum], main="Scatterplots entre variables numéricas")

apply(datos[,-datos$InjuryAmount],2,function(x) lm(datos$InjuryAmount~x))


names_without_output <- c("ClaimNb","Exposure","CarAge","DriverAge",
              "Density","ClaimAmount","PropertyAmount",
              "Power","Brand","Gas","Region")  # No esta InjuryAmount
datos1<-datos[,-1]#Quitamos PolicyID

models <- lapply(names_without_output, function(x) {
  formula <- as.formula(paste("InjuryAmount ~", x))
  lm(formula, data = datos1)
})
#models
summaries <- lapply(models, summary)
summaries

modelGeneral<-lm(InjuryAmount~.,data=datos1)
summary(modelGeneral)
