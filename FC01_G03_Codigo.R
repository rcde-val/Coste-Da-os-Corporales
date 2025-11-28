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
install.packages("psych") # Estadísticos descriptivos
# Carga de paquetes
for (pkg in paquetes) {
  library(pkg, character.only = TRUE)
}
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
# 3.1.1.1 Variables discretas y contínuas
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# 3.1.1.1.1 ClaimNb (Discreta)
#-------------------------------------------------------------------------------
# Estadísticos
describe(datos$ClaimNb)
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
# 3.1.1.1.2 Exposure (Contínua)
#-------------------------------------------------------------------------------
# Estadísticos
describe(datos$Exposure)
# Crear "n" intervalos 
Exposure_intervalos<-cut(datos$Exposure,breaks=5)
# Calcular frecuencias relativas asociadas a cada uno de los "n" intervalos
Exposure_freq_rel <- round(prop.table(table(Exposure_intervalos)) * 100, 1)
# Reformatear etiquetas con 1 decimal
Exposure_niveles_fmt <- gsub("\\(|\\]|\\[", "", levels(Exposure_intervalos))
Exposure_niveles_fmt <- sapply(strsplit(Exposure_niveles_fmt, ","), function(x) {
  sprintf("(%.1f, %.1f]", as.numeric(x[1]), as.numeric(x[2]))
})
# Barplot
Exposure_barplot<-barplot(Exposure_freq_rel,
              main = "Exposure",
              xlab = "Tiempo de vigencia y exposición al riesgo (años)",
              ylab = "Frecuencia relativa (%)",
              col = "lightblue",
              ylim = c(0, 120),
              names.arg = Exposure_niveles_fmt)
# Añadir etiquetas sobre cada barra
text(x = Exposure_barplot, 
     y = Exposure_freq_rel + 5,
     labels = sprintf("%.1f", Exposure_freq_rel))



    

# 

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
