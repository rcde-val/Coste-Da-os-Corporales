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


paquetes <- c("tidyverse", "readxl", "usethis", "ggplot2")

for (pkg in paquetes) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

for (pkg in paquetes) {
  library(pkg, character.only = TRUE)
}

datos <- read_excel("CASO 5_ daños corporales autos.xlsx")
head(datos)
