###################################
#
# Programación problem set 1
# Estadísticas descriptivas.
# Integrantes:
# Sebastian Franco Torres
#
#######################
# Plan de acción: 
# A las variables continuas sacales las estadísticas de siempre: N, media, sd, percentiles.
# A las categóricas hace un gráfico de barras para ver el número de datos por categoría.
# Si hay continuas con valores atípicos, hace un histograma para analizarlos mejor.

# Preparación del ambiente ------------------------------------------------
rm(list=ls())

libraries = c("ggplot2", "tidyverse", "skimr", "stargazer") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

#Se importa la base.
DB = readRDS(paste0(path,"Stores/Base_final.rds"))



# Estadísticas vars continuas ---------------------------------------------
#Las variables continuas son.
continuas = c("Ingresos_porhora", "hoursWorkUsual", "antiguedad_puesto", "Escolaridad")

DB_continuas = DB[,colnames(DB) %in% continuas]
  
#Se calculan distintas estadísticas:
#Número de observaciones #que no son missing
Observations = apply(DB_continuas, MARGIN = 2, function(x){
  Non_missings = sum(!is.na(x))
  return(Non_missings)
}) 

#Media
mean_aux = round(apply(DB_continuas, MARGIN = 2, mean, na.rm = TRUE),2)

#SD
sd_aux = round(apply(DB_continuas, MARGIN = 2, sd, na.rm = TRUE),2)

#Percentiles.
percentiles = t(round(apply(DB_continuas, MARGIN = 2, quantile, na.rm = TRUE,
                           probs = c(c(0.05, 0.25, 0.5, 0.75, 0.95))),2))

#En una matriz
Estadisticas_continuas = cbind(Observations, mean_aux, sd_aux, percentiles)
colnames(Estadisticas_continuas) = c("No. Observaciones", "Media", "Desv. Estándar", 
                                 "Per. 5", "Per. 25", "Per. 50", "Per. 75", 
                                 "Per 95")
#Hacia latex
stargazer(Estadisticas_continuas, type = "latex", title = "Estadísticas descriptivas",
          subtitle = "variables continuas", label = "Tabla_continuas",
          summary = FALSE)

#Histogramas de variables.
#Función auxiliar para gráficar un histograma.
Hist_aux = function(data, x){
  
  
  
  
  
}







