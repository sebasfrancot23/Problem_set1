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


















