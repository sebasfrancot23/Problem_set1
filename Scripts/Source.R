###
#
# Programación problem set 1
# Código source.
# Integrantes:
# Sebastian Franco Torres
#
###

#Esto código está diseñado para poder correr todos los scripts en orden y en un
#solo lugar.

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

#webscrapping
source(paste0(path,"Scripts/Importacion_limpieza_datos.R"))

#Estadísticas descriptivas.
source(paste0(path,"Scripts/Estadisticas_descriptivas_imputacion.R"))

#Punto 3
source(paste0(path,"Scripts/Punto_3.R"))

#Punto 4
source(paste0(path,"Scripts/Punto 4.R"))

#Punto 5
source(paste0(path,"Scripts/Punto 5.R"))



