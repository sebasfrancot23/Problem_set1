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
Observations = apply(DB_continuas, MARGIN = 2, function(x){
  Non_missings = sum(!is.na(x))
  return(Non_missings)
})

mean = apply(DB_continuas, MARGIN = 2, mean)

# Supongamos que tienes un dataframe llamado 'datos' y quieres calcular las estadísticas para la variable 'variable_a'
# Reemplaza 'datos' y 'variable_a' con los nombres reales de tu dataframe y variable

# Calcular el número de observaciones que no son missings
num_observaciones <- sum(!is.na(datos$variable_a))

# Calcular la media
media <- mean(datos$variable_a, na.rm = TRUE)

# Calcular la desviación estándar
desviacion_estandar <- sd(datos$variable_a, na.rm = TRUE)

# Calcular los percentiles 5, 25, 50, 75 y 95
percentiles <- quantile(datos$variable_a, probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE)

# Crear una tabla con las estadísticas
tabla_estadisticas <- matrix(c(num_observaciones, media, desviacion_estandar, percentiles), nrow = 1, byrow = TRUE)

# Nombrar las filas y columnas de la tabla
rownames(tabla_estadisticas) <- c("Variable_A")
colnames(tabla_estadisticas) <- c("No. Observaciones", "Media", "Desviación Estándar", "Percentil 5", "Percentil 25", "Percentil 50", "Percentil 75", "Percentil 95")

# Imprimir la tabla con stargazer
stargazer(tabla_estadisticas, type = "text", summary = FALSE)










