###
#
# Programación problem set 1
# Punto 5.
# Integrantes:
# Sebastian Franco Torres
#
###


# Preparación del ambiente ------------------------------------------------
rm(list=setdiff(ls(), "DB"))

libraries = c("ggplot2", "tidyverse", "stats", "stargazer", "caret") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

#Se define una semilla
set.seed(398759)

# Punto a. ----------------------------------------------------------------
#Se divide la muestra entre train y validación.
Train_index = createDataPartition(y = DB$log_ingresos_porhora, p = 0.7, 
                                  list = F, groups = 100) #groups garantiza 
#que en ambas bases vamos a tener todas las categorías de oficio.

DB_train = DB[Train_index,]
DB_test = DB[-Train_index,]

#Se reestiman los modelos con la base de entrenamiento.
lm_age = lm(log_ingresos_porhora ~ age+age_2, DB_train)
lm_sex = lm(log_ingresos_porhora ~ sex, DB_train)
lm_sex_multiple = lm(log_ingresos_porhora ~ sex+age+age_2 + maxEducLevel + 
                       oficio + sizeFirm, data = DB_train)
lm_sex_interaccion= lm(log_ingresos_porhora ~ sex+age+age_2 + age*sex + maxEducLevel + 
              oficio + sizeFirm, data = DB_train)



# Punto b. ----------------------------------------------------------------
#Por cada modelo, se calcula el RMSE del log_salario con la muestra de 
#validación.
prediccion_age = predict(lm_age, newdata = DB_test)
prediccion_sex = predict(lm_sex, newdata = DB_test)
prediccion_sex_multiple = predict(lm_sex_multiple, newdata = DB_test)
prediccion_sex_interaccion = predict(lm_sex_interaccion, newdata = DB_test)

#Y los RMSE:
RMSE_age = RMSE(prediccion_age, DB_test$log_ingresos_porhora)
RMSE_sex = RMSE(prediccion_sex, DB_test$log_ingresos_porhora)
RMSE_sex_multiple = RMSE(prediccion_sex_multiple, DB_test$log_ingresos_porhora)
RMSE_sex_interaccion = RMSE_sex_interaccion(prediccion_sex_interaccion, 
                                      DB_test$log_ingresos_porhora)



















