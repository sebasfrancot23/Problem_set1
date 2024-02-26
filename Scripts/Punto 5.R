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
#Para el punto b. se hará un modelo segmentado, así que se crea la variable de 
#una vez.

Quiebre = 45 #En verdad es arbitrario este número
DB = DB %>% mutate(age_segmentada = age - Quiebre) %>% 
  mutate(indicadora = ifelse(age>=Quiebre, 1, 0))

#Se divide la muestra entre train y validación.
Train_index = createDataPartition(y = DB$log_ingresos_porhora, p = 0.7, 
                                  list = F)

DB_train = DB[Train_index,]
DB_test = DB[-Train_index,]

#Se reestiman los modelos con la base de entrenamiento.
lm_age = lm(log_ingresos_porhora ~ age+age_2, DB_train)
lm_sex = lm(log_ingresos_porhora ~ sex, DB_train)
lm_sex_multiple = lm(log_ingresos_porhora ~ sex+age+age_2 + maxEducLevel + 
                       sizeFirm + oficio, data = DB_train) #Acá se agrega oficio.
lm_sex_interaccion= lm(log_ingresos_porhora ~ sex+age+age_2 + age*sex + maxEducLevel + 
               sizeFirm + oficio, data = DB_train)



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
RMSE_sex_interaccion = RMSE(prediccion_sex_interaccion, 
                                      DB_test$log_ingresos_porhora)

#Se plantean otros 5 modelos.
lm_completo = lm(log_ingresos_porhora ~ sex + age+ 
                   hoursWorkUsual + as.factor(Estrato) + Independiente +
                   antiguedad_puesto + formal + sizeFirm + maxEducLevel + oficio,
                    data = DB_train)
lm_log_log = lm(log_ingresos_porhora ~ sex + log(age) + 
                  log(hoursWorkUsual) + as.factor(Estrato) + Independiente +
                  antiguedad_puesto + formal + sizeFirm + maxEducLevel + oficio,
                data = DB_train)
lm_polinomios = lm(log_ingresos_porhora ~ sex + poly(age, 4, raw = T) + 
                     poly(hoursWorkUsual, 4, raw = T) + as.factor(Estrato) + Independiente +
                     poly(antiguedad_puesto, 5, raw = T) + formal + sizeFirm + maxEducLevel +
                     oficio, data = DB_train)

lm_discriminador = lm(log_ingresos_porhora ~ sex + age*sex + 
                        hoursWorkUsual*sex + as.factor(Estrato) + Independiente*sex +
                        antiguedad_puesto + formal + sizeFirm*sex + maxEducLevel*sex +
                        oficio, data = DB_train)

#Por último, se corre una regresión por segmentos en la edad.
lm_quiebre = lm(log_ingresos_porhora ~ sex + age + age_segmentada*indicadora + 
                  hoursWorkUsual + as.factor(Estrato) + Independiente +
                  antiguedad_puesto + formal + sizeFirm + maxEducLevel + 
                  oficio, data = DB_train)

#Predicciones y errores estándar.
prediccion_completo = predict(lm_completo, newdata = DB_test)
RMSE_completo = RMSE(prediccion_completo, DB_test$log_ingresos_porhora)

prediccion_log_log = predict(lm_log_log, newdata = DB_test)
RMSE_log_log = RMSE(prediccion_log_log, DB_test$log_ingresos_porhora)

prediccion_polinomios = predict(lm_polinomios, newdata = DB_test)
RMSE_polinomios = RMSE(prediccion_polinomios, DB_test$log_ingresos_porhora)

prediccion_discriminador = predict(lm_discriminador, newdata = DB_test)
RMSE_discriminador = RMSE(prediccion_discriminador, DB_test$log_ingresos_porhora)

prediccion_quiebre = predict(lm_quiebre, newdata = DB_test)
RMSE_quiebre = RMSE(prediccion_quiebre, DB_test$log_ingresos_porhora)

#Todo en una tabla para latex.
Modelos = data.frame("Modelo" = c("age", "sex", "sex_multiple", "sex_interacción",
                                  "lm_completo", "log log", "Polinomios", 
                                  "discriminador", "quiebre"),
                     "RMSE" = c(RMSE_age, RMSE_sex, RMSE_sex_multiple, 
                                RMSE_sex_interaccion, RMSE_completo, RMSE_log_log,
                                RMSE_polinomios, RMSE_discriminador, RMSE_quiebre))
xtable::xtable(Modelos)
saveRDS(Modelos, paste0(path,"Stores/Resultados_modelos_punto5.rds"))


# Punto d. ----------------------------------------------------------------
#Los mejores dos modelos: polinomio y quiebre.

#Se específica el método de resampleo.
ctrl = trainControl("LOOCV")

#Se realiza el proceso de LOOCV:
CV_completo = train(log_ingresos_porhora ~ sex + age + age_segmentada*indicadora + 
                      hoursWorkUsual + as.factor(Estrato) + Independiente +
                      antiguedad_puesto + formal + sizeFirm + maxEducLevel + 
                      oficio, 
      data = DB[1:nrow(DB), ], method = "lm", trControl = ctrl)

#Para correr los polinomios se crean las transformaciones de las variables.
DB = DB %>% 
  mutate(age_3 = age^3) %>%
  mutate(age_4 = age^4) %>%
  mutate(hoursWorkUsual_2 = hoursWorkUsual^2) %>%
  mutate(hoursWorkUsual_3 = hoursWorkUsual^3) %>%
  mutate(hoursWorkUsual_4 = hoursWorkUsual^4) %>%
  mutate(antiguedad_puesto_2 = antiguedad_puesto^2) %>%
  mutate(antiguedad_puesto_3= antiguedad_puesto^3) %>%
  mutate(antiguedad_puesto_4 = antiguedad_puesto^4) %>%
  mutate(antiguedad_puesto_5 = antiguedad_puesto^5) 

CV_polinomios = train(log_ingresos_porhora ~ sex + age + age_2 + age_3 + age_4 +
                        hoursWorkUsual + hoursWorkUsual_2 + hoursWorkUsual_3  +
                      hoursWorkUsual_4 + as.factor(Estrato) + Independiente + 
                        antiguedad_puesto + antiguedad_puesto_2 + 
                        antiguedad_puesto_3 + antiguedad_puesto_4 +
                        antiguedad_puesto_5 + formal + sizeFirm + maxEducLevel +
                        oficio, 
                      data = DB[1:nrow(DB), ], method = "lm", trControl = ctrl)
#Se extraen los RMSE de los CV.
Mejores_modelos = data.frame("Nombres" = c("Lm quiebre", "Polinomios"),
                             "RMSE validación" = c(RMSE_completo, 
                                                   RMSE_polinomios),
                             "RMSE LOOCV" = c(RMSE(CV_completo$pred$pred, 
                                                   DB$log_ingresos_porhora),
                                              RMSE(CV_polinomios$pred$pred,
                                                   DB$log_ingresos_porhora)))
xtable::xtable(Mejores_modelos)
saveRDS(Mejores_modelos, paste0(path,"Stores/LOOCV_punto5.rds"))













