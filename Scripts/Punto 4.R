###
#
# Programación problem set 1
# Punto 4.
# Integrantes:
# Sebastian Franco Torres
#
###


# Preparación del ambiente ------------------------------------------------
rm(list=ls())


libraries = c("ggplot2", "tidyverse", "stats", "stargazer", "boot") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

#Se importa la base.
DB = readRDS(paste0(path,"Stores/Base_final.rds"))



# Punto a. ----------------------------------------------------------------
#La variable sex está codificada como 1 si es hombre. El punto lo pide al revés, 
#se modifica la variable.
DB = DB %>% mutate(sex = ifelse(sex==0, 1, 0))
#Se calcula el logaritmo natural del salario.
DB$log_ingresos_porhora = log(DB$Ingresos_porhora)

#Primero un pequeño análisis gráfico
png(filename = paste0(path, "Views/Scatter_sex_ingresos.png"),
    width = 1464, height = 750)
ggplot(DB, aes(x = as.factor(sex), y = Ingresos_porhora/1000)) +
  geom_point(color = "blue") +  
  labs(y = "Ingreso por hora", x = "Sexo", caption = "Cifras en miles de pesos.") +
  scale_y_continuous(n.breaks = 6) +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.caption = element_text(size = 15),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black", size = 1))
dev.off()

#Se estima el modelo sencillo.
DB = DB[!is.na(DB$Ingresos_porhora),]

lm_sex =lm(log_ingresos_porhora ~ sex, DB)
stargazer(lm_sex, type = "text")


# Punto b. ----------------------------------------------------------------
#Modelo más completo.
#Se crea la edad al cuadrado.
DB = DB %>% mutate(age_2 = age^2)

#Se específica que las variables categóricas son de este tipo.
Vars_categoricas = c("sizeFirm", "maxEducLevel", "oficio")
for (i in Vars_categoricas){
  DB[[i]] = factor(DB[[i]])
}

#Primero sin aplicar FWL.
lm_sex_completo = lm(log_ingresos_porhora ~ sex+age+age_2 + maxEducLevel + 
                       oficio + sizeFirm, data = DB)
#Para comparar, se conserva el coeficiente de sex.
Efecto_sex = lm_sex_completo$coefficients[2]

#Ahora se aplica FWL.
#Se eliminan los efectos de las demás X sobre sex.
sex_aux = lm(sex ~ age+age_2 + maxEducLevel + 
               oficio + sizeFirm, data = DB)
#Sobre Y.
Ingreso_aux = lm(log_ingresos_porhora ~ age+age_2 + maxEducLevel + 
                   oficio + sizeFirm, data = DB)

#Se guardan los residuales de cada regresión auxiliar.
DB$Resid_sex_aux = sex_aux$residuals
DB$Resid_Ingreso_aux = Ingreso_aux$residuals

#Se realiza la segunda etapa de FWL.
FWL_sex = lm(Resid_Ingreso_aux ~ Resid_sex_aux, data = DB)
stargazer(FWL_sex, type = "text")

# Punto b, bootstrap ------------------------------------------------------
#Se define la función para emplear en bootstrap.
Function_to_boot = function(data, index){
  
  #Se eliminan los efectos de las demás X sobre sex.
  sex_aux = lm(sex ~ age+age_2 + maxEducLevel + 
                 oficio + sizeFirm, data = data, subset = index)
  #Sobre Y.
  Ingreso_aux = lm(log_ingresos_porhora ~ age+age_2 + maxEducLevel + 
                      oficio + sizeFirm, data = data, subset = index)
  
  #Se guardan los residuales de cada regresión auxiliar.
  data$Resid_sex_aux = sex_aux$residuals
  data$Resid_Ingreso_aux = Ingreso_aux$residuals
  
  ##Se realiza la segunda etapa de FWL.
  FWL_sex = coef(lm(Resid_Ingreso_aux ~ Resid_sex_aux, data = data,
                    subset = index))[2]
  return(FWL_sex)
}

#Los resultados del bootstrap.
Resultados_boot_sex = boot(DB, Function_to_boot, R = 10)

#Algunos resultados en una tabla.
resultados_FWL = data.frame(Método = c("Regresión múltiple", "FWL", 
                                       "FWL con bootstrap"),
                            Estimador_sex = c(round(Efecto_sex,4), 
                                              round(FWL_sex$coefficients[2],4),
                                              round(mean(Resultados_boot_sex$t),
                                                    4)),
                            Error_estandar = c(round(sqrt(diag(vcov(lm_sex_completo)))[2],3),
                                               round(sqrt(diag(vcov(FWL_sex)))[2],3), 
                                               round(sd(Resultados_boot_sex$t),3)))
#La diagonal de la matriz varcov es la varianza de los estimadores.
#En una tabla
xtable::xtable(resultados_FWL, type = "text")

# Punto c. ----------------------------------------------------------------
#Se añadirá una interacción entre edad y sex para determinar si la edad que máximiza
#el salario es diferente según el sexo del trabajador.

#La función de efecto marginal cambia según si es mujer o no.
MG_edad = function(Data, lm_object, female){
  
  #Si es mujer toca incorporar el nuevo coeficiente de sex*age.
  if (female){
  Efecto = lm_object[["coefficients"]]["age"] + 
    2*lm_object[["coefficients"]]["age_2"]*Data + lm_object[["coefficients"]]["sex:age"]
  } else {
    Efecto = lm_object[["coefficients"]]["age"] + 
      2*lm_object[["coefficients"]]["age_2"]*Data #No está el efecto de sex:age.
  }
  
  return(Efecto)
}

#Nuevamente, se define una función que realice el proceso de bootstrap.
Age_max_bootstap_female = function(data, index){
  
  #Se define la función encuentra la raíz en el polonimo del efecto marginal.
  Max_find = function(lm_object, data, female){
    aux = uniroot(MG_edad, interval = c(min(data$age), max(data$age)), 
                  lm_object = lm_object, female = female)
    return(aux[["root"]])
  }
  
  #Se corre el modelo con la submuestra del bootstrap.
  lm_aux = lm(log_ingresos_porhora ~ sex+age+age_2 + age*sex + maxEducLevel + 
                oficio + sizeFirm, data = data, subset = index)
  
  #Se emplea la función auxiliar para encontrar la raíz en la elasticidad.
  optimo = Max_find(lm_aux, data = data, female = T)
  
  return(optimo)
}
Age_max_bootstap_male = function(data, index){
  
  #Se define la función encuentra la raíz en el polonimo del efecto marginal.
  Max_find = function(lm_object, data, female){
    aux = uniroot(MG_edad, interval = c(min(data$age), max(data$age)), 
                  lm_object = lm_object, female = female)
    return(aux[["root"]])
  }
  
  #Se corre el modelo con la submuestra del bootstrap.
  lm_aux = lm(log_ingresos_porhora ~ sex+age+age_2 + age*sex + maxEducLevel + 
                oficio + sizeFirm, data = data, subset = index)
  
  #Se emplea la función auxiliar para encontrar la raíz en la elasticidad.
  optimo = Max_find(lm_aux, data = data, female = F)
  
  return(optimo)
}

#Se corre el bootstrap.
Boot_mujer = boot(DB, Age_max_bootstap_female, R = 10)
Boot_hombre = boot(DB, Age_max_bootstap_male, R = 10)

#Los intervalos de confianza al 95%.
IC_female = quantile(Boot_mujer$t, probs = c(0.025, 0.975))
IC_male = quantile(Boot_hombre$t, probs = c(0.025, 0.975))

#Cambio menor para el gráfico.
names(IC_female) = NULL
names(IC_male) = NULL 

#Se hace un gráfico de las predicciones, edad máxima e intervalos de confianza por sexo.

#Primero la predicción sin discriminar.
lm_aux = lm(log_ingresos_porhora ~ sex+age+age_2 + age*sex + maxEducLevel + 
              oficio + sizeFirm, data = DB)
DB$Prediccion_salario = predict(lm_aux, newdata = DB) |> exp()
  
#Se divide la muestra.
DB_female = filter(DB, sex==1)
DB_male = filter(DB, sex == 0)

#En donde está la edad que maximiza el salario. 
Age_max_male = unique(DB_male[DB_male$Pred == max(DB_male$Prediccion_salario), "age"])
Age_max_female = unique(DB_female[DB_female$Pred == max(DB_female$Prediccion_salario), "age"])

#El gráfico.
DB %>% arrange(Prediccion_salario) %>% 
  mutate(id =1:nrow(DB)) %>%
  ggplot(aes(x = age, y = Prediccion_salario, group = as.factor(sex), 
             color = as.factor(sex))) +
  geom_line()

  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = Age_max, linetype = "dashed", color = "red") +
  geom_text(aes(label = ifelse(age == Age_max, paste0(as.character(Age_max), " años")
                               , "")), vjust = -0.3, hjust = -0.2, color = "black") +
  labs(title = "Semielasticidad del ingreso contra la edad" ,
       y = "Semielasticidad", x = "Edad") +
  scale_y_continuous(n.breaks = 6, expand = c(0,0)) +
  scale_x_continuous(n.breaks = 6, expand = c(0,0)) +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.caption = element_text(size = 15),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black", size = 1))




































