###
#
# Programación problem set 1
# Punto 4.
# Integrantes:
# Sebastian Franco Torres
#
###


# Preparación del ambiente ------------------------------------------------
rm(list=setdiff(ls(), c("lm_age")))


rm(list = setdiff(ls(), c("Lineal", "GraficoB", "Graficas_macro", "Fun_Ban", 
                          "Grafico_T", "Fun_FED", "Formato_TRM", "Grafico", 
                          "Fun_Bancos", "Grafica_Bancos", "FED_tasas", 
                          "Grafica_probas", "T_graph", "Spread", "Graficos_B")))

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
DB = DB[!is.na(DB$Ingresos_porhora), ]


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

stargazer(lm_sex)

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
                       sizeFirm, data = DB)

#Ahora se aplica FWL.
#Se eliminan los efectos de las demás X sobre sex.
sex_aux = lm(sex ~ age+age_2 + maxEducLevel + 
               sizeFirm, data = DB)
#Sobre Y.
Ingreso_aux = lm(log_ingresos_porhora ~ age+age_2 + maxEducLevel + 
                   sizeFirm, data = DB)

#Se guardan los residuales de cada regresión auxiliar.
DB$Resid_sex = sex_aux$residuals
DB$Resid_Ingreso_aux = Ingreso_aux$residuals

#Se realiza la segunda etapa de FWL.
FWL_sex = lm(Resid_Ingreso_aux ~ Resid_sex, data = DB)

#Hacia latex
stargazer(lm_sex, FWL_sex, type = "latex", omit = c(FALSE,TRUE))

# Punto b, bootstrap ------------------------------------------------------
#Se define la función para emplear en bootstrap.
Function_to_boot = function(data, index){
  
  #Se eliminan los efectos de las demás X sobre sex.
  sex_aux = lm(sex ~ age+age_2 + maxEducLevel + 
                 sizeFirm, data = data, subset = index)
  #Sobre Y.
  Ingreso_aux = lm(log_ingresos_porhora ~ age+age_2 + maxEducLevel + 
                       sizeFirm, data = data, subset = index)
  
  #Se guardan los residuales de cada regresión auxiliar.
  data$Resid_sex_aux = sex_aux$residuals
  data$Resid_Ingreso_aux = Ingreso_aux$residuals
  
  ##Se realiza la segunda etapa de FWL.
  FWL_sex = coef(lm(Resid_Ingreso_aux ~ Resid_sex_aux, data = data,
                    subset = index))[2]
  return(FWL_sex)
}

#Los resultados del bootstrap.
Resultados_boot_sex = boot(DB, Function_to_boot, R = 100)

#Algunos resultados en una tabla.
resultados_FWL = data.frame(Método = c("Regresión múltiple", "FWL", 
                                       "FWL con bootstrap"),
                            Estimador_sex = c(round(lm_sex_completo$coefficients[2],4), 
                                              round(FWL_sex$coefficients[2],4),
                                              round(mean(Resultados_boot_sex$t),
                                                    4)),
                            Error_estandar = c(round(sqrt(diag(vcov(lm_sex_completo)))[2],3),
                                               round(sqrt(diag(vcov(FWL_sex)))[2],3), 
                                               round(sd(Resultados_boot_sex$t),3)))
#La diagonal de la matriz varcov es la varianza de los estimadores.
#En una tabla
xtable::xtable(resultados_FWL, type = "text")

saveRDS(resultados_FWL, paste0(path,"Stores/FWL_bootstrap_punto_4.rds"))

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

#Se define la función encuentra la raíz en el polonimo del efecto marginal.
Max_find = function(lm_object, data, female){
  aux = uniroot(MG_edad, interval = c(min(data$age), max(data$age)), 
                lm_object = lm_object, female = female)
  return(aux[["root"]])
}

#Nuevamente, se define una función que realice el proceso de bootstrap.
Age_max_bootstap_female = function(data, index){

  #Se corre el modelo con la submuestra del bootstrap.
  lm_aux = lm(log_ingresos_porhora ~ sex+age+age_2 + age*sex + maxEducLevel + 
               sizeFirm, data = data, subset = index)
  
  #Se emplea la función auxiliar para encontrar la raíz en la elasticidad.
  optimo = Max_find(lm_aux, data = data, female = T)
  
  return(optimo)
}
Age_max_bootstap_male = function(data, index){
  
  #Se corre el modelo con la submuestra del bootstrap.
  lm_aux = lm(log_ingresos_porhora ~ sex+age+age_2 + age*sex + maxEducLevel + 
                sizeFirm, data = data, subset = index)
  
  #Se emplea la función auxiliar para encontrar la raíz en la elasticidad.
  optimo = Max_find(lm_aux, data = data, female = F)
  
  return(optimo)
}

#Se corre el bootstrap.
Boot_mujer = boot(DB, Age_max_bootstap_female, R = 100)
Boot_hombre = boot(DB, Age_max_bootstap_male, R = 100)

#Los intervalos de confianza al 95%.
IC_female = quantile(Boot_mujer$t, probs = c(0.025, 0.975))
IC_male = quantile(Boot_hombre$t, probs = c(0.025, 0.975))

#Cambio menor para el gráfico.
names(IC_female) = NULL
names(IC_male) = NULL 

#Se hace un gráfico de las predicciones, edad máxima e intervalos de confianza por sexo.
#Primero la predicción sin discriminar.
lm_aux = lm(log_ingresos_porhora ~ sex+age+age_2 + age*sex + as.factor(maxEducLevel) + 
               as.factor(sizeFirm), data = DB)

#este modelo solo tiene variables categóricas, la única continua es age.
#Se obtiene una sub base para calcular las modas.
DB_categoricas = DB[,c("sizeFirm", "maxEducLevel")]

#Se calcula la moda por variable
modas = apply(DB_categoricas, 2, function(x){
  aux = names(which.max(table(x))) |> as.numeric()
  return(aux)
})

#se extrae el coeficiente de la categoría que más se repite por variable 
#(la moda):
coeficientes = list()
for (i in 1:length(modas)){
  #Para buscar el coeficiente.
  nombre_aux = paste0("as.factor(", names(modas[i]),")", modas[i])
  
  coeficientes[[nombre_aux]] = lm_aux$coefficients[nombre_aux]
}

#Se define una función auxiliar para calcular el perfil edad-salario por género.
#Función auxiliar del salario con los coeficientes estimados.
salario = function(age, female, lm_object, coef){
    
    if(female==1){
      
      perfil_log = lm_object$coefficients["(Intercept)"] + lm_object$coefficients["sex"]+ 
        lm_object$coefficients["age"]*age + lm_object$coefficients["age_2"]*age^2 +
        lm_object$coefficients["sex:age"]*age + coef[[1]] + 
        coef[[2]] #+ coef[[3]]
      names(perfil_log) = NULL
      
    } else {
      
      perfil_log = lm_object$coefficients["(Intercept)"] + lm_object$coefficients["age"]*age + 
        lm_object$coefficients["age_2"]*age^2 + coef[[1]] + 
        coef[[2]] #+ coef[[3]]
      names(perfil_log) = NULL
      
    }
    return(exp(perfil_log))
}

#Se evalúa la función en cada observación.
for (i in 1:nrow(DB)) {
  
  DB[i, "perfil_w_edad"] = salario(DB[i,"age"], DB[i,"sex"] ,
                                   lm_aux, coeficientes)
  
}


#Para calcular la edad que maximice el salario por género se divide la muestra.
DB$Prediccion_salario = predict(lm_aux, newdata = DB) |> exp() 

#En donde está la edad que maximiza el salario. 
Age_max_male = Max_find(lm_aux, data = DB, female = F)
Age_max_female = Max_find(lm_aux, data = DB, female = T)

#El gráfico.
png(filename = paste0(path, "Views/Perfil_ingresos_edad.png"),
    width = 1464, height = 750)
ggplot(DB, aes(x = age, group = as.factor(sex), color = as.factor(sex))) +
  geom_line(aes(y = perfil_w_edad/1000), size = 1.5) + #Ahora el IC de los hombres
  geom_vline(xintercept = Age_max_male, linetype = "solid", color = "darkred") +
  geom_vline(xintercept = IC_male[1], linetype = "dashed", color = "darkred") +
  geom_vline(xintercept = IC_male[2], linetype = "dashed", color = "darkred") +
  #Ahora las mujeres
  geom_vline(xintercept = Age_max_female, linetype = "solid", color = "orange") +
  geom_vline(xintercept = IC_female[1], linetype = "dashed", color = "orange") +
  geom_vline(xintercept = IC_female[2], linetype = "dashed", color = "orange") +
  scale_color_manual(values = c("darkred","orange"), name = "Género", 
                     labels = c("Hombre", "Mujer")) +
  scale_x_continuous(n.breaks = 8) +
  labs(y = "Ingresos por hora", x = "Edad", caption = "Cifras en miles de pesos.") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.caption = element_text(size = 15),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black", size = 1),
        legend.text = element_text(size = 15))
dev.off()
  
#En una tabla esos resultados:
Resultados_boot = data.frame("Género" = c("Hombre", "Mujer"),
                             "Edad óptima" = c(Age_max_male, Age_max_female),
                             "IC. 95%" = c(paste0(round(IC_male[1],0), ", "
                                                  , round(IC_male[2],0)),
                                           paste0(round(IC_female[1],0), ", "
                                                  , round(IC_female[2],0))))
xtable::xtable(Resultados_boot) 
saveRDS(Resultados_boot, paste0(path,"Stores/Punto_4c_Bootstrap_.rds"))
                                         



























