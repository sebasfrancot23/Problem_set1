###
#
# Programación problem set 1
# Punto 3.
# Integrantes:
# Sebastian Franco Torres
#
###


# Preparación del ambiente ------------------------------------------------
rm(list=ls())

libraries = c("ggplot2", "tidyverse", "stats", "stargazer") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

#Se importa la base.
DB = readRDS(paste0(path,"Stores/Base_final.rds"))


# Análisis gráfico. ------------------------------------------------------
#Se crea la variable de edad al cuadrado.
DB = DB %>% mutate(age_2 = age^2)

#Se hace un scatterplot para determinar la relación entre w y la edad.
#Con todos los datos.
W_full = ggplot(DB, aes(x = age, y = Ingresos_porhora/1000)) +
  geom_point(color = "blue") +  
  labs(y = "Ingreso por hora", x = "Edad", caption = "Cifras en miles de pesos.") +
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
#Hasta el percentil 75.
perc_75 = quantile(DB$Ingresos_porhora, na.rm = T)[4]

W_75 = ggplot(DB %>% filter(Ingresos_porhora<=perc_75), aes(x = age, y = Ingresos_porhora)) +
  geom_point(color = "blue") +  
  labs(y = "Ingreso por hora", x = "Edad") +
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

#En un solo gráfico:
png(filename = paste0(path, "Views/Scatter_age_ingresos.png"),
    width = 1464, height = 750)
ggpubr::ggarrange(W_full, W_75, ncol = 2, 
                  ggtitle = "Relación entre Edad e Ingresos_porhora") 
dev.off()


# Estimación modelo. ------------------------------------------------------
lm_age = lm(Ingresos_porhora ~ age+age_2, DB)
stargazer(lm_age, type = "text") #Se exportan los resultados.

#Se calcula el MSE del modelo.
DB_aux = DB[!is.na(DB$Ingresos_porhora),]
predicciones = predict(lm_age, newdata = DB_aux)

MSE = mean((DB_aux$Ingresos_porhora - predicciones))^2 

#En una matriz.
aux = summary(lm_age)
Errores = data.frame(R_2 = aux$adj.r.squared,
                 MSE = MSE,
                 RMSE = sqrt(MSE)) #Saquemos el root de paso.
#Hacia latex.
stargazer(Errores, type = "text", title = "Medidas de ajuste", 
          subtitle = "dentro de muestra")

#Las predicciones de forma gráfica.
DB_predict = data.frame(age = DB_aux$age,
                    Ingresos_porhora = DB_aux$Ingresos_porhora,
                    Predicciones = predicciones)

#El valor de edad que máxima el salario.
Age_max = unique(DB_predict[DB_predict$Predicciones == max(DB_predict$Predicciones), "age"])

#El gráfico
png(filename = paste0(path, "Views/Scatter_age_ingresos_prediccion.png"),
    width = 1464, height = 750)
ggplot(DB_predict, aes(x = age, y = Ingresos_porhora/1000)) +
  geom_point(color = "blue") +  
  geom_line(aes(y = predicciones/1000), size = 2) + 
  geom_vline(xintercept = Age_max, linetype = "dashed", color = "red") +
  labs(y = "Ingreso por hora", x = "Edad", caption = "Cifras en miles de pesos.") +
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

# Efecto edad sobre ingresos ----------------------------------------------
#Se define una función auxiliar del efecto marginal de la edad sobre los ingresos.
MG_edad = function(Data, lm_object){
  Efecto =  lm_object[["coefficients"]][2] + 2*lm_object[["coefficients"]][3]*Data
  return(Efecto)
}

#Se evalúa la función: 
DB_aux$Efecto_marginal = MG_edad(DB_aux$age, lm_age)

#El gráfico:
png(filename = paste0(path, "Views/Efecto_edad_ingresos.png"),
    width = 1464, height = 750)
ggplot(DB_aux, aes(x = age, y = Efecto_marginal)) +
  geom_line(size = 2, color = "skyblue", fill = "black") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = Age_max+0.6, linetype = "dashed", color = "red") +
  geom_text(aes(label = ifelse(age == Age_max, paste0(as.character(Age_max), " años")
                               , "")), vjust = -0.3, hjust = -0.2, color = "black") +
  labs(title = "Efecto marginal de la edad sobre Ingresos_porhora" ,
       y = "Efecto marginal", x = "Edad") +
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























