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
set.seed(39075)

libraries = c("ggplot2", "tidyverse", "stats", "stargazer", "boot") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

#Se importa la base.
DB = readRDS(paste0(path,"Stores/Base_final.rds"))


# Análisis gráfico. ------------------------------------------------------


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
ggpubr::ggarrange(W_full, W_75, ncol = 2)  
dev.off()


# Estimación modelo. ------------------------------------------------------
#Se calcula el logaritmo natural del salario.
DB$log_ingresos_porhora = log(DB$Ingresos_porhora)

#Se crea la variable de edad al cuadrado.
DB = DB %>% mutate(age_2 = age^2)

lm_age = lm(log_ingresos_porhora ~ age+age_2, DB)
stargazer(lm_age, type = "text") #Se exportan los resultados.

#Se calcula el MSE del modelo.
DB_aux = DB[!is.na(DB$Ingresos_porhora),]
predicciones = predict(lm_age, newdata = DB_aux) |> exp() #En niveles.  
 

MSE = mean((DB_aux$Ingresos_porhora - predicciones))^2 

#En una matriz.
aux = summary(lm_age)
Errores = data.frame(Nombres = c("R cuadrado ajustado", "MSE", "RMSE"),
                     Estadistico = c(aux$adj.r.squared, MSE, sqrt(MSE)))
#Hacia latex.
stargazer(t(Errores), type = "text", title = "Medidas de ajuste", 
          subtitle = "dentro de muestra", keep = c(1,3), 
          column.labels = c("Estadístico", "Valor"),
          align = T)

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
    Efecto = lm_object[["coefficients"]][2] + 2*lm_object[["coefficients"]][3]*Data
  return(Efecto)
}

#Se evalúa la función: 
DB_aux$Efecto_marginal = MG_edad(DB_aux$age, lm_age)

#El gráfico:
png(filename = paste0(path, "Views/Efecto_edad_ingresos.png"),
    width = 1464, height = 750)
ggplot(DB_aux, aes(x = age, y = Efecto_marginal)) +
  geom_line(size = 1, color = "blue") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = Age_max+0.6, linetype = "dashed", color = "red") +
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
dev.off()


# Análisis de Bootstrap ---------------------------------------------------
#Si se encuentra el punto que máximiza el logaritmo del salario, también se encuentra
#el punto que máxima el salario, puesto que exp(f(x)) es una transformación monotónica.

#Se define la función que realiza la optimización del salario con respecto a la edad.
Max_find = function(lm_object, data){
  aux = uniroot(MG_edad, interval = c(min(data$age), max(data$age)), 
                lm_object = lm_object)
  return(aux[["root"]])
}

#La función para emplear en bootstrap.
Function_to_boot = function(data, index){
  #Se corre el modelo y se guardan los coeficientes.
  lm_aux = (lm(log_ingresos_porhora ~ age+age_2, 
                         data = data, subset = index))
  
  #Se emplea la función auxiliar para encontrar la raíz en la elasticidad.
  optimo = Max_find(lm_aux, data = data)
  
  return(optimo)

}

#Los resultados del bootstrap.
Bootstrap = boot(DB, Function_to_boot, R = 10)

#Se crea un histograma con la distribución empírica del estimador. 
Resultados_boot = data.frame("Simulación" = 1:Bootstrap$R,
           "Estimación" = Bootstrap$t)

#Para resaltar en el gráfico los intervalos de confianza.
Percentiles = quantile(Bootstrap$t, c(0.025, 0.0975))
names(Percentiles) = NULL
png(filename = paste0(path, "Views/Hist_bootstrap.png"),
    width = 1464, height = 750)
ggplot(Resultados_boot, aes(x=Estimación)) +
  geom_histogram(fill = "skyblue", color = "black", alpha = 0.8) +
  geom_vline(xintercept = Percentiles[1], linetype = "dashed", color = "red") +
  geom_vline(xintercept = Percentiles[2], linetype = "dashed", color = "red") +
  scale_x_continuous(n.breaks = 10, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) + 
  labs(y = "Frecuencia",
       x = "Valor estimador", caption = "Intervalo de confianza del 95% en rojo") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.caption = element_text(size = 14),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black", size = 1))
dev.off()

#En una tabla los resultados del bootstrap. 
Matriz_boot = data.frame("Media" = mean(Bootstrap$t),
                         "Error est." = sd(Bootstrap$t),
                         "Perc. 2.5" = Percentiles[1],
                         "Perc. 97.5" = Percentiles[2])

stargazer(c(mean(Bootstrap$t), sd(Bootstrap$t), Percentiles[1], Percentiles[2]),
          type = "latex", title = "Resultados Bootstrap", 
          column.labels = c("Media", "Error est.", "Perc. 2.5", "Perc. 97.5"),
          align = T)














