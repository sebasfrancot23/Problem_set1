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
length(predict(lm_age, newdata = DB)) 

length(predict(lm_age, newdata = DB[!is.na(DB$Ingresos_porhora),])) 

























