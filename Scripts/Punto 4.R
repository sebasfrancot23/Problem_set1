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


libraries = c("ggplot2", "tidyverse", "stats", "stargazer") 

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
lm_sex =lm(log_ingresos_porhora ~ sex, DB)
stargazer(lm_sex, type = "text")

#Modelo más completo.



















