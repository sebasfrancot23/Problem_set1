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
DB = DB[!is.na(DB$Ingresos_porhora),]

lm_sex =lm(log_ingresos_porhora ~ sex, DB)
stargazer(lm_sex, type = "text")

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

















