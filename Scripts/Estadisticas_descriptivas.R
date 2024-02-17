###
#
# Programación problem set 1
# Estadísticas descriptivas.
# Integrantes:
# Sebastian Franco Torres
#
###
# Plan de acción: 
# A las variables continuas sacales las estadísticas de siempre: N, media, sd, percentiles.
# A las categóricas hace un gráfico de barras para ver el número de datos por categoría.
# Si hay continuas con valores atípicos, hace un histograma para analizarlos mejor.

# Preparación del ambiente ------------------------------------------------
rm(list=ls())

libraries = c("ggplot2", "tidyverse", "skimr", "stargazer", "gridExtra", "ggpubr") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

#Se importa la base.
DB = readRDS(paste0(path,"Stores/Base_final.rds"))


# Análisis de missing values. ---------------------------------------------
#Tabla con total de missings y proporción al total de datos.

Missings = skim(DB[,-(1:3)]) %>% select(skim_variable, n_missing)
Missings$proporcion = round((Missings$n_missing/dim(DB)[1])*100,2)

#En Latex.
stargazer(Missings, type = "latex", title = "Valores faltantes",
          label = "Tabla_missings", summary = FALSE)

#En un correlograma.
DB_aux = DB %>% 
  mutate_all(~ifelse(is.na(.), 1, 0))
DB_aux = DB_aux %>% select(which(apply(DB_aux, 2, sd) > 0))

png(filename = paste0(path, "Views/Missings_corr.png"),
    width = 1464, height = 750)
corrplot::corrplot(cor(DB_aux), type = "lower")
dev.off()


# Estadísticas vars continuas ---------------------------------------------
#Las variables continuas son.
continuas = c("Ingresos_porhora", "hoursWorkUsual", "antiguedad_puesto", "age")

DB_continuas = DB[,colnames(DB) %in% continuas]
  
#Se calculan distintas estadísticas:
#Número de observaciones #que no son missing
Observations = apply(DB_continuas, MARGIN = 2, function(x){
  Non_missings = sum(!is.na(x))
  return(Non_missings)
}) 

#Media
mean_aux = round(apply(DB_continuas, MARGIN = 2, mean, na.rm = TRUE),2)

#SD
sd_aux = round(apply(DB_continuas, MARGIN = 2, sd, na.rm = TRUE),2)

#Percentiles.
percentiles = t(round(apply(DB_continuas, MARGIN = 2, quantile, na.rm = TRUE,
                           probs = c(c(0.05, 0.25, 0.5, 0.75, 0.95))),2))

#En una matriz
Estadisticas_continuas = cbind(Observations, mean_aux, sd_aux, percentiles)
colnames(Estadisticas_continuas) = c("No. Observaciones", "Media", "Desv. Estándar", 
                                 "Per. 5", "Per. 25", "Per. 50", "Per. 75", 
                                 "Per 95")
#Hacia latex
stargazer(Estadisticas_continuas, type = "latex", title = "Estadísticas descriptivas",
          subtitle = "variables continuas", label = "Tabla_continuas",
          summary = FALSE)

#Histogramas de variables continuas.
#Antiguedad
png(filename = paste0(path, "Views/Hist_antiguedad.png"),
    width = 1464, height = 750)
antiguedad_puesto = ggplot(DB, aes(x=antiguedad_puesto)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black", alpha = 0.8) +
  scale_x_continuous(n.breaks = 10, expand = c(0,0)) +
  scale_y_continuous(n.breaks = 10, expand = c(0,0)) + 
  labs(y = "Frecuencia",
       x = "No. de meses") +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black", size = 1))
print(antiguedad_puesto)
dev.off()

#Ingreso
#Con toda la muestra.
Ingresos_total = ggplot(DB, aes(x=Ingresos_porhora)) +
  geom_histogram(fill = "skyblue", color = "black", alpha = 0.8) +
  scale_x_continuous(n.breaks = 10, expand = c(0,0)) +
  scale_y_continuous(n.breaks = 10, expand = c(0,0)) + 
  labs(subtitle = "Muestra completa",
       y = "Frecuencia",
       x = "Ingreso laboral") +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black", size = 1))

Ingresos_menor_perc95 = ggplot(DB %>% 
                                 filter(Ingresos_porhora <= percentiles["Ingresos_porhora","95%"]),
                               aes(x=Ingresos_porhora)) +
  geom_histogram(fill = "skyblue", color = "black", alpha = 0.8) +
  scale_x_continuous(n.breaks = 10, expand = c(0,0)) +
  scale_y_continuous(n.breaks = 10, expand = c(0,0)) + 
  labs(subtitle = "Menor a percentil 95",
       y = "Frecuencia",
       x = "Ingreso laboral") +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black", size = 1))

Ingresos_mayor_perc95 = ggplot(DB %>% 
                                 filter(Ingresos_porhora > percentiles["Ingresos_porhora","95%"]),
                               aes(x=Ingresos_porhora)) +
  geom_histogram(fill = "skyblue", color = "black", alpha = 0.8) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10, expand = c(0,0)) + 
  labs(subtitle = "Mayor a percentil 95",
       y = "Frecuencia",
       x = "Ingreso laboral") +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 15),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(), 
        axis.line = element_line(color = "black", size = 1)) 


#En uno solo
png(filename = paste0(path, "Views/Hist_ingreso.png"),
    width = 1464, height = 750)
ggarrange(Ingresos_total, 
          ggarrange(Ingresos_menor_perc95, Ingresos_mayor_perc95,
                    ncol = 2, align = "h", widths = c(1, 1)),
          nrow = 2, heights = c(1, 1))
dev.off()


# Estadísticas discretas --------------------------------------------------
#Se seleccionan las variables discretas.
Discretas = c("sex", "Estrato", "Independiente", "Urbano", "formal", "sizeFirm",
              "maxEducLevel" )

DB_discretas = DB[,colnames(DB) %in% Discretas]

#Se hace un gráfico de barras por cada var discreta.
for (i in colnames(DB_discretas)){
  
  png(filename = paste0(path, "Views/Hist_", i, ".png"),
      width = 1464, height = 750)
  aux = ggplot(DB_discretas, aes(x = as.factor(!!sym(i)))) +
    geom_bar(fill = "skyblue", color = "black", alpha = 0.8, 
             width = 0.5) +
    geom_text(stat = "count", aes(label = after_stat(count)), 
              vjust = -0.5) +
    labs(title = paste0("Distribución variable ", i), 
         y = "Conteo") +
    theme(plot.title = element_text(hjust = 0.5, size = 15),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),  
          panel.grid.minor = element_blank(), 
          axis.line = element_line(color = "black", size = 1)) 
  print(aux)
  dev.off()
}






























