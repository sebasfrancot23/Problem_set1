###################################
#
# Programación problem set 1
# Webscrapping y limpieza de datos.
# Integrantes:
# Sebastian Franco Torres
#
#######################


# Preparación del ambiente ------------------------------------------------
rm(list=ls())

libraries = c("rvest", "tidyverse", "skimr") 

if(length(setdiff(libraries, rownames(installed.packages()))) > 0){
  install.packages(setdiff(libraries, rownames(installed.packages())))
}

invisible(sapply(libraries, require, character.only = TRUE,quietly = TRUE))

#Directorio de trabajo
path = gsub("(.+)Scripts.+","\\1",rstudioapi::getActiveDocumentContext()$path)

# Importación base de datos -----------------------------------------------

#Fíjate en la dirección de cada chunk de la base de datos.
Tabla = "https://ignaciomsarmiento.github.io/GEIH2018_sample/page1.html" |> read_html() |> html_table()
dim(Tabla) #Está vacío. Pero dentro del HTML de estas páginas, en network se encuentra la dirección de otro
#html que sí contiene los datos.


#Se raspa la información de internet con un loop. 
chunks = 1 #Cuántos chunks se van a extraer.
lista_aux = list()

for (i in 1:chunks){
  
  #Se guarda la tabla en la página web.
  aux = paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", as.character(i)
                   , ".html") |> read_html() |> html_table() |> as.data.frame()
  
  lista_aux[[paste0("chunk_", as.character(i))]] = aux
}


# Limpieza base de datos --------------------------------------------------
#Función auxiliar para limpiar las bases de datos.
Limpieza_bases = function(x){
  
  aux = x %>%
    filter(age>=18 & ocu == 1) %>% #ocu indica si está empleado (1).
    rename(Ingresos_porhora = y_salary_m_hu, antiguedad_puesto = p6426, 
           Urbano = clase, Independiente = cuentaPropia, Estrato = estrato1) %>% 
    #Urbano es 1 si está en la ciudad.
    select(directorio, secuencia_p, dominio, sex, age, Ingresos_porhora,
           hoursWorkUsual, Estrato, Independiente, antiguedad_puesto,
           Urbano, formal, sizeFirm, maxEducLevel, oficio) %>%
    filter(!is.na(Ingresos_porhora))
  #Variables del modelo. formal indica si paga o no seguridad social.
    
    #Hay algunas categorías en oficio que solo tienen un dato. Más adelante eso 
    #será conflictivo al separar la muestra en validación y train. Por tanto,
    #los oficios con pocas observaciones se acumularán en una sola categoría.
    
    #Nos quedamos con las categorías que tienen 3 o menos observaciones
    filtro = table(aux[["oficio"]]) <= 3
    
    #Cuántas personas están en estos oficios.
    oficios_bajos = table(aux[["oficio"]])[filtro]
    
    nombres_oficios_bajos = names(oficios_bajos) |> as.numeric()
    
    #Si el oficio de la persona está en nombres_oficios_bajos, se cambia al
    #nuevo oficio.
    aux = aux %>% mutate(oficio = ifelse(oficio %in% nombres_oficios_bajos,
                                         0, oficio))
    
    
    
  return(aux)
}


#Se aplica la función a cada base de datos.
j = 1
for (i in names(lista_aux)){
  
  if (j==1){
    #Se aplica la función.
    Base_limpia = Limpieza_bases(lista_aux[[i]])
  } else {
    #Se unen las bases de datos.
    Base_limpia = bind_rows(Base_limpia, Limpieza_bases(lista_aux[[i]]))
  } 
  
  j = j+1
}

# Imputación missings values ----------------------------------------------
#Los missings values no se imputan, se eliminan.
#¿Por qué? Todos los missings en las X son también missing en Y. Si queremos usar esas observaciones
#deben tener valores en ambas variables. Pero imputar tanto X como Y es lo mismo que forzar 
#la relación Y = f(x), así que las estimaciones no capturarían la verdadera relación causal.

#Excepto por este muchacho en maxEducLevel.
Base_limpia = Base_limpia %>% mutate(maxEducLevel = 
                                       ifelse(is.na(maxEducLevel), 
                                              round(mean(maxEducLevel, na.rm = T),0), 
                                              maxEducLevel))

#Se exporta la base lista para el modelo:
saveRDS(Base_limpia, file = paste0(path, "Stores/Base_final.rds"))





 

























