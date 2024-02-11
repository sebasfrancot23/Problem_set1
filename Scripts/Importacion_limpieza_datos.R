###################################
#
# Programación problem set 1
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
dim(Tabla) #Está vacío. Pero dentro del HTML de estas páginas, en networkm se encuentra la dirección de otro
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
aux = lista_aux[["chunk_1"]]

#Se filtra la base para individuos mayores a 18 años y empleados.
aux = aux %>%
  filter(age>=18 & dsi == 0) #dsi indica si está empleado (0) o desempleado (1)

























