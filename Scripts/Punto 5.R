###
#
# Programación problem set 1
# Punto 5.
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