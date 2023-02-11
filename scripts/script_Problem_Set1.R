#---
# Titulo: "Big Data y Machine Learning"
# Subtitulo: Problem Set 1
# Autores:
          # Diana Higuera
          # Juan Sebastian Vallejo
          # Riky Andres Carrillo Cadena - 202027540
          # Lizbeth Hernandez
# Fecha: '2023-02-12'
# output:
# html_document: default
#---


#---Configuración inicial

#install.packages("pacman") # activar solo una vez en el pc
library(pacman)

## llamar la librería pacman: contiene la función p_load()
require(pacman)
require(dplyr)

## El comadno "p_load" permite instalar/cargar las librerías que se enlistan:
p_load(tidyverse, # contiene las librerías ggplot, dplyr...
       rvest, # web-scraping
       stargazer)

#---Paso 1. Descargar base de datos de la GEIH 2018-Bogotá uasndo web-scraping
  # Aqui vamos a leer el html de la página web donde estan los datos
  geih2018_html<-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")
  
  # Ver la clase de objeto
  class(geih2018_html)
  View(geih2018_html)

  #Extraer tablas
  geih2018_html%>%
  html_table()
  
  # Mostrar el número de tablas extraidas
  length(geih2018_html)
  
  # Guardarla como data.frame
  geih2018<-geih2018_html%>%
    html_table()%>%
    as.data.frame()
  
  # Vamos hacer un loop que replique el paso anterior para las 10 pag
  
  url<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
  data<-data.frame()
  
  #Loop
  for (i in 1:10){
    url_i<-paste0(url,i,".html")
    tablas<-url_i%>%
      read_html()%>%
      html_table() %>% .[[1]]
      data<-rbind.data.frame(data,tablas)
  }
  
  # Eliminar primera columna de la data
  db<-data[,-1]
  
  # Conocer la ruta del directorio de trabajo
  getwd()
  
  # Elegir el directorio 
  setwd("C:/Users/andre/OneDrive/Github/Repositorios/Big_Data_Problem_Set1/data")
  
  #Guardar data
  write.table(db, "db_geih2018.txt", sep = "\t", quote = F, row.names = F)
  
  
#---Limpieza de la base de datos 
  
  # Aqui vamos a visualizar una parte de la base de datos
  db
  
  # Ahora vamos a conocer la dimension de la base de datos
  dim(db)
  
  # Podemos ver que tenemos una matriz de 32177 (filas) x 177 (columnas), es decir,
  # tenemos 32.177observaciones y 177 variables
  
  # Ahora vamos a visualizar una parte tanto de las primeras como de las últimas 
  # filas de la base de datos
  
  #Primeras filas
  head(db)
  
  #Últimas filas
  tail(db)
  
  #Vamos a explorar el tipo de variable de la base de datos
  glimpse(db)
  
  #Vamos a mirar si hay valores perdidos
  is.na(db_geih2018)
  
  #Queremos conocer cuantos valores perdidos hay 
  sum(is.na(db))
  # En nuestra base de datos hay 3.421.720
  
  # Vamos a seleccionar las variables de interes 
  geih18<-db %>%
      select(y_salary_m_hu,age,sex)
  
  #Valores perdido en variables especificas 
  sum(is.na(geih18$y_salary_m_hu))
  
  glimpse(geih18)
  

  

