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

## El comadno "p_load" permite instalar/cargar las librerías que se enlistan:
p_load(tidyverse, # contiene las librerías ggplot, dplyr...
       rvest) # web-scraping

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
  
  