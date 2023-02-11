#-------------------------------------------------------------------------------
# Titulo: "Prediciendo el ingreso
# Subtitulo: Problem Set 1
# Curso: "Big Data y Machine Learning"
#-------------------------------------------------------------------------------
# Autores:
          # Andres Carrillo
          # Diana Higuera
          # Sebastian Vallejo
          # Lizbeth Hernandez

# Crado: 2023-02-08
# Última modificación: 2023-02-12
# # Descripción:Este código contiene información acerca de la obtención de los datos 
#               utilizando web-scraping, la limpieza de la base de datos , el 
#               analisis estadístico y las estimaciones de regresión del taller 1
#-------------------------------------------------------------------------------
# [Notas]: 
#   
# * Todos los datos iniciales se almacenan en /data
# * Todos los códigos se almacenan en / scripts
# * Todas las figuras ytablas se envían a /view
# --------------------------------------------------------------------------------

#---Limpiar el espacio de trabajo 

  rm(list=ls())

#---Configuración inicial : Instalar y llamar paquetes 
  
  install.packages("pacman") # activar solo una vez en el pc
  library(pacman)

  ## llamar la librería pacman: contiene la función p_load()
  require(pacman)

  ## El comandoo "p_load" permite instalar/cargar las librerías que se enlistan:
  p_load(tidyverse, # contiene las librerías ggplot, dplyr...
         rvest, # web-scraping
         stargazer,)

#---Paso 1. Descargar base de datos de la GEIH 2018-Bogotá uasndo web-scraping
  
  # *Nota: Aqui vamos a realizar el codigo para hacer web-scraping para una sola pag, 
  # posteriormente haremos un loop para el caso especifico del problem set que hay 
  # que extraer los datos de varias paginas 
  
  
  # Aqui vamos a leer el html de la página web donde estan los datos
  data1_html<-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")
  
  # Ver la clase de objeto
  class(data1_html)
  View(data1_html)

  #Extraer tablas
  data1_html%>%
  html_table()
  
  # Mostrar el número de tablas extraidas
  length(data1_html)
  
  # Guardarla como data.frame
  data1<-data1_html%>%
    html_table()%>%
    as.data.frame()
  
  # Vamos hacer un loop que replique el paso anterior para las 10 pag donde se 
  # encuentran los datos de la GEIH 2018 para Bogotá
  
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
  data<-data[,-1]
  
  # Conocer la ruta del directorio de trabajo
  getwd()
  
  # Elegir el directorio 
  setwd("C:/Users/andre/OneDrive/Github/Repositorios/Big_Data_Problem_Set1/data")
  
  #Guardar data
  write.table(data, "db_geih2018.txt", sep = "\t", quote = F, row.names = F)
  
  
#---Limpieza de la base de datos 
  
  # Visualización de una parte de la base de datos
  data
  
  # Dimensión de la base de datos
  dim(data)
  
  # Podemos ver que tenemos una matriz de 32177 (filas) x 177 (columnas), es decir,
  # tenemos 32.177observaciones y 177 variables
  
  # Ahora vamos a visualizar una parte tanto de las primeras como de las últimas 
  # filas de la base de datos
  
  #Primeras filas
  head(data)
  
  #Últimas filas
  tail(data)
  
  #Vamos a explorar el tipo de variable de la base de datos
  glimpse(data)
  
  #Vamos a mirar si hay valores perdidos
  is.na(data)
  
  #Queremos conocer cuantos valores perdidos hay 
  sum(is.na(data))
  # En nuestra base de datos hay 3.421.720
  
  # Vamos a seleccionar las variables de interes 
  geih18<-data %>%
      select(y_salary_m_hu,age,sex)
  
  #Valores perdido en variables especificas 
  sum(is.na(geih18$y_salary_m_hu))
  
  glimpse(geih18)
  
  geih1 <- geih2018 %>% filter(dominio == 'BOGOTA' & age >= 18 & ocu == 1)
  

  

