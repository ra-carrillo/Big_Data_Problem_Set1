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
  

  #--- 2. Limpieza de la base de datos
  
  ## Filtrar la base con ocupados mayores a 18 años
  geih1 <- geih18 %>% filter(dominio == 'BOGOTA' & age >= 18 & ocu == 1)
  ## Renombrar variables 
  geih1<-geih1 %>% rename(inglab=p6500)
  # Datos faltantes en la variable inglab
  sum(is.na(geih1$inglab))
  ## Imputación de valores faltantes, los datos faltantes de la variable inglab se reemplazan por los de ingresos total (ingtot)
  geih1 <- geih1 %>% 
    mutate(inglab = case_when(
      !is.na(ingtot) ~ ingtot,
      TRUE ~ inglab
    ))
  ## Crear la variable w, salario por hora, expresada como cifra entera en miles
  geih1<-geih1 %>% mutate(w=inglab %/%(hoursWorkUsual*4))
  sum(is.na(geih1$w))
  ##Seleccionar las variables con las que se trabajará
  geih2 <- geih1 %>% select(directorio, secuencia_p, orden, clase, estrato1, age, w, ingtot, maxEducLevel, ocu, sex)
  str(geih2)
  ## Convertir variables texto a factor
  y <- c("estrato1", "sex", "maxEducLevel")
  geih2[y] <- lapply(geih2[y], factor)
  
<<<<<<< HEAD
  
 
=======
  #---3. Estadística descriptiva
  ## Creación de una variables categórica para rangos de edad
  geih2= geih2 %>% mutate(
    cat_age = case_when(
      age <= 30~ '18-30',
      age > 30 & age <= 50 ~ '30-50',
      TRUE ~ '>50'
    )
  )
  ## Tabla por rangos de edad
  Tabla1 <- table(~ age + factor(sex) + factor(estrato1) +
                    factor(maxEducLevel) + hoursWorkUsual + inglab + w
                  | cat_age, 
                  data=geih2, overall="Total")
  
  
  #---4. Regresión 1_Age-wage profile
  geih2$age2 <- geih2$age*geih2$age
  reg1 <- lm(w ~ age + age2, data = geih2)
  summary(reg1)
  stargazer(reg1,type="text")
  #Bootstrap
  sample_coef_intercept <- NULL
  sample_coef_x1 <- NULL
  sample_erstd_x1 <- NULL
  sample_coef_x2 <- NULL
  sample_erstd_x2 <- NULL
  for (i in 1:1000) {
    sample_d = geih2[sample(1:nrow(geih2), 0.3*nrow(geih2), replace = TRUE), ]
    reg_boots <- lm(w ~ age + age2, data = sample_d)
    sample_coef_intercept <-
      c(sample_coef_intercept, reg_boots$coefficients[1])
    
    sample_coef_x1 <-
      c(sample_coef_x1, reg_boots$coefficients[2])
    
    sample_erstd_x1 <-
      c(sample_erstd_x1, coef(summary(reg_boots))[2, 2])
    
    sample_coef_x2 <-
      c(sample_coef_x2, reg_boots$coefficients[3])
    
    sample_erstd_x2 <-
      c(sample_erstd_x2, coef(summary(reg_boots))[3, 2])
  }
  
  coefs <- rbind(sample_coef_intercept, sample_coef_x1, sample_erstd_x1, 
                 sample_coef_x2, sample_erstd_x2)
  ## Combinación de los resultados en una tabla 
  means.boots = c(mean(sample_coef_intercept), mean(sample_coef_x1), 
                  mean(sample_coef_x2))
  erstd.boots = c(0,mean(sample_erstd_x1),mean(sample_erstd_x2))
  knitr::kable(round(
    cbind(
      sample = coef(summary(reg1))[, c(1,2)],
      bootstrap = means.boots,
      erstdBoots = erstd.boots),4), 
    "simple", caption = "Coefficients in different models")
>>>>>>> e9fb9cd43af031e07e8673d31bae2087d3d1ba02
  
  # Intervalos de confianza
  confint(reg1)
  a <-
    cbind(
      quantile(sample_coef_intercept, prob = 0.025),
      quantile(sample_coef_intercept, prob = 0.975))
  b <-
    cbind(quantile(sample_coef_x1, prob = 0.025),
          quantile(sample_coef_x1, prob = 0.975))
  c <-
    cbind(quantile(sample_coef_x2, prob = 0.025),
          quantile(sample_coef_x2, prob = 0.975))
  d <-
    round(cbind(
      sample = confint(reg1),
      boot = rbind(a, b, c)), 4)
  colnames(d) <- c("2.5 %", "97.5 %",
                   "2.5 %", "97.5 %")
  
  #---5. Regresión 2_The gender earnings GAP
  reg2 <- lm(w ~ sex, data = geih2)
  summary(reg2)
  stargazer(reg2,type="text")
  
