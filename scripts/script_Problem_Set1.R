#-------------------------------------------------------------------------------
# Titulo: "Prediciendo el ingreso"
# Subtitulo: Problem Set 1
# Curso: "Big Data y Machine Learning"
#-------------------------------------------------------------------------------
# Autores:
          # Andres Carrillo
          # Diana Higuera
          # Sebastian Vallejo
          # Lizbeth Hernandez

# Creado: 2023-02-08
# Última modificación: 2023-02-12
# # Descripción:Este código contiene información acerca de la obtención de los datos 
#               utilizando web-scraping, la limpieza de la base de datos , el 
#               analisis estadístico y las estimaciones de regresión del taller 1
#-------------------------------------------------------------------------------
# [Notas]: 
#   
# * Todos los datos iniciales se almacenan en /data
# * Todos los códigos se almacenan en / scripts
# * Todas las figuras y tablas se envían a /view
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
         stargazer,
         ggplot2, 
         hrbrthemes,
         tidymodels) # Tiene las herramientas para crear modelos de Machine learning

#---1. Descargar base de datos de la GEIH 2018-Bogotá uasndo web-scraping ###################################################
  
  # *Nota: Aqui vamos a realizar el codigo para hacer web-scraping para una sola pag, 
  # posteriormente haremos un loop para el caso especifico del problem set que hay 
  # que extraer los datos de varias paginas 
  
  
  ## leer el html de la página web donde estan los datos
  
  data1_html<-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")
  
  ## Ver la clase de objeto
  
  class(data1_html)
  View(data1_html)

  ## Extraer tablas
  data1_html%>%
  html_table()
  
  ##  Mostrar el número de tablas extraidas
  length(data1_html)
  
  ##  Guardarla como data.frame
  data1<-data1_html%>%
    html_table()%>%
    as.data.frame()
  
  # Vamos hacer un loop que replique el paso anterior para las 10 pag donde se 
  # encuentran los datos de la GEIH 2018 para Bogotá
  
  url<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
  data<-data.frame()
  
  ## Loop
  for (i in 1:10){
    url_i<-paste0(url,i,".html")
    tablas<-url_i%>%
      read_html()%>%
      html_table() %>% .[[1]]
    data<-rbind.data.frame(data,tablas)
  }
  
  ##  Eliminar primera columna de la data
  data<-data[,-1]
  
  ## Conocer la ruta del directorio de trabajo
  getwd()
  
  ## Elegir el directorio 
  
  setwd("C:/Users/andre/OneDrive/Github/Repositorios/Big_Data_Problem_Set1/data")
  
  ## Guardar data
  write.table(data, "db_geih2018.txt", sep = "\t", quote = F, row.names = F)
  
  
#--- 2. Limpieza de la base de datos ###############################################################
  
  ## Visualización de una parte de la base de datos
  data
  
  ## Dimensión de la base de datos
  dim(data)
  
  # Podemos ver que tenemos una matriz de 32177 (filas) x 177 (columnas), es decir,
  # tenemos 32.177observaciones y 177 variables
  
  # Ahora vamos a visualizar una parte tanto de las primeras como de las últimas 
  # filas de la base de datos
  
  ## Primeras filas
  head(data)
  
  ## Últimas filas
  tail(data)
  
  ## Explorar el tipo de variable de la base de datos
  glimpse(data)
  
  ## Inspeccionar valores perdidos de la base de datos
  is.na(data)
  
  ## Cuantificar los valores perdidos de la base de datos
  
  sum(is.na(data))
  
  # En nuestra base de datos hay 3.421.720 datos faltantes
  
  ##### Revisiones aleatorias con anexos DANE #####
  
  # Población Total: 8.164.164
  
  data %>% 
    summarise(
      Total = sum(fex_dpto,
                  na.rm = TRUE)
    )
  
  # Ocupados: 4.153.890 se aproxima al promedio mensual de 2018
  
  data %>% 
    summarise(
      Total = sum(fex_dpto*ocu, 
                  na.rm = TRUE)
    )
  
  ## Filtrar la base con ocupados mayores a 18 años
  
  geih2018<- data %>% 
    filter(dominio == 'BOGOTA' & age >= 18 & ocu == 1)
  
  ##### Cálculo de la variable de ingreso laboral según la metodología de pobreza del DANE #####
  # Revisar documento metodológico carpeta docs
  
  geih2018 <- geih2018 %>% 
    rowwise() %>% 
    mutate( 
      Labor.Income.DANE = sum(impa, # ingreso primera actividad
                         impaes, # ingreso pa imputado por DANE
                         ie, # ingreso en especie
                         iees,# ingreso en especie imputado por DANE
                         isa, # ingreso por segunda actividad
                         isaes, # ingreso por sa imputado por DANE
                         na.rm = TRUE)
    )
  
  summary(geih2018$Labor.Income.DANE) # No contamos con NAs
  
  
  Zeros <- geih2018 %>% 
    filter(Labor.Income == 0) # 323 observaciones cuentan con ingresos igual a 0
  
  # Corresponde al 1% de la muestra
  
  ### Cálculo del Ingreso laboral sin variables imputadas por el DANE
  
  geih2018 <- geih2018 %>% 
    rowwise() %>% 
    mutate( 
      Labor.Income.Alt = sum(impa, # ingreso primera actividades
                         #ie, # ingreso en especie
                         isa, # ingreso por segunda actividad
                         na.rm = TRUE),
      Labor.Income.Alt = case_when(
        is.na(y_total_m) == TRUE ~ y_total_m,
        TRUE ~ Labor.Income.Test
      )
    )
  
  summary(geih2018$Labor.Income.Alt) 
  
  # En promedio, sin el ingreso en especie, coincide con la variable y_total_m
  
  summary(geih2018$y_total_m)
  
  # Cuenta con 1778 NAs, es decir, 10% de la muestra. 
  
  # Sigue siendo muy alto como para imputar
  
  # Sin embargo, se crea una variable adicional imputada por la mediana 
  
  ##### Manejo de NAs #####
  
  ### Imputación con la mediana (No se usa la media dado que está arrastrada hacia arriba por valores muy altos)
  
  Median = median(geih2018$y_total_m,
                  na.rm = TRUE)
  
  geih2018 <- geih2018 %>% 
    mutate(y_total_m_imputada = 
             case_when(
               is.na(y_total_m) == TRUE ~ Median,
               TRUE ~ y_total_m
             )
           )

  summary(geih2018$y_total_m)
  
  summary(geih2018$y_total_m_imputada) # Se mantiene una distribución original bastante similar
  
  summary(geih2018$Labor.Income.DANE) # Se mantiene una distribución original bastante similar
  
  # Manejaría y_total_m_imputada y Labor.Income.DANE para todos los cálculos de aquí en adelante

  ####### Renombrar variables (Yo borraria esta subsección) #####  
  

  
  geih2018<-geih2018 %>% 
    rename(inglab=p6500)
  
  ## Datos faltantes en la variable inglab
  
  sum(is.na(geih2018$inglab)) 
  
  ## Imputación de valores faltantes, los datos faltantes de la variable inglab 
  #  se reemplazan por los de ingresos total (ingtot)
  
  geih2018<- geih2018 %>% 
    mutate(inglab = case_when(
      !is.na(ingtot) ~ ingtot,
      TRUE ~ inglab
    ))
  
  ####### Creación variable de ingreso laboral por hora #####
  
  geih2018<-geih2018 %>% 
    mutate(w=inglab %/%(hoursWorkUsual*4)) 
  
  sum(is.na(geih2018$w))
  
 Zoom <- geih2018 %>% 
   select(hoursWorkUsual, hoursWorkActualSecondJob, totalHoursWorked)
  # revisando las variables de horas trabajadas de la base, deberíamos trabajar con total hours worked
 # incluye las work usual más de las del segundo trabajo

 summary(Zoom)
 
 # Por la distribucion se asume que son horas semanales trabajadas
 
 ### Se calcula el ingreso laboral por semana
 
 geih2018 <- geih2018 %>% 
   mutate(Weekly.Wage = round(y_total_m_imputada/4),
          Weekly.Wage.DANE = round(Labor.Income.DANE/4)
   )
 
 ### Se calcula el ingreso laboral por hora
 
 geih2018 <- geih2018 %>% 
   mutate(Hourly.Wage = round(Weekly.Wage/totalHoursWorked,1),
          Hourly.Wage.DANE = round(Weekly.Wage.DANE/totalHoursWorked,1)
          )
 
 Zoom <- geih2018 %>% 
   select(y_total_m_ha, 
          Hourly.Wage,
          Hourly.Wage.DANE)
 
 summary(Zoom) # Se compara las distribuciones de las 3 variables

 # Distribucion bastante similar excepto por el maximo de la del DANE
 
 # Trabajaria con Hourly.Wage y Hourly.Wage.DANE para las regresiones
 
  ####### Seleccionar las variables con las que se trabajará #####
  
  db_geih2018 <- geih2018 %>% 
  select(directorio, secuencia_p, orden, # Variables de ID
         clase, estrato1, age, maxEducLevel, sex, # Demograficas
         Labor.Income.DANE, y_total_m_imputada,
         hoursWorkUsual, hoursWorkActualSecondJob, totalHoursWorked, # Hours worked
         Hourly.Wage, Hourly.Wage.DANE, # Nuestras Y
         inglab, w, # Las que yo borraria
         formal, relab, regSalud, cotPension, sizeFirm # Variables laborales relevantes
         )
   

  
  ## Convertir variables texto a factor
  
  y <- c("estrato1", "sex", "maxEducLevel")
  db_geih2018[y] <- lapply(db_geih2018[y], as.factor)
  
  
  #---3. Estadística descriptiva ##########################################################################################
    
  ##### Box plot Edad - Ingreso Laboral #####
  
  db_geih2018 %>% 
    ggplot(aes(x=age, 
               y=Hourly.Wage, 
               fill= sex)) + 
    geom_boxplot(alpha=0.3) +
    theme(legend.position="none") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_brewer(palette="BuPu") # Demasiados Outliers!
  
  # Se computan los limites inferiores y superiores del boxplot
  
  ylim1 = boxplot.stats(db_geih2018$Hourly.Wage)$stats[c(1, 5)]
  
  db_geih2018 %>% 
    ggplot(aes(x=age, 
               y=Hourly.Wage, 
               fill= sex)) + 
    geom_boxplot(alpha=0.3,
                 outlier.shape = NA) +
    scale_y_continuous(labels = scales::comma) +
    coord_cartesian(ylim = ylim1*1.05) +
    scale_fill_brewer(palette="Dark2")
  
  # No parecen haber diferencias en el salario por hora
  
  ### Boxplot con ingreso laboral mensual
  
  ylim1 = boxplot.stats(db_geih2018$y_total_m_imputada)$stats[c(1, 5)]
  
  db_geih2018 %>% 
    ggplot(aes(x=age, 
               y=y_total_m_imputada, 
               fill= sex)) + 
    geom_boxplot(alpha=0.3,
                 outlier.shape = NA) +
    scale_y_continuous(labels = scales::comma) +
    coord_cartesian(ylim = ylim1*1.05) +
    scale_fill_brewer(palette="Dark2")
  
  #### Si se toma el ingreso laboral mensual, ya se empieza a apreciar la brecha salarial
  
  ##### Scatter plot Edad - Ingreso laboral #####
  
  ### Ingreso laboral por hora 
  
  db_geih2018 %>% 
    ggplot(aes(x=age, 
               y = Hourly.Wage,
               shape = sex,
               color = sex)
    ) + 
    geom_point() +
    scale_y_continuous(labels = scales::comma) +
    #coord_cartesian(ylim = ylim1*1.05) +
    theme_minimal()
  
  ### Ingreso laboral mensual 
  
  db_geih2018 %>% 
    filter(y_total_m_imputada <= 20000000) %>%  
    ggplot(aes(x=age, 
               y = y_total_m_imputada,
               shape = sex,
               color = sex)
    ) + 
    geom_point() +
    scale_y_continuous(labels = scales::comma) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1) +
    theme_minimal()
  
  # Se ve mas clara la relacion cuadratica que queremos encontrar
  
  
  ##### REVISAR ESTE CHUNK Creación de una variables categórica para rangos de edad #####
  
  # db_geih2018= db_geih2018 %>% mutate(
  #   cat_age = case_when(
  #     age <= 30~ '18-30',
  #     age > 30 & age <= 50 ~ '30-50',
  #     TRUE ~ '>50'
  #   )
  # )
  
  ## Tabla por rangos de edad REVISAR LAS VARIABLES SI ESTAN EN LA BASE
  
  #Tabla1 <- table(~ age + factor(sex) + factor(estrato1) +
   #                 factor(maxEducLevel) + hoursWorkUsual + inglab + w
    #              | cat_age, 
     #             data=db_geih2018, overall="Total")

  
  # Obtener el código de latex para la tabla 1
  #print(xtable(Tabla1), include.rownames = FALSE)
  
  
  #---4. Regresión1: Profile Age-Wage #########################################################################
  
  # Creacion de la variables necesarias para correr el modelo
  
  db_geih2018 <- db_geih2018 %>% 
    mutate(
      age2 = age^2, # Edad al cuadrado
      Log.Hourly.Wage = log(Hourly.Wage) # Log del salario por hora
    )
  
  OLS_Data <- db_geih2018
  
  reg1 <- lm(log(w) ~ age + age2,# Aqui no deberia ser el logaritmo del salario por hora?
             data = db_geih2018) # Por otro lado yo actualizaría a las nuevas variables de ingreso laboral
  
  summary(reg1)
  
  stargazer(reg1,type="text")

  reg1 <- lm(Log.Hourly.Wage ~ age + age2,
             data = db_geih2018)
  
  summary(reg1)
  
  stargazer(reg1,type="text")

  #Bootstrap
  
  sample_coef_intercept <- NULL
  sample_coef_x1 <- NULL
  sample_erstd_x1 <- NULL
  sample_coef_x2 <- NULL
  sample_erstd_x2 <- NULL
  
  for (i in 1:1000) {
    sample_d = db_geih2018[sample(1:nrow(db_geih2018), 0.3*nrow(db_geih2018), replace = TRUE), ]
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
  
  #---5. Regresión2: The gender earnings GAP ########################################################################
  reg2 <- lm(w ~ sex, data = db_geih2018)
  summary(reg2)
  stargazer(reg2,type="text")
  

  
  #---6 Predicting Earnings ############################################################
  
  