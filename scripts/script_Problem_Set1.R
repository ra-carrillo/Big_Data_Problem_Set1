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
  
  #install.packages("pacman") # activar solo una vez en el pc
  library(pacman)

  ## llamar la librería pacman: contiene la función p_load()
  require(pacman)

  ## El comandoo "p_load" permite instalar/cargar las librerías que se enlistan:
  p_load(tidyverse, # contiene las librerías ggplot, dplyr...
         rvest, # web-scraping
         stargazer,
         ggplot2, 
         hrbrthemes,
         tidymodels,
         fastDummies) # Tiene las herramientas para crear modelos de Machine learning

#---1. Descargar base de datos de la GEIH 2018-Bogotá usando web-scraping ###################################################
  
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
  
  ##### Cargar base de datos para evitar correr el loop #####
  
  data <- read.delim(
    "C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Big_Data_Problem_Set1/data/db_geih2018.txt")
  
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
  
  ## Filtrar la base con ocupados mayores a 18 años
  
  geih2018<- data %>% 
    filter(age >= 18 & ocu == 1)
  
  #Inspección variables ingreso de la base de datos
  
  summary(geih2018$y_ingLab_m) #	labor income salaried - nominal monthly - all occ. (includes tips and commission
  summary(geih2018$y_total_m) # income salaried + independents total - nominal monthly

  #Porcentaje de valores perdidos
  
  6650/16542
  1778/16542
  
  #Construccion variable ingreso laboral (Metodología DANE)
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
  
  summary(geih2018$y_total_m) # Variable base datos (Incluye NAs)
  summary(geih2018$Labor.Income.DANE ) # Variable construida No contamos con NAs
  
  #Visualizar cuantas observaciones tienen ingresos iguales a 0
  Zeros <- geih2018 %>% 
    filter(Labor.Income.DANE == 0) # 323 observaciones cuentan con ingresos igual a 0

  323/16542
  # Corresponde al 1% de la muestra
  
  # Replicar variable "y_total_m"
  ### Cálculo del Ingreso laboral sin variables imputadas por el DANE
  
  geih2018 <- geih2018 %>% 
    rowwise() %>% 
    mutate( 
      Labor.Income.Test = sum(impa, # ingreso primera actividades
                         #ie, # ingreso en especie
                         isa, # ingreso por segunda actividad
                         na.rm = TRUE),
      Labor.Income.Test = case_when(
        is.na(y_total_m) == TRUE ~ y_total_m,
        TRUE ~ Labor.Income.Test
      )
    )

  
  summary(geih2018$Labor.Income.Test) 
  summary(geih2018$y_total_m) # Variable base datos (Incluye NAs)

  # En promedio, sin el ingreso en especie, coincide con la variable y_total_m
  
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
  
  summary(geih2018$Labor.Income.DANE) 
  
  # Manejamos y_total_m_imputada y Labor.Income.DANE para todos los cálculos de aquí en adelante

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
 summary( geih2018  $Weekly.Wage)
 summary( geih2018  $Weekly.Wage.DANE)
 
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
 
  ##### Seleccionar las variables con las que se trabajará #####
  
  db_geih2018 <- geih2018 %>% 
  select(directorio, secuencia_p, orden, # Variables de ID
         clase, estrato1, age, maxEducLevel, sex, # Demograficas
         Labor.Income.DANE, "Labor.Income" = y_total_m_imputada,
         hoursWorkUsual, hoursWorkActualSecondJob, totalHoursWorked, # Hours worked
         Hourly.Wage, Hourly.Wage.DANE, # Nuestras Y
         #inglab, w, # Las que yo borraria
         formal, relab, regSalud, cotPension, sizeFirm, oficio # Variables laborales relevantes
         )
 
  #---3. Estadística descriptiva ##########################################################################################
    
  ##### Box plot Edad - Ingreso Laboral #####
  
  db_geih2018 %>% 
    ggplot(aes(x=age, 
               y=Hourly.Wage, 
               fill= as.factor(sex))) + 
    geom_boxplot(alpha=0.3) +
    theme(legend.position="none") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_brewer(palette="BuPu")+ # Demasiados Outliers!
   labs(x='Edad', y='Salario por hora')
  
  # Se computan los limites inferiores y superiores del boxplot
  
  ylim1 = boxplot.stats(db_geih2018$Hourly.Wage)$stats[c(1, 5)]
  
  db_geih2018 %>% 
    ggplot(aes(x=age, 
               y=Hourly.Wage, 
               fill= as.factor(sex))) + 
    geom_boxplot(alpha=0.3,
                 outlier.shape = NA) +
    scale_y_continuous(labels = scales::comma) +
    coord_cartesian(ylim = ylim1*1.05) +
    scale_fill_brewer(palette="Dark2")
  labs(x='Edad', y='Salario por hora')
  
  # No parecen haber diferencias en el salario por hora
  
  ### Boxplot con ingreso laboral mensual
  
  ylim1 = boxplot.stats(db_geih2018$Labor.Income)$stats[c(1, 5)]
  
  db_geih2018 %>% 
    ggplot(aes(x=age, 
               y=Labor.Income, 
               fill= as.factor(sex))) + 
    geom_boxplot(alpha=0.3,
                 outlier.shape = NA) +
    scale_y_continuous(labels = scales::comma) +
    coord_cartesian(ylim = ylim1*1.05) +
    scale_fill_brewer(palette="Dark2")+
  labs(x='Edad', y='Salario mensual')
  
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
    coord_cartesian(ylim = ylim1*1.05) +
    theme_minimal()+
    labs(x='Edad', y='Salario por hora')
  
  ### Ingreso laboral mensual 
  
  db_geih2018 %>% 
    filter(Labor.Income <= 20000000) %>%  
    ggplot(aes(x=age, 
               y = Labor.Income,
               shape = sex,
               color = sex)
    ) + 
    geom_point() +
    scale_y_continuous(labels = scales::comma) +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1) +
    theme_minimal()
  
  # Se ve mas clara la relacion cuadratica que queremos encontrar
  
  
  #---4. Regresión 1: Age-Wage profile
  
  ### Creacion de la variables necesarias para correr el modelo
  
  db_geih2018 <- db_geih2018 %>% 
    mutate(
      age2 = age^2, # Edad al cuadrado
      Log.Hourly.Wage = log(Hourly.Wage),
      Log.Hourly.Wage.DANE = log (Hourly.Wage.DANE) # Log del salario por hora
    )
  
 ### Modelo de Regresión 1: Estimador MCO
  
  reg1 <- lm(Log.Hourly.Wage ~ age + age2,
             data = db_geih2018) 
  
  summary(reg1)
  stargazer(reg1,type="text")
  summary(db_geih2018$Log.Hourly.Wage)
  #Para obtener el código de la tabla en latex

  stargazer(reg1, header=FALSE,
            digits=2, single.row=FALSE,
            intercept.bottom=TRUE,
            df = FALSE
            )
  
  
  ### Bootstrap
  
  sample_coef_intercept <- NULL
  sample_coef_x1 <- NULL
  sample_erstd_x1 <- NULL
  sample_coef_x2 <- NULL
  sample_erstd_x2 <- NULL
  
  for (i in 1:1000) {
    sample_d = db_geih2018[sample(1:nrow(db_geih2018), 0.3*nrow(db_geih2018), replace = TRUE), ]
    reg_boots <- lm(Log.Hourly.Wage ~ age + age2, data = sample_d)
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
  
  means.boots = c(mean(sample_coef_intercept), mean(sample_coef_x1), 
                  mean(sample_coef_x2))
  erstd.boots = c(0,mean(sample_erstd_x1),mean(sample_erstd_x2))
  knitr::kable(round(
    cbind(
      sample = coef(summary(reg1))[, c(1,2)],
      bootstrap = means.boots,
      erstdBoots = erstd.boots),4), 
    "simple", caption = "Coefficients in different models")

  ### Intervalos de confianza
  
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

  
  reg2 <- lm(Log.Hourly.Wage ~ sex, data = db_geih2018) 
  summary(reg2)
  stargazer(reg2,type="text")
  
  ## Bootstrap por género
  Tabla_men <- db_geih2018 %>% filter(sex == 1)
  Tabla_fem <- db_geih2018 %>% filter(sex == 0)
  
  ## men
  Tabla_men$age2 <- Tabla_men$age*Tabla_men$age
  reg_men <- lm(Log.Hourly.Wage ~ age + age2, data = Tabla_men)
  sample_coef_intercept <- NULL
  sample_coef_x1 <- NULL
  sample_erstd_x1 <- NULL
  sample_coef_x2<- NULL
  sample_erstd_x2 <- NULL
  for (i in 1:1000) {
    sample_d = Tabla_men[sample(1:nrow(Tabla_men), 0.3*nrow(Tabla_men), replace = TRUE), ]
    
    model_boots <- lm(Log.Hourly.Wage ~ age + age2, data = sample_d)
    
    sample_coef_intercept <-
      c(sample_coef_intercept, model_boots$coefficients[1])
    
    sample_coef_x1 <-
      c(sample_coef_x1, model_boots$coefficients[2])
    
    sample_erstd_x1 <-
      c(sample_erstd_x1, coef(summary(model_boots))[2, 2])
    
    sample_coef_x2 <-
      c(sample_coef_x2, model_boots$coefficients[3])
    
    sample_erstd_x2 <-
      c(sample_erstd_x2, coef(summary(model_boots))[3, 2])
  }
  coefs <- rbind(sample_coef_intercept, sample_coef_x1, sample_erstd_x1, 
                 sample_coef_x2, sample_erstd_x2)
  
  # Combinar los resultados en una tabla 
  means.boot = c(mean(sample_coef_intercept), mean(sample_coef_x1), 
                 mean(sample_coef_x2))
  erstd.boot = c(0,mean(sample_erstd_x1),mean(sample_erstd_x2))
  knitr::kable(round(
    cbind(
      sample = coef(summary(reg_men))[, c(1,2)],
      bootstrap = means.boot,
      erstdBoots = erstd.boot),4), 
    "simple", caption = "Coefficients in different models")
  
  # Intervalos de confianza
  
  confint(reg_men)
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
      sample = confint(reg_men),
      boot = rbind(a, b, c)), 4)
  colnames(d) <- c("2.5 %", "97.5 %",
                   "2.5 %", "97.5 %")
  d
  
  
  ## female
  Tabla_fem$age2 <- Tabla_fem$age*Tabla_fem$age
  reg_fem <- lm(Log.Hourly.Wage ~ age + age2, data = Tabla_fem)
  sample_coef_intercept <- NULL
  sample_coef_x1 <- NULL
  sample_erstd_x1 <- NULL
  sample_coef_x2<- NULL
  sample_erstd_x2 <- NULL
  for (i in 1:1000) {
    sample_d = Tabla_fem[sample(1:nrow(Tabla_fem), 0.3*nrow(Tabla_fem), replace = TRUE), ]
    
    model_boots <- lm(Log.Hourly.Wage ~ age + age2, data = sample_d)
    
    sample_coef_intercept <-
      c(sample_coef_intercept, model_boots$coefficients[1])
    
    sample_coef_x1 <-
      c(sample_coef_x1, model_boots$coefficients[2])
    
    sample_erstd_x1 <-
      c(sample_erstd_x1, coef(summary(model_boots))[2, 2])
    
    sample_coef_x2 <-
      c(sample_coef_x2, model_boots$coefficients[3])
    
    sample_erstd_x2 <-
      c(sample_erstd_x2, coef(summary(model_boots))[3, 2])
  }
  coefs <- rbind(sample_coef_intercept, sample_coef_x1, sample_erstd_x1, 
                 sample_coef_x2, sample_erstd_x2)
  
  # Combinar los resultados en una tabla
  means.boot = c(mean(sample_coef_intercept), mean(sample_coef_x1), 
                 mean(sample_coef_x2))
  erstd.boot = c(0,mean(sample_erstd_x1),mean(sample_erstd_x2))
  knitr::kable(round(
    cbind(
      sample = coef(summary(reg_fem))[, c(1,2)],
      bootstrap = means.boot,
      erstdBoots = erstd.boot),4), 
    "simple", caption = "Coefficients in different models")
  
  
  # Intervalos de confianza 
  confint(reg_fem)
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
      sample = confint(reg_fem),
      boot = rbind(a, b, c)), 4)
  colnames(d) <- c("2.5 %", "97.5 %",
                   "2.5 %", "97.5 %")
  d
  
  
  reg2 <- lm(Log.Hourly.Wage ~ sex, data = db_geih2018) 
  summary(reg2)
  stargazer(reg2,type="text")
  
  #---6 Predicting Earnings ############################################################
  
  
  ##### Pre-procesamiento de la base y ED#####
  
  Model_Data <- db_geih2018 %>% 
    select("Class" = clase, "Strata" = estrato1, "Age" = age, 
           "Age_Sqrt"= age2, "sex.old" = sex, 
           "Max.Educ.Level" = maxEducLevel, 
           Hourly.Wage, Hourly.Wage.DANE, 
           "Formal" = formal, relab, "Job.Type"= oficio, cotPension, #regSalud,
           sizeFirm, Log.Hourly.Wage, Log.Hourly.Wage.DANE) %>% 
    mutate(
      Sex = case_when(
        sex.old == 1 ~ "Man",
        sex.old == 0 ~ "Women"
        ),
      Worker.Type  = case_when(
        relab == 1 ~ "Private Employee",
        relab == 2 ~ "Goverment Employee",
        relab == 3 ~ "Household Employee",
        relab == 4 ~ "Self Employed",
        relab == 5 ~ "Employer",
        relab == 6 ~ "No Rem Family Employee",
        relab == 7 ~ "No Rem Private Employee",
        relab == 8 ~ "Jornaler",
        relab == 9 ~ "Other"
      ),
      Firm.Size  = case_when(
        sizeFirm == 1 ~ "Self Employed",
        sizeFirm == 2 ~ "2-5 Workers",
        sizeFirm == 3 ~ "6-10 Workers",
        sizeFirm == 4 ~ "11-50 Workers",
        sizeFirm == 5 ~ ">50 Workers"),
      Job.Type = as.character(Job.Type),
      Max.Educ.Level = as.character(Max.Educ.Level)
      
      )
  
  summary(Model_Data)
  
  ###  Revisar distribución de la variable objetivo   #####  
  
  ## Computada e imputada por nosotros
  
  Model_Data %>% 
    ggplot(aes(x = Log.Hourly.Wage)) + 
    geom_histogram(alpha = 0.5)
  
  # Se observa un leve sesgo en la distribucion de ingresos, por lo que la participacion se estratifica para asegurar una representacion adecuada
  
  ## Imputada por DANE
  
  Model_Data %>% 
    ggplot(aes(x = Log.Hourly.Wage.DANE)) + 
    geom_histogram(alpha = 0.5)
  
  # Una distirbución normal más centrada
  
  ##### a) Partiendo la base de datos en dos sub muestras de entrenamiento y evaluacion ##### 
  
  set.seed(2403)
  
  GEIH_Split <- Model_Data %>% 
    na.omit() %>% 
    initial_split(prop = 0.70,
                  strata = Log.Hourly.Wage)
  
  GEIH_Training <- training(GEIH_Split)
  
  GEIH_Test <- testing(GEIH_Split)
  
  
  ##### b) Estimación modelos y evaluación de ajuste ##### 
  
  #### Primer modelo (Log(Wage) = Age + Age^2) #####
  
  ### Se prepara la base de entrenamiento y se especifica la forma funcional
  
  GEIH_Recipe_M1 <- 
    recipe(Log.Hourly.Wage ~ Age + Age_Sqrt, 
           GEIH_Training)
  
  GEIH_Recipe_M1
  

  ### Se especifica el modelo
  
  GEIH_Spec <- 
    linear_reg() %>% 
    set_engine("lm") %>% 
    set_mode("regression")
  
  ### Workflow
  
  GEIH_Wflow <- workflow(
    GEIH_Recipe_M1,
    GEIH_Spec
    )
  
  GEIH_Wflow
  
  ### Se corre el modelo
  
  GEIH_Fit_M1 <- fit(GEIH_Wflow,
                  GEIH_Training
  )
  
  GEIH_Fit_M1
  
  tidy(GEIH_Fit_M1,
       exponentiate = TRUE)
  
  Training_Metrics_M1 <- glance(GEIH_Fit_M1)
  
  ### Métricas de evaluación
  
  GEIH_Test_Res_M1 <- predict(
    GEIH_Fit_M1, 
    GEIH_Test %>% 
      select(-Log.Hourly.Wage)
  )
  
  GEIH_Test_Res_M1 <- bind_cols(GEIH_Test_Res_M1, 
                             GEIH_Test %>% 
                               select(Log.Hourly.Wage))
  

  ggplot(GEIH_Test_Res_M1, 
         aes(x = Log.Hourly.Wage, 
             y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(alpha = 0.5) + 
    labs(y = "Predicted Log Labor Income", x = "Log Hourly Labor Income") +
    # Scale and size the x- and y-axis uniformly:
    coord_obs_pred()
  
  GEIH_Metrics <- metric_set(rmse, 
                             rsq, 
                             mae)
  
  Out_Sample_Metrics_M1 <- GEIH_Metrics(GEIH_Test_Res_M1, 
               truth = Log.Hourly.Wage, 
               estimate = .pred)
  
  Out_Sample_Metrics_M1
  
  #### Segundo modelo (Log(Wage) = Sex) #####
  
  ### Se prepara la base de entrenamiento y se especifica la forma funcional

  GEIH_Recipe_M2 <- 
    recipe(Log.Hourly.Wage ~ Sex, 
           GEIH_Training) %>% 
    step_string2factor(Sex)
  
  GEIH_Recipe_M2
  
  ### Se especifica el modelo
  
  GEIH_Spec <- 
    linear_reg() %>% 
    set_engine("lm") %>% 
    set_mode("regression")
  
  ### Workflow
  
  GEIH_Wflow <- workflow(
    GEIH_Recipe_M2,
    GEIH_Spec
  )
  
  GEIH_Wflow
  
  ### Se corre el modelo
  
  GEIH_Fit_M2 <- fit(GEIH_Wflow,
                           GEIH_Training
  )
  
  GEIH_Fit_M2
  
  tidy(GEIH_Fit_M2,
       exponentiate = TRUE)
  
  Training_Metrics_M2 <- glance(GEIH_Fit_M2)
  
  ### Métricas de evaluación
  
  GEIH_Test_Res_M2 <- predict(
    GEIH_Fit_M2, 
    GEIH_Test %>% 
      select(-Log.Hourly.Wage)
  )
  
  GEIH_Test_Res_M2 <- bind_cols(GEIH_Test_Res_M2, 
                             GEIH_Test %>% 
                               select(Log.Hourly.Wage))
  
  
  ggplot(GEIH_Test_Res_M2, 
         aes(x = Log.Hourly.Wage, 
             y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(alpha = 0.5) + 
    labs(y = "Predicted Log Labor Income", x = "Log Hourly Labor Income") +
    # Scale and size the x- and y-axis uniformly:
    coord_obs_pred()
  
  GEIH_Metrics <- metric_set(rmse, 
                             rsq, 
                             mae)
  
  Out_Sample_Metrics_M2 <- GEIH_Metrics(GEIH_Test_Res_M2, 
               truth = Log.Hourly.Wage, 
               estimate = .pred)
  
  
  #### Tercer modelo (Log(Wage) = Sex + Controles (FWL)) #####
  
  FWL_Model_Data <- dummy_cols(GEIH_Training, 
                     select_columns = c("Firm.Size",
                                        "Max.Educ.Level",
                                        "Job.Type",
                                        "Worker.Type")
                     )
  
  FWL_Model_Data[sapply(FWL_Model_Data, is.character)] <- lapply(FWL_Model_Data[sapply(FWL_Model_Data, is.character)], factor)
  
  OLS1 <- lm(Log.Hourly.Wage ~ Sex + Age + Age_Sqrt + 
               `Firm.Size_2-5 Workers` + `Firm.Size_6-10 Workers`+
               `Firm.Size_11-50 Workers` + `Firm.Size_>50 Workers` +
               Formal + `Worker.Type_Employer` + `Worker.Type_Goverment Employee` +
               `Worker.Type_Household Employee` + `Worker.Type_Self Employed` +
               `Worker.Type_No Rem Family Employee` + `Worker.Type_No Rem Private Employee` +
               `Worker.Type_Private Employee`, FWL_Model_Data)
  
  stargazer(OLS1, type = "text")
  
  summary(FWL_Model_Data)
  
  
  Test <- FWL_Model_Data %>% 
    mutate(Resid.Sex = lm(sex.old ~  Age + Age_Sqrt + 
                                       `Firm.Size_2-5 Workers` + `Firm.Size_6-10 Workers`+
                                       `Firm.Size_11-50 Workers` + `Firm.Size_>50 Workers` +
                                       Formal + `Worker.Type_Employer` + `Worker.Type_Goverment Employee` +
                                       `Worker.Type_Household Employee` + `Worker.Type_Self Employed` +
                                       `Worker.Type_No Rem Family Employee` + `Worker.Type_No Rem Private Employee` +
                                       `Worker.Type_Private Employee`)$residuals) 
  
  #Residuals of Sex ~ controls
  
  OLS2 <- lm(Log.Hourly.Wage ~ Resid.Sex,
             Test)
  
  stargazer(OLS1, 
            OLS2,
            type = "text")
  
  summary(GEIH_Training)
  
  ### Se prepara la base de entrenamiento y se especifica la forma funcional
  
  GEIH_Recipe_M3 <- 
    recipe(Log.Hourly.Wage ~ Sex + Max.Educ.Level + Age + Age_Sqrt + 
             Worker.Type + Formal + Firm.Size + Job.Type, 
           GEIH_Training) %>% 
    step_dummy(Max.Educ.Level, Worker.Type, Firm.Size, 
               Job.Type)
  
  GEIH_Recipe_M3
  
  ## Se revisa que la base se vaya a preparar correctamente
  
  GEIH_Prep <- prep(GEIH_Recipe_M3)

  bake(GEIH_Prep, 
       new_data = NULL) %>% 
    str()
  
  
  ### Se especifica el modelo
  
  GEIH_Spec <- 
    linear_reg() %>% 
    set_engine("lm") %>% 
    set_mode("regression")
  
  ### Workflow
  
  GEIH_Wflow <- workflow(GEIH_Recipe_M3,
                         GEIH_Spec)
  
  ### Se corre el modelo
  
  GEIH_Fit_M3 <- fit(GEIH_Wflow,
                  GEIH_Training
  )
  
  GEIH_Fit_M3
  
  tidy(GEIH_Fit_M3,
       exponentiate = TRUE)
  
  Training_Metrics_M3 <- glance(GEIH_Fit_M3)
  
  ### Métricas de evaluación
  
  GEIH_Test_Res_M3 <- predict(
    GEIH_Fit_M3, 
    GEIH_Test %>% 
      select(-Log.Hourly.Wage)
  )
  
  GEIH_Test_Res_M3 <- bind_cols(GEIH_Test_Res_M3, 
                             GEIH_Test %>% 
                               select(Log.Hourly.Wage))
  
  ggplot(GEIH_Test_Res_M3, 
         aes(x = Log.Hourly.Wage, 
             y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(alpha = 0.5) + 
    labs(y = "Predicted Log Labor Income", x = "Log Hourly Labor Income") +
    # Scale and size the x- and y-axis uniformly:
    coord_obs_pred()
  
  GEIH_Metrics <- metric_set(rmse, 
                             rsq, 
                             mae)
  
  Out_Sample_Metrics_M3 <- GEIH_Metrics(GEIH_Test_Res_M3, 
               truth = Log.Hourly.Wage, 
               estimate = .pred)
  
  Out_Sample_Metrics_M1
  Out_Sample_Metrics_M2
  Out_Sample_Metrics_M3
  
  
  #### Intento conjunto ####
  
  
  ### Se prepara la base de entrenamiento y se especifica la forma funcional
  
  GEIH_Recipe_M1 <- 
    recipe(Log.Hourly.Wage ~ age + age2, 
           GEIH_Training)
  
  GEIH_Recipe_M1
  
  GEIH_Recipe_M2 <- 
    recipe(Log.Hourly.Wage ~ Sex, 
           GEIH_Training) %>% 
    step_string2factor(Sex)
  
  GEIH_Recipe_M2
  
  ### Se especifica el modelo
  
  GEIH_Spec <- 
    linear_reg() %>% 
    set_engine("lm") %>% 
    set_mode("regression")
  
  ### Workflow
  
  GEIH_Wflow <- workflow_set(
    list (GEIH_Recipe_M1, GEIH_Recipe_M2),
    list(GEIH_Spec, GEIH_Spec)
  )
  
  GEIH_Wflow
  
  ### Se corre el modelo
  
  GEIH_Fit <- workflow_map(GEIH_Wflow,
                           GEIH_Training
  )
  
  GEIH_Fit
  
  tidy(GEIH_Fit,
       exponentiate = TRUE)
  
  Training_Metrics <- glance(GEIH_Fit)
  
  ### Métricas de evaluación
  
  GEIH_Test_Res <- predict(
    GEIH_Fit, 
    GEIH_Test %>% 
      select(-Log.Hourly.Wage)
  )
  
  GEIH_Test_Res <- bind_cols(GEIH_Test_Res, 
                             GEIH_Test %>% 
                               select(Log.Hourly.Wage))
  
  
  ggplot(GEIH_Test_Res, 
         aes(x = Log.Hourly.Wage, 
             y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(alpha = 0.5) + 
    labs(y = "Predicted Log Labor Income", x = "Log Hourly Labor Income") +
    # Scale and size the x- and y-axis uniformly:
    coord_obs_pred()
  
  GEIH_Metrics <- metric_set(rmse, 
                             rsq, 
                             mae)
  
  GEIH_Metrics(GEIH_Test_Res, 
               truth = Log.Hourly.Wage, 
               estimate = .pred)
  