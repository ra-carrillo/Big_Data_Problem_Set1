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
  require(fastDummies)
  ## El comando "p_load" permite instalar/cargar las librerías que se enlistan:

  p_load(tidyverse, # contiene las librerías ggplot, dplyr...
         rvest, # web-scraping
         stargazer,
         ggplot2, 
         hrbrthemes,
         tidymodels,
         splines,
         gridExtra,
         ranger, # Tiene las herramientas para crear modelos de Machine learning
         bootstrap,
         boot,
         rio,
         tidyr) 

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
  
  # Manejaría y_total_m_imputada y Labor.Income.DANE para todos los cálculos de aquí en adelante

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
 
 
 #Construcción variables nivel educativo
 # - Primaria
 
 geih2018$primaria <- ifelse(geih2018$p6210 == 3, 1, 0)
 geih2018$primaria[geih2018$p6210 == "."] <- NA
 
 # - Secundaria
 
 geih2018$secundaria <- ifelse(geih2018$p6210 == 4, 1, 0)
 geih2018$secundaria[geih2018$p6210 == "."] <- NA
 
 # - Media
 
 geih2018$media <- ifelse(geih2018$p6210 == 5, 1, 0)
 geih2018$media[geih2018$p6210 == "."] <- NA
 
 # - Superior
 
 geih2018$superior <- ifelse(geih2018$p6210 == 6, 1, 0)
 geih2018$superior[geih2018$p6210 == "."] <- NA
  
  db_geih2018 <- geih2018 %>% 
  select(directorio, secuencia_p, orden, # Variables de ID
         clase, "Estrato"=estrato1, age, maxEducLevel, sex, "Kinship" = p6050, # Demograficas
         Labor.Income.DANE, "Labor.Income" = y_total_m_imputada,
         hoursWorkUsual, hoursWorkActualSecondJob, totalHoursWorked, # Hours worked
         Hourly.Wage, Hourly.Wage.DANE, # Nuestras Y
         formal, relab, regSalud, cotPension, "Self.Emp" = cuentaPropia,
         sizeFirm, oficio, # Variables laborales relevantes
          primaria,secundaria,media,superior)

 #Niveles socio-economicos

 db_geih2018 <- dummy_cols(db_geih2018, select_columns = c("Estrato"))
 
 ### Creacion de la variables necesarias para correr el modelo
 
 db_geih2018 <- db_geih2018 %>% 
   mutate(
     age2 = age*age, # Edad al cuadrado
     Log.Hourly.Wage = log(Hourly.Wage),
     Log.Hourly.Wage.DANE = log (Hourly.Wage.DANE), # Log del salario por hora
     Log.Month.Wage = log(Labor.Income))  # Log del salario mensual

 Model_Data <- db_geih2018 %>% 
   select("Class" = clase, "Strata" = Estrato, "Age" = age, 
          "Age_Sqrt"= age2, "Sex" = sex, 
          "Max.Educ.Level" = maxEducLevel,
          Hourly.Wage, Hourly.Wage.DANE, 
          "Formal" = formal, relab, "Job.Type"= oficio, "Tothour.worked"=totalHoursWorked, cotPension, regSalud,
          sizeFirm, Log.Hourly.Wage, Log.Hourly.Wage.DANE,
          primaria,secundaria,media,superior) %>%
   mutate(
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
     HealthCare.System = case_when(
       regSalud == 1 ~ "Contributory",
       regSalud == 2 ~ "Special",  
       regSalud == 3 ~ "Subsidized",
       TRUE ~ "NA"
     ),
     Job.Type = as.character(Job.Type),
     Max.Educ.Level = as.character(Max.Educ.Level),
     Class = as.character(Class),
     Strata = as.character(Strata)
   ) %>% 
   na.omit()
 
 summary(Model_Data)
 
 #Creación variable mujer=1 y hombre=0
 table(Model_Data$Sex)
 
 Model_Data <- Model_Data %>% 
   mutate(
     Female=Female<-ifelse(Sex==0,1,0))  
 
 table(Model_Data$Female)
 Model_Data$Female
 
 Model_Data$Female <- factor(Model_Data$Female, levels=c(0, 1), labels=c("Hombre", "Mujer"))
 Model_Data$Female
 
 table(Model_Data$Female)


 #AQUI VOY
 
  #---3. Estadística descriptiva ##########################################################################################
    
  ##### Box plot Edad - Ingreso Laboral #####
  
  db_geih2018 %>% 
    ggplot(aes(x=age, 
               y=Hourly.Wage, 
               fill= as.factor(sex))) + 
    geom_boxplot(alpha=0.3) +
    theme(legend.position="none") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_brewer(palette="BuPu") # Demasiados Outliers!
  
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
  
#-----------------------------------------------------------#
  
#---4. Regresión1: Profile Age-Wage 
  
 ### Regresión 1: Estimación por MCO
  
  reg1 <- lm(Log.Hourly.Wage  ~ Age + Age_Sqrt,
             data = Model_Data) 
  
  summary(reg1)
  
  stargazer(reg1,type="text")

  #Para obtener el código de la tabla en latex
  
  stargazer(reg2, header=FALSE,
            digits=2, single.row=FALSE,
            intercept.bottom=TRUE,
            df = FALSE
  )

  ### Bootstrap
  
  #Obtener los coeficientes de la regresión
  coefs<-reg1$coef
  coefs 
  
  #Extaer los coeficientes a escalares
  b0<-coefs[1]#constante
  b1<-coefs[2]#coeficiente asociado a la edad
  b2<-coefs[3]#coeficiente asociado a la edad al cuadrado

  # Estimar la elasticidad en el valor de la media 
  Age_bar<-mean(Model_Data$Age)
  
  #Obtener la elasticidad del salario:
  elastpt<-b1+2*b2*Age_bar
  
  elastpt
  
  eta_mod2_fn<-function(data,index,
                        Age_bar=mean(Model_Data$Age)){
    
    #Obtener los coeficientes
    coefs<-lm(Log.Hourly.Wage  ~ Age + Age_Sqrt,data, subset = index)$coefficients
    
    #Poner los coeficientes en escalares 
    b1<-coefs[2]
    b2<-coefs[3] 

    #Calcular la elasticidad del salario
    elastpt<-b1+2*b2*Age_bar
    
    #Devolver la elasticidad del salario
    return(elastpt)
  }
  
  eta_mod2_fn(Model_Data,1:nrow(Model_Data))  

  #Evaluar en diferentes puntos de la distribución de edad
  eta_mod2_fn(Model_Data,1:nrow(Model_Data),Age_bar=-1) 
  
  eta_mod2_fn(Model_Data,1:nrow(Model_Data),Age_bar=2) 
  
  #Obtener el error estándar de nuestra elasticidad
  results <- boot(Model_Data, eta_mod2_fn,R=1000)
  results
  
  # Bootstrap 
  eta_fn<-function(data,index){
    coef(lm(Log.Hourly.Wage  ~ Age + Age_Sqrt, data = Model_Data, subset = index)) 
  } 
  
  eta_fn(Model_Data,1:nrow(Model_Data))
  
  boot <- boot(Model_Data, eta_fn, R = 1000)
  coef_boot <- boot$t0
  SE <- apply(boot$t,2,sd)
  boot
  coef_boot
  
  summary(Model_Data$Age)
  
  # Matriz con las X y las Y
  
  x <- seq(18, 94, length.out = 100)
  y <- coef_boot[1] + coef_boot[2] * x + coef_boot[3] * x^2
  y_inf <- (coef_boot[1]-1.96*SE[1]) + (coef_boot[2]-1.96*SE[2])*x + 
    (coef_boot[3]-1.96*SE[3])*x^2
  y_sup <- (coef_boot[1]+1.96*SE[1]) + (coef_boot[2]+1.96*SE[2])*x + 
    (coef_boot[3]+1.96*SE[3])*x^2
  
  df <- data.frame(x, y, y_inf, y_sup)
  
  # Graficar la función
  
  graph <- ggplot(df, aes(x = x, y = y)) +
    geom_line(aes(color = "Estimado"), size = 1) +
    geom_line(aes(x = x, y = y_inf, color = "Intervalo inferior"), linetype = "dotted", size = 1) +
    geom_line(aes(x = x, y = y_sup, color = "Intervalo superior"), linetype = "dotted", size = 1) +
    scale_color_manual(name = "", values = c("Estimado" = "green3", "Intervalo inferior" = "red", "Intervalo superior" = "red")) +
    labs(x = "Edad", y = "Log(Salario por hora)") +
    theme_classic() +
    scale_x_continuous(limits = c(18, 94)) +
    geom_vline(xintercept = 50, linetype = "dotted") +
    theme(legend.position = "bottom")
  
  ggsave(graph, filename = "C:/Users/andre/OneDrive/Github/Repositorios/Big_Data_Problem_Set1/views/Rplot4.png", height = 5, width = 6)
  
  #---5. Regresión 2: The gender earnings GAP 
  reg2 <- lm(Log.Hourly.Wage ~ Female, data = Model_Data)
  
  summary(reg2)
  
  stargazer(reg2,type="text")
  
  #Para obtener el código de la tabla en latex
  
  stargazer(reg2, header=FALSE,
            digits=2, single.row=FALSE,
            intercept.bottom=TRUE,
            df = FALSE
  )
  #---5. Regresión 2: The gender earnings GAP + controles
  reg3 <- lm(Log.Hourly.Wage ~ Female + Age + Age_Sqrt +
               Formal + secundaria + media + superior+ Tothour.worked, data = Model_Data)
  
  summary(reg3)
  
  stargazer(reg2 , reg3,type="text")
  
  #Para obtener el código de la tabla en latex
  
  stargazer(reg2,reg3, header=FALSE,
            digits=2, single.row=FALSE,
            intercept.bottom=TRUE,
            df = FALSE
  )
  
  Model_Data2 <- Model_Data %>% 
    select(Log.Hourly.Wage,Female,Age,Age_Sqrt,Formal,
           secundaria,media,superior,Tothour.worked) %>%
    mutate(Log.Hourly.Wage=as.numeric(Log.Hourly.Wage),
                                   Female=as.numeric(Female),
                                   Age=as.numeric(Age),
                                   Age_Sqrt=as.numeric(Age_Sqrt),
                                   Formal=as.numeric(Formal),
                                   secundaria=as.numeric(secundaria),
                                   media=as.numeric(media),
                                   superior=as.numeric(superior),
                                   Tothour.worked=as.numeric(Tothour.worked))
  
  #Ahora vamos a estimar la brecha a partir del teorema de FWl
  
  # Primero estimar el y residual
  Model_Data2<-Model_Data2%>% 
    mutate(loghw_ResidF=lm(Log.Hourly.Wage ~  Age + Age_Sqrt +
                             Formal + secundaria + media + superior+ Tothour.worked,Model_Data2)$residuals)

  # Segundo estimar el x residual
  Model_Data2<-Model_Data2%>% 
    mutate(fem_ResidF=lm(Female  ~  Age + Age_Sqrt +
                           Formal + secundaria + media + superior+ Tothour.worked ,Model_Data2)$residuals) 
  
  # 3) Regress the residuals from step 2 on the residuals from step 1
  
  reg3_fwl<-lm(loghw_ResidF~fem_ResidF,Model_Data2)
  stargazer(reg3,reg3_fwl,type="text")
  
  #Para obtener el código de la tabla en latex
  
  stargazer(reg3,reg3_fwl, header=FALSE,
            digits=2, single.row=FALSE,
            intercept.bottom=TRUE,
            df = FALSE
  )
  
  # Teorema FWL con Bootstrap
  # Establecer la funcion de FWL
  fwl_in_action<-function(Model_Data2,index) {
    #FWL is the regression of residuals on residuals
    Model_Data2$y_resid<-resid(lm(Log.Hourly.Wage ~ Age + Age_Sqrt +
                                    Formal + secundaria + media + superior+ Tothour.worked, data=Model_Data2, subset=index))
    Model_Data2$x_resid<-resid(lm(Female ~ Age + Age_Sqrt +
                                    Formal + secundaria + media + superior+ Tothour.worked, data=Model_Data2, subset=index))
    coef_interest<-coef(lm(y_resid~x_resid, data=Model_Data2, subset=index))
    coef_interest
  }
  
  #Vamos a verificar que si funciona
  lm(Log.Hourly.Wage ~ Female + Age + Age_Sqrt +
       Formal + secundaria + media + superior+ Tothour.worked ,Model_Data2)
  fwl_in_action(Model_Data2,1:nrow(Model_Data2))
  
  #Ejecutar el Bootstrap
  Boot_FWL <- boot(Model_Data2, fwl_in_action, R = 1000)
  Boot_FWL
  

  # Estimacion de edades maximas e intervalos
  
  
  #---5.3 Regresión 2: The gender earnings GAP + controles
  Model_Data2 <- Model_Data2 %>% 
    mutate(Fem_Age = Female*Age,
           Fem_Age2 = Female*Age_Sqrt)
  
  reg4 <- lm(Log.Hourly.Wage ~ Female + Age + Age_Sqrt + Fem_Age + Fem_Age2 +
               Formal + secundaria + media + superior+ Tothour.worked, data = Model_Data2)
  coefs_reg4 <- reg4$coef
  coefs_reg4
  summary(reg4)

  eta_reg4_fn_fem<-function(Model_Data2,index){
    
    #Obtener coeficientes
    coefs<-lm(Log.Hourly.Wage ~ Female + Age + Age_Sqrt + Fem_Age + Fem_Age2 +
                Formal + secundaria + media + superior+ Tothour.worked,Model_Data2, subset = index)$coefficients
    
    #Coeficientes en escalares 
    b0<-coefs[1]
    b1<-coefs[2] 
    b2<-coefs[3]
    b3<-coefs[4]
    b4<-coefs[5] 
    b5<-coefs[6] 
    b6<-coefs[7]
    b7<-coefs[8] 
    b8<-coefs[9]
    b9<-coefs[10]
    b10<-coefs[11] 
    
    # Calcular la edad maxima de las mujeres
    pico_fem<- (-(b2+b4)/(2*(b3+b5)))
    
    return(pico_fem)
  }
  
  eta_reg4_fn_fem(Model_Data2,1:nrow(Model_Data2))
  
  boot_fem <- boot(Model_Data2, eta_reg4_fn_fem, R = 1000)
  boot_fem
  
  #Intervalos de  confianza 
  
  intervalo_fem <- quantile(boot_fem$t, c(0.025, 0.975))
  intervalo_fem
  
  eta_reg4_fn_men<-function(Model_Data2,index){
    
    # Obtener coeficientes
    coefs<-lm(Log.Hourly.Wage ~ Female + Age + Age_Sqrt + Fem_Age + Fem_Age2 +
                Formal + secundaria + media + superior+ Tothour.worked,Model_Data2, subset = index)$coefficients
    
    # Colocar coeficientes en escalares 
    b0<-coefs[1]
    b1<-coefs[2] 
    b2<-coefs[3]
    b3<-coefs[4]
    b4<-coefs[5] 
    b5<-coefs[6] 
    b6<-coefs[7]
    b7<-coefs[8] 
    b8<-coefs[9]
    b9<-coefs[10]
    b10<-coefs[11]  
    
    # Calcular la edad maxima de los hombres
    pico_men <- (-b2/(2*b3))
    
    return(pico_men)
  }
  
  eta_reg4_fn_men(Model_Data2,1:nrow(Model_Data2))
  
  boot_men <- boot(Model_Data2,  eta_reg4_fn_men, R = 1000)
  boot_men
  
  # intervalos de confianza para los hombres 
  
  intervalo_men <- quantile(boot_men$t, c(0.025, 0.975))
  intervalo_men


  #Gráfica para las mujeres
  
  Age_fem<-function(Model_Data2,index){
    
    # Obtener coeficientes
    coefs<-lm(Log.Hourly.Wage ~ Female + Age + Age_Sqrt + Fem_Age + Fem_Age2 +
                Formal + secundaria + media + superior+ Tothour.worked,Model_Data2, subset = index)$coefficients
    
    # Coeficientes en escalares 
    b0<-coefs[1]
    b1<-coefs[2] 
    b2<-coefs[3]
    b3<-coefs[4]
    b4<-coefs[5] 
    b5<-coefs[6] 
    b6<-coefs[7]
    b7<-coefs[8] 
    b8<-coefs[9]
    b9<-coefs[10]
    b10<-coefs[11]  
    
    #Edad Máxima mujeres
    beta_age_fem<- ((b2+b4))
    
    
    return(beta_age_fem)
    
  }
  
  Age_fem(Model_Data2,1:nrow(Model_Data2))
  
  boot_age_fem <- boot(Model_Data2, Age_fem, R = 1000)
  boot_age_fem
  coef_ed_fem <- boot_age_fem$t0
  se_ed_fem <- apply(boot_age_fem$t,2,sd)
  
  
  Age2_fem<-function(Model_Data2,index){
    
    #Obtener coeficientes
    coefs<-lm(Log.Hourly.Wage ~ Female + Age + Age_Sqrt + Fem_Age + Fem_Age2 +
                Formal + secundaria + media + superior+ Tothour.worked,Model_Data2, subset = index)$coefficients
    
    #Coeficientes en escalares 
    b0<-coefs[1]
    b1<-coefs[2] 
    b2<-coefs[3]
    b3<-coefs[4]
    b4<-coefs[5] 
    b5<-coefs[6] 
    b6<-coefs[7]
    b7<-coefs[8] 
    b8<-coefs[9]
    b9<-coefs[10]
    b10<-coefs[11] 
    
    # Edad máxima mujeres
    beta_age2_fem <- (b3+b5)
    return(beta_age2_fem )
  }
  
  Age2_fem(Model_Data2,1:nrow(Model_Data2))
  
  boot_age2_fem <- boot(Model_Data2, Age2_fem, R = 1000)
  boot_age2_fem
  coef_ed2_fem <- boot_age2_fem$t0
  se_ed2_fem <- apply(boot_age2_fem$t,2,sd)
  
  #-------------------------------------------------------------------------------
  #Gráfica para los hombres
  age_men<-function(Model_Data2,index){
    
    # Obtener coeficientes
    coefs<-lm(Log.Hourly.Wage ~ Female + Age + Age_Sqrt + Fem_Age + Fem_Age2 +
                Formal + secundaria + media + superior+ Tothour.worked,Model_Data2, subset = index)$coefficients
    
    # Colocar coeficientes en escalares 
    b0<-coefs[1]
    b1<-coefs[2] 
    b2<-coefs[3]
    b3<-coefs[4]
    b4<-coefs[5] 
    b5<-coefs[6] 
    b6<-coefs[7]
    b7<-coefs[8] 
    b8<-coefs[9]
    b9<-coefs[10]
    b10<-coefs[11]
    
    # Edad máxima de los hombres
    beta_age_men<- ((b2))
    
    
    return(beta_age_men)
    
  }
  
  age_men(Model_Data2,1:nrow(Model_Data2))
  
  boot_age_men <- boot(Model_Data2, age_men, R = 1000)
  boot_age_men
  coef_ed_men <- boot_age_men$t0
  se_ed_men<- apply(boot_age_men$t,2,sd)
  
  
  age2_men<-function(Model_Data2,index){
    
    # Obtener coeficientes
    coefs<-lm(Log.Hourly.Wage ~ Female + Age + Age_Sqrt + Fem_Age + Fem_Age2 +
                Formal + secundaria + media + superior+ Tothour.worked,Model_Data2, subset = index)$coefficients
    
    # Colocar coeficientes en escalares 
    b0<-coefs[1]
    b1<-coefs[2] 
    b2<-coefs[3]
    b3<-coefs[4]
    b4<-coefs[5] 
    b5<-coefs[6] 
    b6<-coefs[7]
    b7<-coefs[8] 
    b8<-coefs[9]
    b9<-coefs[10]
    b10<-coefs[11]
    
    # Calcular edad pico para mujeres
    beta_age2_men <- (b3)
    
    
    return(beta_age2_men)
    
  }
  
  age2_men(Model_Data2,1:nrow(Model_Data2))
  
  boot_age2_men <- boot(Model_Data2, age2_men, R = 1000)
  boot_age2_men
  coef_ed2_men <- boot_age2_men$t0
  se_ed2_men <- apply(boot_age2_men$t,2,sd)
  
  
  # Dataframe para las x e y
  
  x <- seq(18, 94, length.out = 100)
  
  
  y_fem <- 12.33 + coef_ed_fem * x + coef_ed2_fem * x^2
  y_fem_i <- 12.19 + (coef_ed_fem-1.96*se_ed_fem) * x + (coef_ed2_fem-1.96*se_ed2_fem) * x^2
  y_fem_s <- 12.47 + (coef_ed_fem+1.96*se_ed_fem) * x + (coef_ed2_fem+1.96*se_ed2_fem) * x^2
  
  
  y_men <- coef_ed_men * x + coef_ed2_men * x^2
  y_men_i <- (coef_ed_men-1.96*se_ed_men) * x + (coef_ed2_men-1.96*se_ed2_men) * x^2
  y_men_s <- (coef_ed_men+1.96*se_ed_men) * x + (coef_ed2_men+1.96*se_ed2_men) * x^2
  
  
  df <- data.frame(x, y_fem, y_fem_i, y_fem_s,y_men,y_men_i,y_men_s)
  
  # Grafico de la función para las mujeres 738
  
  ggplot(df, aes(x = x, y = y_fem)) +
    geom_line(aes(color = "Estimado"), size = 1) +
    geom_line(aes(x = x, y = y_fem_i, color = "Intervalo inferior"), linetype = "dotted", size = 1) +
    geom_line(aes(x = x, y = y_fem_s, color = "Intervalo superior"), linetype = "dotted", size = 1) +
    scale_color_manual(name = "", values = c("Estimado" = "green3", "Intervalo inferior" = "red", "Intervalo superior" = "red")) +
    labs(x = "Edad", y = "Log(Salario por hora)") +
    theme_classic() +
    scale_x_continuous(limits = c(18, 94)) +
    geom_vline(xintercept = 51, linetype = "dotted") +
    theme(legend.position = "bottom")
  
  ggplot(df, aes(x = x, y = y_men)) +
    geom_line(aes(color = "Estimado"), size = 1) +
    geom_line(aes(x = x, y = y_men_i, color = "Intervalo inferior"), linetype = "dotted", size = 1) +
    geom_line(aes(x = x, y = y_men_s, color = "Intervalo superior"), linetype = "dotted", size = 1) +
    scale_color_manual(name = "", values = c("Estimado" = "green3", "Intervalo inferior" = "red", "Intervalo superior" = "red")) +
    labs(x = "Edad", y = "Log(Salario por hora)") +
    theme_classic() +
    scale_x_continuous(limits = c(18, 94)) +
    geom_vline(xintercept = 54, linetype = "dotted") +
    theme(legend.position = "bottom")
  
  
  #---6 Predicting Earnings ############################################################

  
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
    initial_split(prop = 0.70)
  
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
       exponentiate = FALSE)
  
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
  

  M1 <- ggplot(GEIH_Test_Res_M1, 
         aes(x = Log.Hourly.Wage, 
             y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(alpha = 0.5,
               color = "#cc0000") + 
    labs(y = "Predicción del logaritmo del ingreso laboral por hora", 
         x = "Logaritmo del ingreso laboral por hora") +
    coord_obs_pred() +
    theme_classic() +
    theme(text = element_text(family = "Helvetica"))
  
  setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Big_Data_Problem_Set1/views")
  
  ggsave("M1.png", 
         M1, 
         width = 4, 
         height = 4)
    
  
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
           GEIH_Training)
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
       exponentiate = FALSE)
  
  Training_Metrics_M2 <- glance(GEIH_Fit_M2)
  
  ### Métricas de evaluación out of sample
  
  GEIH_Test_Res_M2 <- predict(
    GEIH_Fit_M2, 
    GEIH_Test %>% 
      select(-Log.Hourly.Wage)
  )
  
  GEIH_Test_Res_M2 <- bind_cols(GEIH_Test_Res_M2, 
                             GEIH_Test %>% 
                               select(Log.Hourly.Wage))
  
  
  M2 <- ggplot(GEIH_Test_Res_M2, 
               aes(x = Log.Hourly.Wage, 
                   y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(alpha = 0.5,
               color = "#cc0000") + 
    labs(y = "Predicción del logaritmo del ingreso laboral por hora", 
         x = "Logaritmo del ingreso laboral por hora") +
    coord_obs_pred() +
    theme_classic() +
    theme(text = element_text(family = "Helvetica"))
  
  setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Big_Data_Problem_Set1/views")
  
  ggsave("M2.png", 
         M2, 
         width = 4, 
         height = 4)
  
  GEIH_Metrics <- metric_set(rmse, 
                             rsq, 
                             mae)
  
  Out_Sample_Metrics_M2 <- GEIH_Metrics(GEIH_Test_Res_M2, 
               truth = Log.Hourly.Wage, 
               estimate = .pred)
  
  
  #### Tercer modelo (Log(Wage) = Sex + Controles (FWL)) #####

  ##### Regresión de Residualización #####
  
  GEIH_Recipe_R <- 
    recipe(Sex ~ Max.Educ.Level + Age + Age_Sqrt + 
             Worker.Type + Formal + Firm.Size + Job.Type, 
           Model_Data) %>% 
    step_dummy(Max.Educ.Level, Worker.Type, Firm.Size, 
               Job.Type)
  
  GEIH_Recipe_R
  
  
  ### Se especifica el modelo
  
  GEIH_Spec <- 
    linear_reg() %>% 
    set_engine("lm") %>% 
    set_mode("regression")
  
  ### Workflow
  
  GEIH_Wflow <- workflow(GEIH_Recipe_R,
                         GEIH_Spec)
  
  ### Se corre el modelo
  
  GEIH_Fit_R <- fit(GEIH_Wflow,
                     Model_Data
  )
  
  tidy(GEIH_Fit_R,
       exponentiate = FALSE)
  
  ### Calcular los residuales 
  
  GEIH_Resids <- predict(
    GEIH_Fit_R, 
    Model_Data
  )
  
  GEIH_Resids <- bind_cols(GEIH_Resids, 
                                    Model_Data)
  
  GEIH_Resids <- GEIH_Resids %>% 
    mutate(
      Resid = Sex - .pred
    )

  #### Partición de la base con residuales para el FWL #####
  
  set.seed(2403)
  
  GEIH_Split_FWL <- GEIH_Resids %>% 
    na.omit() %>% 
    initial_split(prop = 0.70)
  
  GEIH_Training_FWL <- training(GEIH_Split_FWL)
  
  GEIH_Test_FWL <- testing(GEIH_Split_FWL)
  
  
  
  #### Regresión MCO #####
  
  ### Se prepara la base de entrenamiento y se especifica la forma funcional
  
  GEIH_Recipe_M3 <- 
    recipe(Log.Hourly.Wage ~ Sex + Max.Educ.Level + Age + Age_Sqrt + 
             Worker.Type + Formal + Firm.Size + Job.Type, 
           GEIH_Training_FWL) %>% 
    step_dummy(Max.Educ.Level, Worker.Type, Firm.Size, 
               Job.Type)
  
  GEIH_Recipe_M3
  
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
                     GEIH_Training_FWL
  )
  
  GEIH_Fit_M3
  
  OLS3 <- tidy(GEIH_Fit_M3,
              exponentiate = FALSE)
  
  OLS3 <- OLS3 %>%
    mutate(p.value = round(p.value, 4))
    

  
  Training_Metrics_M3 <- glance(GEIH_Fit_M3)
  
  
  ### Métricas de evaluación out of sample
  
  GEIH_Test_Res_M3 <- predict(
    GEIH_Fit_M3, 
    GEIH_Test_FWL %>% 
      select(-Log.Hourly.Wage)
  )
  
  GEIH_Test_Res_M3 <- bind_cols(GEIH_Test_Res_M3, 
                                GEIH_Test_FWL %>% 
                                  select(Log.Hourly.Wage))
  
  
  M3 <- ggplot(GEIH_Test_Res_M3, 
               aes(x = Log.Hourly.Wage, 
                   y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(alpha = 0.5,
               color = "#004c99") + 
    labs(y = "Predicción del logaritmo del ingreso laboral por hora", 
         x = "Logaritmo del ingreso laboral por hora") +
    coord_obs_pred() +
    theme_classic() +
    theme(text = element_text(family = "Helvetica"))
  
  setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Big_Data_Problem_Set1/views")
  
  ggsave("M3.png", 
         M3, 
         width = 4, 
         height = 4)
  
  GEIH_Metrics <- metric_set(rmse, 
                             rsq, 
                             mae)
  
  Out_Sample_Metrics_M3 <- GEIH_Metrics(GEIH_Test_Res_M3, 
                                        truth = Log.Hourly.Wage, 
                                        estimate = .pred)
  
  #### Regresión FWL #####
  
  GEIH_Recipe_FWL <- 
    recipe(Log.Hourly.Wage ~ Resid, 
           GEIH_Training_FWL)
  
  GEIH_Recipe_FWL
  
  ### Se especifica el modelo
  
  GEIH_Spec <- 
    linear_reg() %>% 
    set_engine("lm") %>% 
    set_mode("regression")
  
  ### Workflow
  
  GEIH_Wflow <- workflow(GEIH_Recipe_FWL,
                         GEIH_Spec)
  
  ### Se corre el modelo
  
  GEIH_Fit_FWL <- fit(GEIH_Wflow,
                      GEIH_Training_FWL
  )
  
  FWL <- tidy(GEIH_Fit_FWL,
       exponentiate = FALSE)
  
  OLS3
  FWL # Se observa el mismo coeficiente que en el MCO con controles
  
  
  Training_Metrics_FWL <- glance(GEIH_Fit_FWL)
  
  # In sample presentamos r cuadrado mucho más bajos y AIC y BIC significativamente más altos
  
  ### Métricas de evaluación out of sample
  
  GEIH_Test_Res_FWL <- predict(
    GEIH_Fit_FWL, 
    GEIH_Test_FWL %>% 
      select(-Log.Hourly.Wage)
  )
  
  GEIH_Test_Res_FWL <- bind_cols(GEIH_Test_Res_FWL, 
                                 GEIH_Test_FWL %>% 
                                  select(Log.Hourly.Wage))
  
  
  
  M_FWL <- ggplot(GEIH_Test_Res_FWL, 
               aes(x = Log.Hourly.Wage, 
                   y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(alpha = 0.5,
               color = "#cc0000") + 
    labs(y = "Predicción del logaritmo del ingreso laboral por hora", 
         x = "Logaritmo del ingreso laboral por hora") +
    coord_obs_pred() +
    theme_classic() +
    theme(text = element_text(family = "Helvetica"))
  
  setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Big_Data_Problem_Set1/views")
  
  ggsave("M_FWL.png", 
         M_FWL, 
         width = 4, 
         height = 4)
  
  GEIH_Metrics <- metric_set(rmse, 
                             rsq, 
                             mae)
  
  Out_Sample_Metrics_FWL <- GEIH_Metrics(GEIH_Test_Res_FWL, 
                                        truth = Log.Hourly.Wage, 
                                        estimate = .pred)
  
  #### Cuarto modelo = Modelo 3 y se agrega Estrato, Reg de salud y parentezco #####
  
  ### Se prepara la base de entrenamiento y se especifica la forma funcional
  
  GEIH_Recipe_M4 <- 
    recipe(Log.Hourly.Wage ~ Sex  + Age + Age_Sqrt + Max.Educ.Level +
             + Strata + Worker.Type + Formal + Firm.Size + Kinship +
             Job.Type + HealthCare.System, 
           GEIH_Training) %>% 
    step_dummy(Max.Educ.Level, Worker.Type, Firm.Size, HealthCare.System,
               Job.Type, Strata, Kinship)
  
  GEIH_Recipe_M4
  
  ### Se especifica el modelo
  
  GEIH_Spec <- 
    linear_reg() %>% 
    set_engine("lm") %>% 
    set_mode("regression")
  
  ### Workflow
  
  GEIH_Wflow <- workflow(GEIH_Recipe_M4,
                         GEIH_Spec)
  
  ### Se corre el modelo
  
  GEIH_Fit_M4 <- fit(GEIH_Wflow,
                     GEIH_Training)
  
  GEIH_Fit_M4
  
  ## Evaluar los coeficientes
  
  OLS4 <- tidy(GEIH_Fit_M4,
               exponentiate = FALSE)

  ## Aproximar p values para mejor entendimiento
  
  OLS4 <- OLS4 %>%
    mutate(p.value = round(p.value, 7))
  

  Training_Metrics_M4 <- glance(GEIH_Fit_M4)
  
  
  ### Métricas de evaluación out of sample
  
  GEIH_Test_Res_M4 <- predict(
    GEIH_Fit_M4, 
    GEIH_Test %>% 
      select(-Log.Hourly.Wage)
  )
  
  GEIH_Test_Res_M4 <- bind_cols(GEIH_Test_Res_M4, 
                                GEIH_Test %>% 
                                  select(Log.Hourly.Wage))
  
  
  M4 <- ggplot(GEIH_Test_Res_M4, 
               aes(x = Log.Hourly.Wage, 
                   y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(alpha = 0.5,
               color = "#004c99") + 
    labs(y = "Predicción del logaritmo del ingreso laboral por hora", 
         x = "Logaritmo del ingreso laboral por hora") +
    coord_obs_pred() +
    theme_classic() +
    theme(text = element_text(family = "Helvetica"))
  
  setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Big_Data_Problem_Set1/views")
  
  ggsave("M4.png", 
         M4, 
         width = 4, 
         height = 4)
  
  
  GEIH_Metrics <- metric_set(rmse, 
                             rsq, 
                             mae)
  
  Out_Sample_Metrics_M4 <- GEIH_Metrics(GEIH_Test_Res_M4, 
                                        truth = Log.Hourly.Wage, 
                                        estimate = .pred)
  
  
  #### Quinto modelo = Modelo 4 y se agregan interacciones entre edad, sexo, estrato #####
  
  ### Se prepara la base de entrenamiento y se especifica la forma funcional
  
  GEIH_Recipe_M5 <- 
    recipe(Log.Hourly.Wage ~ Sex  + Age + Age_Sqrt + Max.Educ.Level +
             + Strata + HealthCare.System + Kinship +
             Worker.Type + Formal + Firm.Size + Job.Type , 
           GEIH_Training) %>% 
    step_dummy(Max.Educ.Level, Worker.Type, Firm.Size, 
               Job.Type, Strata, HealthCare.System, Kinship) %>% 
    step_interact(~ Age:Sex + Age:starts_with("Strata") + Sex:starts_with("Strata"))
  
  GEIH_Recipe_M5
  
  ## Revisar si la base de entrenamiento se está preparando bien
  
  Zoom <- juice(prep(GEIH_Recipe_M5))
  
  ### Se especifica el modelo
  
  GEIH_Spec <- 
    linear_reg() %>% 
    set_engine("lm") %>% 
    set_mode("regression")
  
  ### Workflow
  
  GEIH_Wflow <- workflow(GEIH_Recipe_M5,
                         GEIH_Spec)
  
  ### Se corre el modelo
  
  GEIH_Fit_M5 <- fit(GEIH_Wflow,
                     GEIH_Training)
  
  GEIH_Fit_M5
  
  ## Evaluar los coeficientes
  
  OLS5 <- tidy(GEIH_Fit_M5,
               exponentiate = FALSE)
  
  ## Aproximar p values para mejor entendimiento
  
  OLS5 <- OLS5 %>%
    mutate(p.value = round(p.value, 7))
  
  
  Training_Metrics_M5 <- glance(GEIH_Fit_M5)
  
  
  ### Métricas de evaluación out of sample
  
  GEIH_Test_Res_M5 <- predict(
    GEIH_Fit_M5, 
    GEIH_Test %>% 
      select(-Log.Hourly.Wage)
  )
  
  GEIH_Test_Res_M5 <- bind_cols(GEIH_Test_Res_M5, 
                                GEIH_Test %>% 
                                  select(Log.Hourly.Wage))
  
  
  
  M5 <- ggplot(GEIH_Test_Res_M5, 
               aes(x = Log.Hourly.Wage, 
                   y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(alpha = 0.5,
               color = "#004c99") + 
    labs(y = "Predicción del logaritmo del ingreso laboral por hora", 
         x = "Logaritmo del ingreso laboral por hora") +
    coord_obs_pred() +
    theme_classic() +
    theme(text = element_text(family = "Helvetica"))
  
  setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Big_Data_Problem_Set1/views")
  
  ggsave("M5.png", 
         M5, 
         width = 4, 
         height = 4)
  
  GEIH_Metrics <- metric_set(rmse, 
                             rsq, 
                             mae)
  
  Out_Sample_Metrics_M5 <- GEIH_Metrics(GEIH_Test_Res_M5, 
                                        truth = Log.Hourly.Wage, 
                                        estimate = .pred)

  
  #### Sexto modelo = Modelo 4 y se agregan interacciones entre sexo y educacion y tipo de trabajador #####
  
  ### Se prepara la base de entrenamiento y se especifica la forma funcional
  
  GEIH_Recipe_M6 <- 
    recipe(Log.Hourly.Wage ~ Sex  + Age + Age_Sqrt + Max.Educ.Level +
             + Strata + HealthCare.System + Kinship +
             Worker.Type + Formal + Firm.Size + Job.Type , 
           GEIH_Training) %>% 
    step_dummy(Max.Educ.Level, Worker.Type, Firm.Size, 
               Job.Type, Strata, HealthCare.System, Kinship) %>% 
    step_interact(~ Sex:starts_with("Worker") + Sex:starts_with("Max.Educ.Level") + Sex:starts_with("Job") + Age:starts_with("Job"))
  
                    
  
  # Dado que no hubo mejoras en el RMSE out of sample con el modelo anterior, no se meten dejan esas interacciones pasadas
  
  GEIH_Recipe_M6
  
  ## Revisar si la base de entrenamiento se está preparando bien
  
  Zoom <- juice(prep(GEIH_Recipe_M6))
  
  ### Se especifica el modelo
  
  GEIH_Spec <- 
    linear_reg() %>% 
    set_engine("lm") %>% 
    set_mode("regression")
  
  ### Workflow
  
  GEIH_Wflow <- workflow(GEIH_Recipe_M6,
                         GEIH_Spec)
  
  ### Se corre el modelo
  
  GEIH_Fit_M6 <- fit(GEIH_Wflow,
                     GEIH_Training)
  
  GEIH_Fit_M6
  
  ## Evaluar los coeficientes
  
  OLS6 <- tidy(GEIH_Fit_M6,
               exponentiate = FALSE)
  
  ## Aproximar p values para mejor entendimiento
  
  OLS6 <- OLS6 %>%
    mutate(p.value = round(p.value, 7))
  
  
  Training_Metrics_M6 <- glance(GEIH_Fit_M6)
  
  
  ### Métricas de evaluación out of sample
  
  GEIH_Test_Res_M6 <- predict(
    GEIH_Fit_M6, 
    GEIH_Test %>% 
      select(-Log.Hourly.Wage)
  )
  
  GEIH_Test_Res_M6 <- bind_cols(GEIH_Test_Res_M6, 
                                GEIH_Test %>% 
                                  select(Log.Hourly.Wage))
  
  
  M6 <- ggplot(GEIH_Test_Res_M6, 
               aes(x = Log.Hourly.Wage, 
                   y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(alpha = 0.5,
               color = "#004c99") + 
    labs(y = "Predicción del logaritmo del ingreso laboral por hora", 
         x = "Logaritmo del ingreso laboral por hora") +
    coord_obs_pred() +
    theme_classic() +
    theme(text = element_text(family = "Helvetica"))
  
  setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Big_Data_Problem_Set1/views")
  
  ggsave("M6.png", 
         M6, 
         width = 4, 
         height = 4)
  
  
  GEIH_Metrics <- metric_set(rmse, 
                             rsq, 
                             mae)
  
  Out_Sample_Metrics_M6 <- GEIH_Metrics(GEIH_Test_Res_M6, 
                                        truth = Log.Hourly.Wage, 
                                        estimate = .pred)
  
  
  #### Séptimo modelo = Modelo 6 y se agregan polinomios de la edad #####
  
  ### Exploración gráficamente los posibles polinomios
  
  plot_smoother <- function(deg_free) {
    ggplot(GEIH_Training, aes(x = Age, y = Log.Hourly.Wage)) + 
      geom_point(alpha = .1) + 
      geom_smooth(
        method = lm,
        formula = y ~ ns(x, df = deg_free),
        color = "#66b2ff",
        se = FALSE
      ) +
      labs(x = "Edad",
           y = "Logaritmo del Ingreso Laboral por Hora")
  } +
    theme_classic()
  
  p1 <- plot_smoother(4)
  p2 <- plot_smoother(30)
  
  Sp <- grid.arrange(p1, p2, ncol = 2)
  
  setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Big_Data_Problem_Set1/views")
  
  ggsave("Splines.png", 
         Sp, 
         width = 7, 
         height = 4)
  
  ### Se prepara la base de entrenamiento y se especifica la forma funcional
  
  GEIH_Recipe_M7 <- 
    recipe(Log.Hourly.Wage ~ Sex  + Age + Age_Sqrt + Max.Educ.Level +
             + Strata + HealthCare.System + Kinship +
             Worker.Type + Formal + Firm.Size + Job.Type , 
           GEIH_Training) %>% 
    step_dummy(Max.Educ.Level, Worker.Type, Firm.Size, 
               Job.Type, Strata, HealthCare.System) %>% 
    step_ns(Age, deg_free = 4) 


  GEIH_Recipe_M7
  
  ## Revisar si la base de entrenamiento se está preparando bien
  
  Zoom <- juice(prep(GEIH_Recipe_M7))
  
  ### Se especifica el modelo
  
  GEIH_Spec <- 
    linear_reg() %>% 
    set_engine("lm") %>% 
    set_mode("regression")
  
  ### Workflow
  
  GEIH_Wflow <- workflow(GEIH_Recipe_M7,
                         GEIH_Spec)
  
  ### Se corre el modelo
  
  GEIH_Fit_M7 <- fit(GEIH_Wflow,
                     GEIH_Training)

  GEIH_Fit_M7
  
  ## Evaluar los coeficientes
  
  OLS7 <- tidy(GEIH_Fit_M7,
               exponentiate = FALSE)
  
  ## Aproximar p values para mejor entendimiento
  
  OLS7 <- OLS7 %>%
    mutate(p.value = round(p.value, 7))
  
  
  Training_Metrics_M7 <- glance(GEIH_Fit_M7)
  
  
  ### Métricas de evaluación out of sample
  
  GEIH_Test_Res_M7 <- predict(
    GEIH_Fit_M7, 
    GEIH_Test %>% 
      select(-Log.Hourly.Wage)
  )
  
  GEIH_Test_Res_M7 <- bind_cols(GEIH_Test_Res_M7, 
                                GEIH_Test %>% 
                                  select(Log.Hourly.Wage))
  
  
  ggplot(GEIH_Test_Res_M7, 
         aes(x = Log.Hourly.Wage, 
             y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(alpha = 0.5) + 
    labs(y = "Predicción del logaritmo del ingreso laboral por hora", x = "Logaritmo del ingreso laboral por hora") +
    # Scale and size the x- and y-axis uniformly:
    coord_obs_pred()
  
  GEIH_Metrics <- metric_set(rmse, 
                             rsq, 
                             mae)
  
  Out_Sample_Metrics_M7 <- GEIH_Metrics(GEIH_Test_Res_M7, 
                                        truth = Log.Hourly.Wage, 
                                        estimate = .pred)
  
  
  #### Octavo modelo = Modelo 7 y se corre un Lasso para hacer selección de variables #####
  
  GEIH_Recipe_M8 <- 
    recipe(Log.Hourly.Wage ~ Sex  + Age + Age_Sqrt + Max.Educ.Level +
             + Strata + HealthCare.System + Kinship +
             Worker.Type + Formal + Firm.Size + Job.Type , 
           GEIH_Training) %>% 
    step_dummy(Max.Educ.Level, Worker.Type, Firm.Size, Kinship,
               Job.Type, Strata, HealthCare.System) %>% 
    step_interact(~ Age:Sex + Age:starts_with("Strata") + Sex:starts_with("Strata")) %>% 
    step_normalize(Age, Age_Sqrt)
  
  
  GEIH_Recipe_M8
  
  ## Revisar si la base de entrenamiento se está preparando bien
  
  Zoom <- juice(prep(GEIH_Recipe_M8))
  
  
  ### Workflow
  
  GEIH_Wflow <- workflow(GEIH_Recipe_M8
                         )
  
  ### Optimización del lambda en función del RMSE
  
  set.seed(2403)
  
  GEIH_Boots <- bootstraps(GEIH_Training, 
                            strata = Log.Hourly.Wage)
  
  Tune_Spec <- linear_reg(penalty = tune(), 
                          mixture = 1) %>%
    set_engine("glmnet")
  
  Lambda_grid <- head(grid_regular(penalty(), 
                                   levels = 50))
  
  ## Se corre el resampleo
  
  doParallel::registerDoParallel()
  
  set.seed(2403)
  Lasso_grid <- tune_grid(
    GEIH_Wflow %>% 
      add_model(Tune_Spec),
    resamples = GEIH_Boots,
    grid = Lambda_grid
  )
  
  # Se evaluan los resultados obtenidos
  
  Lasso_grid %>% 
    collect_metrics()
  
  Lasso_grid %>%
    collect_metrics() %>%
    ggplot(aes(penalty, 
               mean, 
               color = .metric)) +
    geom_errorbar(aes(
      ymin = mean - std_err,
      ymax = mean + std_err
    ),
    alpha = 0.5
    ) +
    geom_line(size = 1.5) +
    facet_wrap(~.metric, scales = "free", nrow = 2) +
    scale_x_log10() +
    theme_classic() +
    theme(legend.position = "none")
  
  
  
  # Se selecciona el Lambda con el RMSE más bajo
  
  Lowest_RMSE <- Lasso_grid %>%
    select_best("rmse")
  
  # EL proceso de optimización de lamba sugiere un valor cercano a cero,
  # es decir, casi igual a correr un OLS
  
  ### Se especifica el modelo con el lambda optimizado
  
  GEIH_Spec_Lasso <- 
    linear_reg(penalty = 0.0013, mixture = 1) %>% 
    set_engine("glmnet") %>% 
    set_mode("regression")
  
  ### Se monta al workflow el modelo con el lambda optimizado 
  
  GEIH_Wflow <- workflow(GEIH_Recipe_M8,
                         GEIH_Spec_Lasso)
  
  
  ### Se corre el modelo
  
  GEIH_Fit_M8 <- fit(GEIH_Wflow,
                     GEIH_Training)
  
  GEIH_Fit_M8
  
  ## Evaluar los coeficientes
  
  Lasso <- tidy(GEIH_Fit_M8,
               exponentiate = FALSE)
  
  
  ### Métricas de evaluación out of sample
  
  GEIH_Test_Res_M8 <- predict(
    GEIH_Fit_M8, 
    GEIH_Test %>% 
      select(-Log.Hourly.Wage)
  )
  
  GEIH_Test_Res_M8 <- bind_cols(GEIH_Test_Res_M8, 
                                GEIH_Test %>% 
                                  select(Log.Hourly.Wage))
  
  
  ggplot(GEIH_Test_Res_M8, 
         aes(x = Log.Hourly.Wage, 
             y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(alpha = 0.5) + 
    labs(y = "Predicción del logaritmo del ingreso laboral por hora", x = "Logaritmo del ingreso laboral por hora") +
    # Scale and size the x- and y-axis uniformly:
    coord_obs_pred()
  
  GEIH_Metrics <- metric_set(rmse, 
                             rsq, 
                             mae)
  
  Out_Sample_Metrics_M8 <- GEIH_Metrics(GEIH_Test_Res_M8, 
                                        truth = Log.Hourly.Wage, 
                                        estimate = .pred)
  
  
  ##### Métricas de evaluación #####
  
# Se consolidan todas las métricas in sample en un data frame
  
  Training_Metrics <- rbind(Training_Metrics_M1,
                            Training_Metrics_M2,
                            Training_Metrics_M3,
                            Training_Metrics_FWL,
                            Training_Metrics_M4,
                            Training_Metrics_M5,
                            Training_Metrics_M6,
                            Training_Metrics_M7
                            )
  
  Training_Metrics_F <- Training_Metrics %>% 
    select(1,2, 8, 9) %>% 
    mutate(
      ID = c("Modelo 1", "Modelo 2", "Modelo 3",
             "Modelo 4", "Modelo 5", "Modelo 6", 
             "Modelo 7", "Modelo 8"),
      r.squared = round(r.squared*100, 2),
      adj.r.squared = round(adj.r.squared*100, 2)
    )
  
  
  Test_Metrics <- rbind(Out_Sample_Metrics_M1,
                            Out_Sample_Metrics_M2,
                            Out_Sample_Metrics_M3,
                            Out_Sample_Metrics_FWL,
                            Out_Sample_Metrics_M4,
                            Out_Sample_Metrics_M5,
                            Out_Sample_Metrics_M6,
                            Out_Sample_Metrics_M7,
                            Out_Sample_Metrics_M8)
  
  Test_Metrics_F <- Test_Metrics %>% 
    filter(.metric == "rmse") %>% 
    mutate(
    ID = c("Modelo 1", "Modelo 2", "Modelo 3",
           "Modelo FWL", "Modelo 4", "Modelo 5", 
           "Modelo 6", "Modelo 7", "Modelo 8"))
  
  
  
  
  
  
  ##### Validación Cruzada (LOOCV y K Fold)#####
  
  
  ### LOOCV #####
  
  #ATENCIÓN: Este modelo no se corre por ser muy costoso en términos de tiempo
  # No solo por lo que implica a nivel computacional sino para dumificar e incluir todas las 
  # categorias de las variables una por una en la regresión.
  # Se dejará explicita la forma en que se desarrollaría, pero se correrá 
  # un procedimiento Kfold con tidymodels para aproximarse a los resultados

  
  # Se define la función para correr una validación cruzada tipo LOOCV de forma manual

  lm_model <- function(data, idx) {
    LOOCV_Training <- data[-idx, ]
    LOOCV_Test <- data[idx, ]
    fit <- lm(Log.Hourly.Wage ~ Sex  + Age + Age_Sqrt + Max.Educ.Level +
                + Strata + HealthCare.System + Kinship + Job.Type
                Worker.Type + Formal + Firm.Size,
                data = LOOCV_Training) # Cabe resaltar que las variables categoricas deben ser dumificadas e incluídas todas menos una. 
    pred <- predict(fit, newdata = LOOCV_Test) 
    return(pred)
  }
  
  # Se corre el LOOCV
  
  Predictions <- map_dbl(1:nrow(LOOCV_Data), function(i) {
    lm_model(LOOCV_Data, i)
  })
  
  ### K Fold #####
  
  ## Se prepara nuestra base de datos para la validación cruzada ######
  
  set.seed(2403)
  
  GEIH_Split <- Model_Data %>% 
    na.omit() %>% 
    initial_split(strata = Log.Hourly.Wage)
  
  GEIH_Training <- training(GEIH_Split)
  
  GEIH_Test <- testing(GEIH_Split)
  
  set.seed(240394)
  
  GEIH_Folds <- vfold_cv(GEIH_Training, 
                         strata = Log.Hourly.Wage)
  
  GEIH_Folds
  
  
  ### Se prepara la base de entrenamiento y se especifica la forma funcional
  
  GEIH_Recipe_M5_KFOLD <- 
    recipe(Log.Hourly.Wage ~ Sex  + Age + Age_Sqrt + Max.Educ.Level +
             + Strata + HealthCare.System + Kinship +
             Worker.Type + Formal + Firm.Size + Job.Type , 
           GEIH_Training) %>% 
    step_dummy(Max.Educ.Level, Worker.Type, Firm.Size, 
               Job.Type, Strata, HealthCare.System, Kinship) %>% 
    step_interact(~ Age:Sex + Age:starts_with("Strata") + Sex:starts_with("Strata"))
  
  GEIH_Recipe_M5_KFOLD
  
  ## Revisar si la base de entrenamiento se está preparando bien
  
  Zoom <- juice(prep(GEIH_Recipe_M5_KFOLD))
  
  ### Se especifica el modelo
  
  Lm_Spec <- 
    linear_reg() %>% 
    set_engine("lm") %>% 
    set_mode("regression")
  
  RF_spec <-
    rand_forest(trees = 1e3) %>%
    set_mode("regression") %>%
    set_engine("ranger")
  
  ### Workflow
  
  GEIH_Wflow <- workflow_set(
    list(GEIH_Recipe_M5_KFOLD, GEIH_Recipe_M5_KFOLD),
                             list(Lm_Spec, RF_spec),
                             cross = FALSE)
  
  
  ### Se corre el modelo
  
  doParallel::registerDoParallel()
  set.seed(2023)
  
  GEIH_RS <-
    workflow_map(
      GEIH_Wflow,
      "fit_resamples",
      resamples = GEIH_Folds
    )
  
  GEIH_RS
  
  ## Evaluar los modelos (Se adiciona un Random Forest para fines comparativos)
  
  # Se plotean contra el RSME y RSQ
  
  Metrics_Plot <- autoplot(GEIH_RS)
  
  ggsave("KFold.png", 
         Metrics_Plot, 
         width = 7, 
         height = 4)
  
  # Se revisan las métricas
  
  Metrics_KFOLD <- collect_metrics(GEIH_RS)
  
  # Se extrae el mejor modelo para volver a ajustar con la base de entrenamiento
  # A pesar de que el Random Forest tuvo un mejor desempeño, 
  # solo fue incluido con fines comparativos, se toma el MCO.
  
  Final_Fit <-
    extract_workflow(GEIH_RS, 
                     "recipe_1_linear_reg") %>%
    fit(GEIH_Training)
  
  # 
  
  tidy(Final_Fit) %>%
    arrange(-abs(estimate))
  
  Training_Metrics_M5_KFOLD <- glance(Final_Fit)
  
  
  ### Métricas de evaluación out of sample
  
  GEIH_Test_Res_M5_KFOLD <- predict(
    Final_Fit, 
    GEIH_Test %>% 
      select(-Log.Hourly.Wage)
  )
  
  GEIH_Test_Res_M5_KFOLD <- bind_cols(GEIH_Test_Res_M5_KFOLD, 
                                GEIH_Test %>% 
                                  select(Log.Hourly.Wage))
  
  
  M5_KFOLD <- ggplot(GEIH_Test_Res_M5_KFOLD, 
               aes(x = Log.Hourly.Wage, 
                   y = .pred)) + 
    # Create a diagonal line:
    geom_abline(lty = 2) + 
    geom_point(alpha = 0.5,
               color = "#004c99") + 
    labs(y = "Predicción del logaritmo del ingreso laboral por hora", 
         x = "Logaritmo del ingreso laboral por hora") +
    coord_obs_pred() +
    theme_classic() +
    theme(text = element_text(family = "Helvetica"))
  
  setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Big_Data_Problem_Set1/views")
  
  ggsave("M5_KFOLD.png", 
         M5_KFOLD, 
         width = 4, 
         height = 4)
  
  GEIH_Metrics <- metric_set(rmse, 
                             rsq, 
                             mae)
  
  Out_Sample_Metrics_M5_KFOLD <- GEIH_Metrics(GEIH_Test_Res_M5_KFOLD, 
                                        truth = Log.Hourly.Wage, 
                                        estimate = .pred)
  
  