#Creación del modelo logistico final
#creacion de variables de trabajo recodificandolas 
#determinación de que variables son significativas en el modelo por país
#Partimos del modelo con el total de variables y mediante la función STEP, llegamos
#al modelo definitivo que será el empleado en el paso 3
# Manipulación de las variables con valor NA
#las variables sd1 y sd3 solo se usan en los conceptos que son significativos para este trabajo
#no podemos aplicar NA, ya que un mismo encuestado puede tener diferentes alternativas en sd1 y sd3.

#citacion stargazer

#Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics
#Tables. R package version 5.2.2. https://CRAN.R-project.org/package=stargazer

#ojo definicion del diretorio de trabajo de R


a<-"C:/Users/amart/Documents/UOC/2020_2/TFM/Eurobarometro_FS_464"
setwd(a)

#librerias a cargar

library(glm.predict)
library(glm2)
library(glmc)
library(glmm)
library(glmmboot)
library(dplyr)
library(tidyr)
library(haven)
library(Rcmdr)
library(car)
library(ggplot2) #, pos = 19)
library(abind) #, pos = 19)
library(gmodels)
library(e1071) #, pos = 22)
library(lattice)
library(survival)
library(Formula)
library(Hmisc)
library(catspec)
library(stargazer)
library(nnet)
library(texreg)
library(reshape2)
library(MASS)
library(mgcv)
library(oglmx)
library(AER) 
library(ggmosaic)
library(mgcv)
library(blorr)


load("global.RData")



#la variable idgov no se incluye en la formula glm.
#una vez obtenida el glm r se hara una regresion lineal entre el valor de la variable
#q2 (numero de veces que se detectan desinformaciones) y la variable idgov. 
#saldra una función del tipo y = alfa beta X, donde X es idgov.
#hay que tratar idgov la distancia en abosulot entre el valor y el del punto del elector mediano.
#para calcular el valor mediano hacer la mediana del idgov de todos los paise. 
#la formula usará abs(idgov-mediana)
#si el valor de alfa y la beta calculada es positiva entonces hay una correlación positiva. Aumenta
#lo mismo se hará con la variable idcom para España

#eliminar NA de un factor
#d1 edad, d2 sexo, d4r2 fin estudios, q2 veces que encuentras desinformación, 
#d1 + d2 + q2 + d4r2 + q3 + d13 + sd2 +
#  q1_1 + q1_2 + q1_3 + q1_4 + q1_5 + q1_6 +
#  sd1.1 + sd1.2 + sd1.3 + sd1.4 + sd1.5 + sd1.6 + sd1.7 + sd1.8,

datos1 <- global

datos1 <- datos1[!is.na(datos1$q4_2BIN),]

datos1 <- datos1[!is.na(datos1$d2),]

datos1 <- datos1[!is.na(datos1$d4r2),]

datos1 <- datos1[!is.na(datos1$q2),]

datos1 <- datos1[!is.na(datos1$q3),]

datos1 <- datos1[!is.na(datos1$d13),]

datos1 <- datos1[!is.na(datos1$sd2),]
#median idgov es la posición del gobierno mediano europeo y lo ponemos en veces
veces <- median(datos1$idgov)
#determinar que variables son necesarias 

datos1$sobre <- abs(datos1$idgov - veces)

datos1$uso1 <- (as.numeric(datos1$sd3.1n)-1)
datos1$uso2 <- (as.numeric(datos1$sd3.2n)-1) 
datos1$uso3 <- (as.numeric(datos1$sd3.3n)-1) 
datos1$suma_sd3=datos1$uso1+datos1$uso2+datos1$uso3

datos1$user2 <- (as.numeric(datos1$sd1.2n)-1) 
datos1$user4 <- (as.numeric(datos1$sd1.4n)-1) 
datos1$user8 <- (as.numeric(datos1$sd1.8n)-1) 
datos1$suma_sd1=datos1$user2+datos1$user4+datos1$user8


save("datos1", file = "datos1.RData")


#global
datosp <-datos1

#"AT","BE","BG","CY","CZ", "DE","EE",
#"DK","EE","ES","FI","FR","GB", "GR",
#"HR", "HU","IE","IT","LT", "LU",
#"LV","MT","NL""PL","PT","RO",
#"SE","SI", "SK"

#cy, ee, hu, IE, NL, SI, SK da error. Lo quito


sink(file="resultados.txt") 

for (val in c("AT","BE","BG","CY","CZ", "DE","EE",
              "DK","EE","ES","FI","FR","GB", "GR",
              "HR", "HU","IE","IT","LT", "LU",
              "LV","MT","NL","PL","PT","RO",
              "SE","SI", "SK","SI",
              "GLOBAL")) 
  #bucle principal para todos los países. Se guarda en fichero txt.

{
#para tratar el fichero globalmente utilizo Val = "GLOBAL" con if

  #val<-"AT"

   if (val == "GLOBAL")
  { datosp <- datos1}
  else 
  { datosp <- filter(datos1, isocntry == val)}
  
  #sink(file=(paste("reg_binomial",val,".txt", sep="")))   
  
  print(val) 

  mbg1 <- glm(q4_2BIN ~  d1 + d2 + q2 + d4r2 + q3 + d13 + sd2 +
                 uso1 + uso2 + uso3 + user2 + user4 + user8,
                 family="binomial"(link = "logit"),data=datosp)

  # p-value
  p_valuembg1 <- 1-pchisq(mbg1$deviance,mbg1$df.residual)

  print(paste("p_value Modelo Inicial: ",p_valuembg1,sep=""))

  #si p_valuemodelo >0,05 enotcees se acepta el modelo como bueno.
  #El valor de dicho pvalor es superior a 0.05 indicando que el modelo considerado tiene 
  #una buena capacidad explicativa. 
  
  #Details
  #Collinearity implies two variables are near perfect linear combinations of one another. Multicollinearity involves more than two variables. In the presence of multicollinearity, regression estimates are unstable and have high standard errors.
  #Tolerance
  #Percent of variance in the predictor that cannot be accounted for by other predictors.
  #Variance Inflation Factor
  #Variance inflation factors measure the inflation in the variances of the parameter estimates due to
  #collinearities that exist among the predictors. It is a measure of how much the variance of the
  #estimated regression coefficient βk is inflated by the existence of correlation among the predictor
  #variables in the model. A VIF of 1 means that there is no correlation among the kth predictor and
  #the remaining predictor variables, and hence the variance of βk is not inflated at all. The general
  #rule of thumb is that VIFs exceeding 4 warrant further investigation, while VIFs exceeding 10 are
  #signs of serious multicollinearity requiring correction.
  #Condition Index
  #Most multivariate statistical approaches involve decomposing a correlation matrix into linear combinations of variables. The linear combinations are chosen so that the first combination has the
  #largest possible variance (subject to some restrictions), the second combination has the next largest
  #variance, subject to being uncorrelated with the first, the third has the largest possible variance,
  #subject to being uncorrelated with the first and second, and so forth. The variance of each of these
  #linear combinations is called an eigenvalue. Collinearity is spotted by finding 2 or more variables
  #that have large proportions of variance (.50 or more) that correspond to large condition indices. A
  #rule of thumb is to label as large those condition indices in the range of 30 or larger.

#determinar cual es el mejor si el anterior o este modelo


#En el punto siguiente estudiaremos la selección de variables para quedarnos con 
#el mejor modelo posible. Es equivalente a STEPAIC en el ologit
  
  mfinal <- step(mbg1,direction = "both",trace=0)
  print("Modelo final")
  print(formula(mfinal))
  p_valuemfinal <- 1-pchisq(mfinal$deviance,mfinal$df.residual)
  print(paste("p_value Modelo Final: ",p_valuemfinal,sep=""))
  
  stargazer(mbg1, mfinal, title=paste("País: ",val,".  Resultados modelos inicial y final", sep=""),
            align=TRUE, type="text",
            dep.var.labels=c("Percepción afectación democracia"),
            column.labels=c("Modelo Inicial","Modelo Final"),
            omit.stat=c("ser","f"), 
            no.space=TRUE, 
            ci=TRUE, ci.level=0.90, single.row=TRUE)
  
#ha eliminado variables?
  print(val)
  print("Estadísticos multivariante Modelo inicial y modelo final")
  print(blr_multi_model_fit_stats(mbg1, mfinal))
  print(" ")
  print(" ")
  
  #sink()

}

sink()
