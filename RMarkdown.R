#Creación del modelo logistico final
#creacion de variables de trabajo recodificandolas 
#determinación de que variables son significativas en el modelo por país
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
library(PMCMR)




load("global.RData")




#eliminar NA de un factor
#d1 edad, d2 sexo, d4r2 fin estudios, q2 veces que encuentras desinformación, q3 confianza, sd2  veces que usas la red

datos1 <- global

datos1 <- datos1[!is.na(datos1$q4_2ORD),]

datos1 <- within(datos1, {
  percepcion <- as.numeric(as.character(q4_2ORD))
})


datos1 <- datos1[!is.na(datos1$q3),]

datos1 <- within(datos1, {
  q3_ORD <- Recode(q3, "c(\"Not at all confident\" ) =  0;c( \"Not very confident\" ) =  1;
                    c( \"Somewhat confident\" ) =  2;c( \"Very confident\" ) =  3 ;", 
       as.factor = TRUE)})

datos1 <- within(datos1, {
confianza <- as.numeric(as.character(q3_ORD))
})

datos1 <- datos1[!is.na(datos1$d2),]

datos1 <- within(datos1, {
  d2ORD <- Recode(d2, "c(\"Female\" ) =  0;c( \"Male\" ) =  1 ;", 
                  as.factor = TRUE)
})
datos1 <- within(datos1, {
  sexo <- as.numeric(as.character(d2ORD))
})


datos1 <- datos1[!is.na(datos1$q2),]

datos1 <- within(datos1, {
  q2ORD <- Recode(q2, "c(\"Seldom or Never\" ) =  0;c( \"Several times a month\" ) =  1;
                  c( \"At least once a week\" ) =  2;c( \"Every day or almost everyday\" ) =  3 ;", 
                  as.factor = TRUE)
})

datos1 <- within(datos1, {
  deteccion <- as.numeric(as.character(q2ORD))
})

datos1 <- datos1[!is.na(datos1$d4r2),]
datos1 <- within(datos1, {
  d4r2ORD <- Recode(d4r2, "c(\"No full-time education\" ) =  0;c( \"Up to 15\" ) =  1;
                  c( \"16-19\" ) =  2;c( \"20 years and older\" ) =  3;c( \"Still Studying\" ) =  4 ;", 
                  as.factor = TRUE)
})

datos1 <- within(datos1, {
  estudios <- as.numeric(as.character(d4r2ORD))
})

datos1 <- datos1[!is.na(datos1$sd2),]
datos1 <- within(datos1, {
  sd2ORD <- Recode(sd2, "c(\"Seldom or Never\" ) =  0;c( \"Several times a month\" ) =  1;
                  c( \"At least once a week\" ) =  2;c( \"Every day or almost everyday\" ) =  3 ;", 
                  as.factor = TRUE)
})

datos1 <- within(datos1, {
  veces_uso <- as.numeric(as.character(sd2ORD))
})

datos1 <- within(datos1, {
  ideologia <- as.numeric(as.character(idgov))
})

datos1 <- within(datos1, {
  edad <- d1
})

datos1 <- within(datos1, {
 sobreexp <-  (veces_uso * deteccion) 
})

save("datos1", file = "datos1.RData")


#global
datosp <-datos1

#tabla contingencia global q4_2 e ideologia del gobierno
#leer por filas y comparar las columnas de cada fila
#el componente del chi cuadrado es la composició del valor del Chi quadrado como sale su suma
#supongamos, por ejemplo el valor NO, definitley not para la opción 4 es 0,47
#eso significa que si el Chi quadrado es 200, solo esa casilla tiene un peso de 0,47 en la composición del 200;
#y por elejmeplo el valor Yes, to some extent para la opción 7 tiene un peso de 20,67 
#la suma de todas las casillas dará 200

#austria 8, 6 belgica, 7 bulgaria, 4 rumania, 5 alemania
#val <- "RO"
#ç

datosp <- filter(datos1, isocntry %in% c("AT", "BE", "BG", "DE", "RO"))

local({
  .Table <- xtabs(~q4_2 + idgov, data = datosp)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nTotal percentages:\n")
  .Table_1 <-totPercents(.Table)
  print(totPercents(.Table))
  .Test <- chisq.test(.Table, correct = FALSE)
  print(.Test)
  cat("\nChi-square components:\n")
  print(round(.Test$residuals^2, 2))
})
datosp <- datos1

#sink(file="mf_lmcopiarv4.txt")


for (val in c("AT","BE","BG","CY","CZ", "DE",
              "DK","EE","ES","FI","FR","GB", "GR",
              "HR", "HU","IE","IT","LT", "LU",
              "LV","MT","NL","PL","PT","RO",
              "SE","SI", "SK",
              "GLOBAL")) 

{

  #para tratar el fichero globalmente utilizo Val = "GLOBAL" con if

#val <- "GLOBAL" 
  
if (val == "GLOBAL")
{ pais <- "GLOBAL"
  datosp <- datos1}
  else 
  { datosp <- filter(datos1, isocntry == val)
    pais <- datosp$country
  }
  
 
#print(val) 
#print(pais)
#prueba
 
  
  mbg1 <- lm(percepcion ~  edad  + sexo + veces_uso + confianza + deteccion + estudios, weights = wex,
          data=datosp)

#print (summary(mbg1))

 # ci_1 <- confint(mbg1)
#  ci_1

#correlaciones
  
 # cor(datosp[, c("confianza", "edad", "sexo", "deteccion", "estudios", "veces_uso")], use = "complete")

     stargazer(mbg1, title=paste("País: ",pais,".  Resultados modelo", sep=""),
               align=TRUE, type="text",
               dep.var.labels=c("Percepción problema para la democracia"),
               column.labels=c("Modelo"),
               ci=TRUE,
               omit.stat = NULL,
               omit.summary.stat = NULL,
             # omit.stat=c("all"), 
               no.space=TRUE, 
               single.row=TRUE)
     
     
#summary(mbg1)

}

#sink()

#GLOBAL
#con la variable ideologia para la hipotesis 3

datosp <- datos1

#ideologia a nivel global

boxplot(percepcion ~ idgov, col = c("yellow", "blue", "white","green", "red"),
        xlab="Ideología gobierno (1-extrema izquierda, 9 -extrema derecha)",
        ylab = "Percepción", data=datosp)

with(datos1, (tapply(percepcion, idgov, mean)))
fm = aov( lm(percepcion ~ idgov, data=datos1) )

summary(fm)
names(fm)

#Identifica en la tabla ANOVA los grados de libertad del factor, 
#los grados de libertad residuales, la suma de cuadrados de los grupos, 
#la suma de cuadrados del error, las medias correspondientes de las sumas de cuadrados 
#de los grupos y del error, el valor del estadístico F. 
#Describe cómo obtenemos cada uno de ellos.

#¿Cuál es el valor crítico de F bajo la hipótesis nula con un nivel 
#de significación alfa = 0.05? (Este valor nos delimitará la región de aceptación y rechazo)

#Bajo la Ho el estadístico de contraste F se distribuye como una F de grados de libertad (I-1), 
#(n-I) donde I es el número de grupos que disponemos y n el tamaño total de la muestral. 
#Así obtenemos el cuantil buscado:
qf(0.05, 4, 22535, lower.tail = F)

#Valores del estadístico > 2.372336 estarán incluidos en la región de rechazo. 
#En nuetro caso 31.26 es mucho mayor que el valor crítico obtenido.

#¿Qué valor de la tabla ANOVA nos proporciona la varianza muestral 
#común (pooled variance en inglés)? ¿Para qué es útil?

# La raíz cuadrada de la media de los cuadrados del error,
#además de proporcionarnos una estimación de la varianza muestral de todos los datos, 
#se utiliza en la obtención de los intervalos de confianza de las medias en cada uno de 
#los grupos de interés.

#Por ejemplo, este sería el intervalo de confianza de la media de los percepciones identificas para las
#ideologías del gobierno 8, con un nivel de confianza del 95%:

intervals = TukeyHSD(fm)
intervals
plot(intervals)
plot(fm$residuals)
summary(fm$residuals)
boxplot(fm$residuals)
hist(fm$residuals)
qqnorm(fm$residuals) 
qqline(fm$residuals)
#shapiro.test(fm$residuals)
boxplot(fm$residuals~idgov, col = c("yellow", "blue", "white","green","red"),data=datos1)  
desviaciones <- with(datos1,tapply(fm$residuals, idgov, sd))
max(desviaciones) / min(desviaciones)   
bartlett.test(fm$residuals ~ idgov,data=datos1)
#El test de Bartlett indica que no tenemos evidencia suficiente para rechazar 
#la hipótesis nula (las varianzas son iguales)


#¿Qué hipótesis contrasta el test ANOVA?
#  Ho: las medias son iguales en todas las poblaciones

#Ha: hay alguna media distinta

#¿Qué hipótesis contrasta la prueba de Kruskal-Wallis?
#  Ho: la variable respuesta es la misma en todas las poblaciones valoradas

#Ha: la variable respuesta es mayor en alguna de las poblaciones

#Cuando no se cumplen las hipótesis exigidas por el modelo ANOVA, es posible utilizar la prueba no paramétrica Kruskal-Wallis:
#  ¿hay diferencias significativas entre las poblaciones?

with(datos1, kruskal.test(percepcion, idgov))
#Indica cuál es el estadístico de contraste, los grados de libertad, el p-valor correspondiente y cuál sería el 
#valor crítico que definiría las regiones de aceptación y rechazo con un nivel de significación alfa = 0.05.

#Bajo la Ho el estadístico de contraste H del test de Kruskal-Wallis se distribuye 
#como una Chi-cuadrado de grados de libertad (I-1) (donde I es el número de grupos que disponemos). Así obtenemos el cuantil buscado:
qchisq(0.05, 5-1, lower.tail = F)

#Valores del estadístico > 9.487729 estarán incluidos en la región de rechazo. 

#En nuetro caso 191.81 es mucho mayor que el 
#valor crítico obtenido.

#Si transformáramos los datos de la variable respuesta, utilizando logaritmos y 
#después aplicáramos el test de KrusKal-Wallis al logaritmo del número de insectos atrapados, ¿variarían los resultados del test estadístico?

with(datos1, kruskal.test(log(percepcion), idgov)) 
#no varia
#Si hemos detectado diferencias significativas en la variable respuesta para las distintas poblaciones.
#¿Sería posible saber cuáles son los grupos que generan estas diferencias?
with(datos1, kruskal.test(percepcion, idgov))
with(datos1, posthoc.kruskal.nemenyi.test(percepcion, idgov, method="Tukey"))
with(datos1, posthoc.kruskal.nemenyi.test(percepcion, idgov, method = "Chisq"))


numSummary(datos1[, "percepcion", drop = FALSE], statistics = c("mean", "sd", "IQR", 
                                                                "quantiles"), quantiles = c(0, 0.25, 0.5, 0.75, 1))
numSummary(datos1[, "ideologia", drop = FALSE], statistics = c("mean", "sd", "IQR", 
                                                                "quantiles"), quantiles = c(0, 0.25, 0.5, 0.75, 1))

AnovaModel.1 <- lm(percepcion ~ country, data = datos1, contrasts = list(country = "contr.Sum"))
Anova(AnovaModel.1)

with(datos1, (tapply(percepcion, list(country), mean, na.rm = TRUE)))  # means
with(datos1, (tapply(percepcion, list(country), sd, na.rm = TRUE)))  # std. deviations
xtabs(~country, data = datos1)  # counts

AnovaModel.2 <- lm(percepcion ~ idgov, data = datos1, contrasts = list(idgov = "contr.Sum"))
Anova(AnovaModel.2)





