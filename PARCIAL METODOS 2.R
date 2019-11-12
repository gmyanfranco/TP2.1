#############################
## EXAMEN PARCIAL 2.0
#############################

data <- read.delim("clipboard")

table(data$Pediatra)
head(data)

#PREGUNTA 1
#El jefe de personal desea estimar el tiempo medio 
#de duración de la atención en la cita 1 de los pacientes del Dr. Marín con un nivel de confianza del 97%.


marin <- subset(data,Pediatra == 'Marín')

library(nortest)
#Prueba de Cramer-Von Mises
cvm.test(marin$Tiempo1)


t.test(marin$Tiempo1,conf.level = 0.97)$conf


#PREGUNTA 2

table(data$Pediatra)

table(data$Pediatra,data$Opinión)

#Moran(1) y Morón(2)

#M?todo de Wald
library(DescTools)
wa<-BinomDiffCI(11,50,13,40,conf.level=0.98,method="wald")
wa[3]-wa[2]

#M?todo de Score
library(PropCIs)
ds<-diffscoreci(11,50,13,40,conf.level=0.98)$conf
ds[2]-ds[1]

#M?todo de Agresti
library(PropCIs)
dac<-wald2ci(11,50,13,40,conf.level=0.98,adjust="AC")$conf
dac[2]-dac[1]

