#------------------------------------------------
# Classe d'ecologia amb R
#------------------------------------------------
# Last update: 21/07/2014
#
# Exemple Pràctica 5. 
# No normalitat, igualtat de variances i diferències significatives
#
# Authors: Bernat Morro (bernatmorro@gmail.com)
# Mediterranean Institute for Advanced Studies (IMEDEA UIB-CSIC)

#------------------------------------------------ 

rm(list=ls())  # Borra la memòria de R. Per evitar interferències amb dades d'altres experiments

############### 1: Carregar les dades


### 1.1: Designar el directori de treball (carpeta on es troba la taula de dades a analitzar)

setwd("E:/Bernat/2013-2014 QUART/Pràctiques integrades d'ecologia/Estadística")

### 1.2: Carregar i anomenar el set de dades amb un nom simple ("Dades")

read.csv("P.5.csv",                           
         sep=";",                                
         dec=",")                             
Dades = read.csv("P.5.csv",sep=";",dec=",")   
Dades


############### 2: Inspeccionar l'objecte


head(Dades)               
dim (Dades)               
summary(Dades)            


############### 3: Representació gràfica


hist(Dades$Sòl.forestal,                               
     main="% de C org. al sòl forestal",         
     xlab="%",                                     
     ylab="Observacions")                       # No sembla que segueixi una distribució normal       

boxplot(Dades$Sòl.forestal, main="Diagrama de caixes") 

boxplot(Dades$Sòl.forestal, Dades$Sòl.agrícola)        


############## 4: Anàlisi estadístic

# Per fer un ANOVA cal que les dades compleixin dos requisits: 
# Igualtat de variances entre grups i distribució normal (gausiana)
# de cadascun


# Per fer l'ANOVA s'ha de canviar la forma de "Dades" per tal
# que a una columna hi hagi totes les observacions i a l'altra
# la variable a la que pertany cada observació. Es fa mitjançant
# la funció "stack()"

stacked <-stack(Dades)
stacked


# 4.1: Es comprova la igualtat de variances:
bartlett.test(values~ind,data=stacked)

# 4.2: Test de normalitat:
shapiro.test(Dades$Sòl.forestal)
shapiro.test(Dades$Sòl.agrícola)
# No segueixen una distribució normal, pel que no es pot utilitzar un ANOVA. L'alternativa més comú és el
# test de Kruskal-Wallis, que no dóna importància a la distribució de les mostres, sempre i quan les dades 
# d'ambdós grups segueixin la mateixa. També és més tolerant amb la igualtat de variances, tot i que sí que 
# han de ser com a mínim comparables. 
# En cas de que les variaces us surtin diferents pot ser convenient revisar les dades i cercar outlayers
# (dades molt allunyades de la resta que probablement siguin errors de mostreig)


# 4.3: Test de Kruskal-Wallis:
kruskal.test(values~ind, data= stacked)
# El p-value de 0.0006971 (p < 0.05) indica que no s'accepta la Ho 
# (les distribucions són iguals), per tant, existeixen diferències
# significatives pel percentatge de carboni orgànic en funció del tipus de sòl
