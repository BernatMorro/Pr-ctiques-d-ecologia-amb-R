#------------------------------------------------
# Classe d'ecologia amb R
#------------------------------------------------
# Last update: 21/07/2014
#
# Exemple Pr?ctica 5. 
# No normalitat, igualtat de variances i difer?ncies significatives
#
# Authors: Bernat Morro (bernatmorro@gmail.com)
# Mediterranean Institute for Advanced Studies (IMEDEA UIB-CSIC)

#------------------------------------------------ 

rm(list=ls())  # Borra la mem?ria de R. Per evitar interfer?ncies amb dades d'altres experiments

############### 1: Carregar les dades


### 1.1: Designar el directori de treball (carpeta on es troba la taula de dades a analitzar)

setwd("E:/Bernat/2013-2014 QUART/Pr?ctiques integrades d'ecologia/Estad?stica")

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


############### 3: Representaci? gr?fica


hist(Dades$S?l.forestal,                               
     main="% de C org. al s?l forestal",         
     xlab="%",                                     
     ylab="Observacions")                       # No sembla que segueixi una distribuci? normal       

boxplot(Dades$S?l.forestal, main="Diagrama de caixes") 

boxplot(Dades$S?l.forestal, Dades$S?l.agr?cola)        


############## 4: An?lisi estad?stic

# Per fer un ANOVA cal que les dades compleixin dos requisits: 
# Igualtat de variances entre grups i distribuci? normal (gausiana)
# de cadascun


# Per fer l'ANOVA s'ha de canviar la forma de "Dades" per tal
# que a una columna hi hagi totes les observacions i a l'altra
# la variable a la que pertany cada observaci?. Es fa mitjan?ant
# la funci? "stack()"

stacked <-stack(Dades)
stacked


# 4.1: Es comprova la igualtat de variances:
bartlett.test(values~ind,data=stacked)

# 4.2: Test de normalitat:
shapiro.test(Dades$S?l.forestal)
shapiro.test(Dades$S?l.agr?cola)
# No segueixen una distribuci? normal, pel que no es pot utilitzar un ANOVA. L'alternativa m?s com? ?s el
# test de Kruskal-Wallis, que no d?na import?ncia a la distribuci? de les mostres, sempre i quan les dades 
# d'ambd?s grups segueixin la mateixa. Tamb? ?s m?s tolerant amb la igualtat de variances, tot i que s? que 
# han de ser com a m?nim comparables. 
# En cas de que les variaces us surtin diferents pot ser convenient revisar les dades i cercar outlayers
# (dades molt allunyades de la resta que probablement siguin errors de mostreig)


# 4.3: Test de Kruskal-Wallis:
kruskal.test(values~ind, data= stacked)
# El p-value de 0.0006971 (p < 0.05) indica que no s'accepta la Ho 
# (les distribucions s?n iguals), per tant, existeixen difer?ncies
# significatives pel percentatge de carboni org?nic en funci? del tipus de s?l
