#------------------------------------------------
# Classe d'ecologia amb R
#------------------------------------------------
# Last update: 21/07/2014
#
# Exemple Pràctica 6. 
# Normalitat, igualtat de variances i diferències no significatives
#
# Authors: Bernat Morro (bernatmorro@gmail.com)
# Mediterranean Institute for Advanced Studies (IMEDEA UIB-CSIC)

#------------------------------------------------ 

rm(list=ls())  # Borra la memòria de R. Per evitar interferències amb dades d'altres experiments

############### 1: Carregar les dades


### 1.1: Designar el directori de treball (carpeta on es troba la taula de dades a analitzar)

setwd("E:/Bernat/2013-2014 QUART/Pràctiques integrades d'ecologia/Estadística")

### 1.2: Carregar i anomenar el set de dades amb un nom simple ("Dades")

read.csv("P.6.csv",                           
         sep=";",                                
         dec=",")                             
Dades = read.csv("P.6.csv",sep=";",dec=",")   
Dades


############### 2: Inspeccionar l'objecte


head(Dades)               
dim (Dades)               
summary(Dades)            


############### 3: Representació gràfica


hist(Dades$llavorsX40,                               
     main="% de C org. al sòl forestal",         
     xlab="%",                                     
     ylab="Observacions")                          

boxplot(Dades$llavorsX10, main="Diagrama de caixes") 

boxplot(Dades$llavorsX10, Dades$llavorsX40)        


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
shapiro.test(Dades$llavorsX10)
shapiro.test(Dades$llavorsX40)


   # Fins aquí tot bé: els dos sets de dades presenten igualtat de variances i segueixen una distribucions normals


# 4.3: ANOVA:
anova(lm(values~ind, data= stacked))
# Per desgràcia, amb un nivell de significància del 0.05 no es pot acceptar que la densitat de sembra
# produeixi diferències significatives en el creixement d'aquesta planta. Es pot intentar repetir
# l'experiment aumentant el tamany mostral, accentuant el factor diferenciador (la densitat, p.ex.
# 10 plantes/cossiol vs 100 plantes/cossiol) o simplement acceptar que les coses no surten sempre com un vol