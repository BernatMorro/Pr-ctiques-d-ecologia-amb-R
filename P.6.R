#------------------------------------------------
# Classe d'ecologia amb R
#------------------------------------------------
# Last update: 21/07/2014
#
# Exemple Pr?ctica 6. 
# Normalitat, igualtat de variances i difer?ncies no significatives
#
# Authors: Bernat Morro (bernatmorro@gmail.com)
# Mediterranean Institute for Advanced Studies (IMEDEA UIB-CSIC)

#------------------------------------------------ 

rm(list=ls())  # Borra la mem?ria de R. Per evitar interfer?ncies amb dades d'altres experiments

############### 1: Carregar les dades


### 1.1: Designar el directori de treball (carpeta on es troba la taula de dades a analitzar)

setwd("E:/Bernat/2013-2014 QUART/Pr?ctiques integrades d'ecologia/Estad?stica")

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


############### 3: Representaci? gr?fica


hist(Dades$llavorsX40,                               
     main="% de C org. al s?l forestal",         
     xlab="%",                                     
     ylab="Observacions")                          

boxplot(Dades$llavorsX10, main="Diagrama de caixes") 

boxplot(Dades$llavorsX10, Dades$llavorsX40)        


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
shapiro.test(Dades$llavorsX10)
shapiro.test(Dades$llavorsX40)


   # Fins aqu? tot b?: els dos sets de dades presenten igualtat de variances i segueixen una distribucions normals


# 4.3: ANOVA:
anova(lm(values~ind, data= stacked))
# Per desgr?cia, amb un nivell de signific?ncia del 0.05 no es pot acceptar que la densitat de sembra
# produeixi difer?ncies significatives en el creixement d'aquesta planta. Es pot intentar repetir
# l'experiment aumentant el tamany mostral, accentuant el factor diferenciador (la densitat, p.ex.
# 10 plantes/cossiol vs 100 plantes/cossiol) o simplement acceptar que les coses no surten sempre com un vol