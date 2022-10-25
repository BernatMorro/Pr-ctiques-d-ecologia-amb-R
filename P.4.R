#------------------------------------------------
# Classe d'ecologia amb R
#------------------------------------------------
# Last update: 19/07/2014
#
# Exemple Pràctica 4. 
# Normalitat, igualtat de variances i diferències significatives
#
# Authors: Bernat Morro (bernatmorro@gmail.com)
# Mediterranean Institute for Advanced Studies (IMEDEA UIB-CSIC)

#------------------------------------------------ 

############### 1: Carregar les dades


### 1.1: Designar el directori de treball (carpeta on es troba la taula de dades a analitzar)

      # Es pot fer manualment copiant la direcció de la carpeta:
setwd("E:/Bernat/2013-2014 QUART/Pràctiques integrades d'ecologia/Estadística")
      # o mitjançant la barra d'eines: Session -> Set Working Directory -> Choose Directory...

### 1.2: Carregar i anomenar el set de dades amb un nom simple ("Dades")

read.csv("P.4.csv",                           # Carregar el fitxer en format csv  
         sep=";",                             # separant les observacions per ";"   
         dec=",")                             # i prenent "," com separador decimal (comprovau-ho a l'excel)
Dades = read.csv("P.4.csv",sep=";",dec=",")   # S'anomena "Dades" per fer-ho més curt
Dades


############### 2: Inspeccionar l'objecte


head(Dades)               # Aspecte de l'objecte (primeres 6 files). Útil per bases de dades grans
dim (Dades)               # 8 files (observacions) i 2 columnes (variables)
summary(Dades)            # Distribució de cada variable (columna)
Dades$Sòl.agrícola        # Observacions de la variable "Sòl.agrícola"
mean(Dades$Sòl.agrícola)  # Mitjana de les observacions de "Sòl.agrícola"



############### 3: Representació gràfica

plot(Dades$Sòl.forestal)                               # Scatterplot. Un punt per observació

barplot(Dades$Sòl.forestal)                            # Gràfic de barres. Una barra per observació

hist(Dades$Sòl.forestal,                               # Histograma. Una barra per grup d'observacions
     main="Densitat aparent del sòl forestal",         # amb el títol "Densitat aparent del sòl forestal"
     xlab="g/cm3",                                     # l'etiqueta "g/cm3" a l'eix d'abcisses
     ylab="Observacions")                              # i "Observacions al d'ordenades
                                            
boxplot(Dades$Sòl.forestal, main="Diagrama de caixes") # Distribució de les observacions de "Sòl.forestal"

boxplot(Dades$Sòl.forestal, Dades$Sòl.agrícola)        # Quan es comparen les dues distribucions es pot intuir que hi haurà diferències significatives


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
    # El p-value > 0.05 indica que es 
    # pot acceptar la hipòtesi nula (les variances de cada grup
    # són iguals). Cosa bona.

    # 4.2: Test de normalitat:
shapiro.test(Dades$Sòl.forestal)
shapiro.test(Dades$Sòl.agrícola)
    # Els p-values d'ambdós tests és > 0.05, per tant s'accepta
    # la Ho (les dades no segueixen una distribució normal). Va bé.

  # Les dades presenten la mateixa variança i pareix que estan normalment
  # distribuïdes, pel que els resultats de l'ANOVA seran vàlids.

    # 4.3: ANOVA:
anova(lm(values~ind, data= stacked))
    # El p-value de 6.365*10^-11 (p < 0.05) indica que no s'accepta la Ho 
    # (les distribucions són iguals), per tant, existeixen diferències
    # significatives per la densitat aparent en funció del tipus de sòl
