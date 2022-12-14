#------------------------------------------------
# Classe d'ecologia amb R
#------------------------------------------------
# Last update: 19/07/2014
#
# Exemple Pr?ctica 4. 
# Normalitat, igualtat de variances i difer?ncies significatives
#
# Authors: Bernat Morro (bernatmorro@gmail.com)
# Mediterranean Institute for Advanced Studies (IMEDEA UIB-CSIC)

#------------------------------------------------ 

############### 1: Carregar les dades


### 1.1: Designar el directori de treball (carpeta on es troba la taula de dades a analitzar)

      # Es pot fer manualment copiant la direcci? de la carpeta:
setwd("E:/Bernat/2013-2014 QUART/Pr?ctiques integrades d'ecologia/Estad?stica")
      # o mitjan?ant la barra d'eines: Session -> Set Working Directory -> Choose Directory...

### 1.2: Carregar i anomenar el set de dades amb un nom simple ("Dades")

read.csv("P.4.csv",                           # Carregar el fitxer en format csv  
         sep=";",                             # separant les observacions per ";"   
         dec=",")                             # i prenent "," com separador decimal (comprovau-ho a l'excel)
Dades = read.csv("P.4.csv",sep=";",dec=",")   # S'anomena "Dades" per fer-ho m?s curt
Dades


############### 2: Inspeccionar l'objecte


head(Dades)               # Aspecte de l'objecte (primeres 6 files). ?til per bases de dades grans
dim (Dades)               # 8 files (observacions) i 2 columnes (variables)
summary(Dades)            # Distribuci? de cada variable (columna)
Dades$S?l.agr?cola        # Observacions de la variable "S?l.agr?cola"
mean(Dades$S?l.agr?cola)  # Mitjana de les observacions de "S?l.agr?cola"



############### 3: Representaci? gr?fica

plot(Dades$S?l.forestal)                               # Scatterplot. Un punt per observaci?

barplot(Dades$S?l.forestal)                            # Gr?fic de barres. Una barra per observaci?

hist(Dades$S?l.forestal,                               # Histograma. Una barra per grup d'observacions
     main="Densitat aparent del s?l forestal",         # amb el t?tol "Densitat aparent del s?l forestal"
     xlab="g/cm3",                                     # l'etiqueta "g/cm3" a l'eix d'abcisses
     ylab="Observacions")                              # i "Observacions al d'ordenades
                                            
boxplot(Dades$S?l.forestal, main="Diagrama de caixes") # Distribuci? de les observacions de "S?l.forestal"

boxplot(Dades$S?l.forestal, Dades$S?l.agr?cola)        # Quan es comparen les dues distribucions es pot intuir que hi haur? difer?ncies significatives


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
    # El p-value > 0.05 indica que es 
    # pot acceptar la hip?tesi nula (les variances de cada grup
    # s?n iguals). Cosa bona.

    # 4.2: Test de normalitat:
shapiro.test(Dades$S?l.forestal)
shapiro.test(Dades$S?l.agr?cola)
    # Els p-values d'ambd?s tests ?s > 0.05, per tant s'accepta
    # la Ho (les dades no segueixen una distribuci? normal). Va b?.

  # Les dades presenten la mateixa varian?a i pareix que estan normalment
  # distribu?des, pel que els resultats de l'ANOVA seran v?lids.

    # 4.3: ANOVA:
anova(lm(values~ind, data= stacked))
    # El p-value de 6.365*10^-11 (p < 0.05) indica que no s'accepta la Ho 
    # (les distribucions s?n iguals), per tant, existeixen difer?ncies
    # significatives per la densitat aparent en funci? del tipus de s?l
