#------------------------------------------------
# Classe d'ecologia amb R
#------------------------------------------------
# Last update: 21/07/2014
#
# Exemple Pràctica 10. 
# Two-way ANOVA. Normalitat, igualtat de variances i diferències significatives
#
# Authors: Bernat Morro (bernatmorro@gmail.com)
# Mediterranean Institute for Advanced Studies (IMEDEA UIB-CSIC)

#------------------------------------------------ 

rm(list=ls())  # Borra la memòria de R. Per evitar interferències amb dades d'altres experiments

############### 1: Carregar les dades


### 1.1: Designar el directori de treball (carpeta on es troba la taula de dades a analitzar)

setwd("E:/Bernat/2013-2014 QUART/Pràctiques integrades d'ecologia/Estadística")

### 1.2: Carregar i anomenar el set de dades amb un nom simple ("Dades")

read.csv("P.10.csv",                           
         sep=";",                                
         dec=",")                             
Dades = read.csv("P.10.csv",sep=";",dec=",")   
Dades         # Per fer-ho més senzill ja s'ha completat l'excel amb una forma "stacked"


############### 2: Inspeccionar l'objecte


head(Dades)               
dim (Dades)               
summary(Dades)            


############### 3: Representació gràfica

   # Per representar aquestes dades cal organitzar-les en grups

AVerdes <- subset(Dades, Estat=="verdes" & Especie=="A")   # Observacions referents a "Dades",
    # que són "verdes" a la columna "Estat" i "A" a la columna "Especie"
AVerdes
ASen <- subset(Dades, Estat=="senescents" & Especie=="A")
BVerdes <- subset(Dades, Estat=="verdes" & Especie=="B")
BSen <- subset(Dades, Estat=="senescents" & Especie=="B")


hist(BSen$Valors,                               
     main="Pèrdua de pes (%)",         
     xlab="%",                                     
     ylab="Observacions")                          


boxplot(AVerdes$Valors, ASen$Valors, BVerdes$Valors, BSen$Valors)     


############## 4: Anàlisi estadístic

# Per fer un ANOVA cal que les dades compleixin dos requisits: 
# Igualtat de variances entre grups i distribució normal (gausiana)
# de cadascun


# 4.1: Es comprova la igualtat de variances:
bartlett.test(Valors~interaction(Especie,Estat), data=Dades)
      # Es tracta d'un experiment amb dues variables qualitatives amb interacció, pel que 
      # s'ha d'utilitzar aquesta forma del test

  # Ha sortit bé


# 4.2: Test de normalitat de cada grup:
shapiro.test(AVerdes$Valors)
shapiro.test(ASen$Valors)
shapiro.test(BVerdes$Valors)
shapiro.test(BSen$Valors)

   # Tampoc hi ha hagut problemes, pel que es pot fer un ANOVA, en aquest cas de 2 vies



# 4.3: 2-way ANOVA:
anova(lm(Valors~Especie*Estat, data= Dades))

   # S'han trobat diferències significatives en funció de l'"Estat" i de l'"Especie" i,
   # a més, aquestes dues variables interaccionen en si
   # ja que P-value Estat:Especie < 0.05