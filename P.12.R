#------------------------------------------------
# Classe d'ecologia amb R
#------------------------------------------------
# Last update: 21/07/2014
#
# Exemple Pr?ctica 12. 
# Two-way ANOVA. Normalitat, igualtat de variances i difer?ncies significatives
#
# Authors: Bernat Morro (bernatmorro@gmail.com)
# Mediterranean Institute for Advanced Studies (IMEDEA UIB-CSIC)

#------------------------------------------------ 

rm(list=ls())  # Borra la mem?ria de R. Per evitar interfer?ncies amb dades d'altres experiments

############### 1: Carregar les dades


### 1.1: Designar el directori de treball (carpeta on es troba la taula de dades a analitzar)

setwd("E:/Bernat/2013-2014 QUART/Pr?ctiques integrades d'ecologia/Estad?stica")

### 1.2: Carregar i anomenar el set de dades amb un nom simple ("Dades")

read.csv("P.12.csv",                           
         sep=";",                                
         dec=",")                             
Dades = read.csv("P.12.csv",sep=";",dec=",")   
Dades         # Per fer-ho m?s senzill ja s'ha completat l'excel amb una forma "stacked"


############### 2: Inspeccionar l'objecte


head(Dades)               
dim (Dades)               
summary(Dades)            


############### 3: Representaci? gr?fica

# Per representar aquestes dades cal organitzar-les en grups

sisi <- subset(Dades, Malla=="s?" & Cobertura=="s?")   # Observacions referents a "Dades",
# amb "s?"a la columna "Malla" i "s?" a la columna "Cobertura"
sisi
nosi <- subset(Dades, Malla=="no" & Cobertura=="s?")
sino <- subset(Dades, Malla=="s?" & Cobertura=="no")
nono <- subset(Dades, Malla=="no" & Cobertura=="no")




hist(sino$Valors,                               
     main="Depredaci? (%)",         
     xlab="%",                                     
     ylab="Observacions")                          


boxplot(sisi$Valors, nosi$Valors, sino$Valors, nono$Valors)     


############## 4: An?lisi estad?stic

# Per fer un ANOVA cal que les dades compleixin dos requisits: 
# Igualtat de variances entre grups i distribuci? normal (gausiana)
# de cadascun


# 4.1: Es comprova la igualtat de variances:
bartlett.test(Valors~interaction(Malla,Cobertura), data=Dades)
# Es tracta d'un experiment amb dues variables qualitatives amb interacci?, pel que 
# s'ha d'utilitzar aquesta forma del test

# Ha sortit b?


# 4.2: Test de normalitat de cada grup:
shapiro.test(sisi$Valors)
shapiro.test(nosi$Valors)
shapiro.test(sino$Valors)
shapiro.test(nono$Valors)

# Tampoc hi ha hagut problemes, pel que es pot fer un ANOVA, en aquest cas de 2 vies



# 4.3: 2-way ANOVA:
anova(lm(Valors~Malla*Cobertura, data= Dades))

# Es pot acceptar que l'?s de malla i la cobertura indueixen difer?ncies significatives.
# A m?s, aquestes dues variables interaccionen en si, ja que P-value Malla:Cobertura < 0.05


  #Fixau-vos que en cas d'haver ignorat la interacci? entre Malla i Cobertura (FORMA ERR?NIA)
anova(lm(Valors~Cobertura, data= Dades))
anova(lm(Valors~Malla, data= Dades))
  # no s'haurien trobat difer?ncies significatives en cap dels dos casos, quan sabem que n'hi ha d'haver

