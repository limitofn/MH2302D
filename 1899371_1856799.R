
######## Set le path ##########
setwd(dir="C:/GitHub/MH2302D")

######################### Entrée des données ################################# 
donnees <- read.csv("1899371_1856799.csv", header = TRUE, sep = ";",dec = ",")
donnees

summary(donnees)

########################### Variable #######################################
quantiteElectricite <- donnees$Électricité.produites..Mégawatt.heures.
quantiteElectricite
prixElectricite <- donnees$Prix.de.vente.de.l.électricité..5000Kw.
prixElectricite
temperature <- donnees$Température.moyen.en.Celsius
temperature

########################### Analyse Des Données ################################

############################ Histogramme des variables #########################
#Aka distribution des donnees

#histogramme de la quantite d'electricite
hist( quantiteElectricite,col="red",main="Histogramme de la production moyenne d'electricte",
      xlab="Intervalle de megawatt/heure",ylab="Effectif", labels = T)
#Histogramme du prix de l'electricite
hist( prixElectricite,col="green",main="Histogramme du prix de vente moyen de l'electricite",
      xlab="Intervalle de prix",ylab="Effectif", labels = T)
#Histogramme de la temperature
hist( temperature,col="orange",main="Histogramme de la temperature moyenne",
      xlab="Intervalle de temperature",ylab="Effectif", labels = T)

############################ Box-Plot ######################################
# Sert seulement a comparer deux population de taille differntes, inutile ici

#box-plot de la quantite d'electricite 
boxplot(quantiteElectricite, col="red", horizontal=T, notch = T)

#box-plot du prix de l'electricite 
boxplot(prixElectricite, col="green", horizontal=T, notch = T)

#box-plot de la temperature
boxplot(temperature, col="orange", horizontal=T, notch = T)

############################ Plot #########################################

#plot de quantite d'electricite produite
plot(quantiteElectricite, type = "h", ylab = "Quantite d'electricite produite (Megawatt/h)", xlab = "Temps écoulé (mois)", 
     main = "Dispersion des valeurs de la production electrique 
     à partir de 2008-01-01")

#plot du prix de la vente d'électricité
plot(prixElectricite, type = "h", ylab = "Prix de vente pour 5000Kw", xlab = "Temps écoulé (mois)", 
     main = "Dispersion des valeurs du prix de vente de l'électricité 
     à partir de 2008-01-01")

#plot de la température
plot(temperature, type = "h", ylab = "Température en Celsius", xlab = "Temps écoulé (mois)", 
     main = "Dispersion des valeurs de la température 
     à partir de 2008-01-01")

############################ Graphique Quantile-Quantile ####################

#Graphique quantite d'electricite
qqnorm(quantiteElectricite, main ="Diagramme de probabilités normal de la production électrique")
qqline(quantiteElectricite)

#Graphique prix de vente 
qqnorm(prixElectricite, main ="Diagramme de probabilités normal de la vente d'électricité")
qqline(prixElectricite)

#Graphique Température
qqnorm(temperature, main ="Diagramme de probabilités normal de la température")
qqline(temperature)

#### La moyenne, l'écart-type, la variance et la taille de l'échantillon #####

#Quantite d'electricite
summary(quantiteElectricite)
length(quantiteElectricite)

#Prix de l'electricite 
summary(prixElectricite)
length(prixElectricite)

#Temperature
summary(temperature)
length(temperature)

########################### Fin de l'analyse des données #######################

########################### Modèle et hypothèse ################################
##Voir histogramme des variables pour posé la loi 




########################### Fin Modèle et hypothèse ############################

########################### Régression linéaire ################################

## Température indépendante, production électrique dépendante
modeLineaireProduction <- lm(quantiteElectricite~ temperature)
summary(modeLineaireProduction)

#Coefficient 
modeLineaireProduction$coefficients

#R^2
summary(modeLineaireProduction)$r.sq

#Creation du graphique
par(mfrow = c(1,1))
plot(temperature,quantiteElectricite, main = "Production électrique mensuelle selon la température moyenne",
     xlab = "Température en Celsius", ylab = "Production électrique (Mw/h)")
abline(modeLineaireProduction)

#Analyse de la variance 
anova(modeLineaireProduction)

#Analyse des résidus
par(mfrow = c(2,2))
plot(modeLineaireProduction)

## production électrique indépendante, prix dépendante 
modeLineairePrix <- lm(prixElectricite~ quantiteElectricite)
summary(modeLineairePrix)

#Coefficient 
modeLineairePrix$coefficients

#R^2
summary(modeLineairePrix)$r.sq

#Creation du graphique
par(mfrow = c(1,1))
plot(quantiteElectricite,prixElectricite, main = "Prix de l'électricité selon la quantité produite",
     xlab = "Production électrique (Mw/h)", ylab = "Prix de l'étricité pour 5000kw")
abline(modeLineairePrix)

#Analyse de la variance 
anova(modeLineairePrix)

#Analyse des résidus
par(mfrow = c(2,2))
plot(modeLineairePrix)

##Régression lineaire multiple (Prix dépendant, quantité et température indépendante)
modelLineaireMult <- lm(prixElectricite~ quantiteElectricite + temperature)
summary(modelLineaireMult)

#Coefficient
modelLineaireMult$coefficients

#R^2
summary(modelLineaireMult)$r.sq

#Creation du graphique
library(scatterplot3d) #À installer à l'aide de install.packages("scatterplot3d")
library(RColorBrewer) #À installer à l'aide de install.packages("RColorBrewer")

# get colors for labeling the points
plotvar <- prixElectricite # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"PuBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color

# scatter plot
plot.angle <- 45
scatterplot3d(quantiteElectricite, temperature, prixElectricite, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=2, 
              col.axis="gray", col.grid="gray", main = "Prix de l'électricité selon la température et la quantité produite",
              xlab = "Production Électrique (Mw/h)", ylab = "Température en Celsius", zlab = "Prix de l'électricité pour 5000Kw")

#Analyse de la variance
anova(modelLineaireMult)

#Analyse des résidus
par(mfrow = c(2,2))
plot(modelLineaireMult)

########################### Fin Régression linéaire ############################





