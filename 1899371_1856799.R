
######## Set le path ##########
setwd(dir="C:/GitHub/MH2302D")

######################### Entr�e des donn�es ################################# 
donnees <- read.csv("1899371_1856799.csv", header = TRUE, sep = ";",dec = ",")
donnees

summary(donnees)

########################### Variable #######################################
quantiteElectricite <- donnees$�lectricit�.produites..M�gawatt.heures.
quantiteElectricite

prixElectricite <- donnees$Prix.de.vente.de.l.�lectricit�..5000Kw.
prixElectricite

temperature <- donnees$Temp�rature.moyen.en.Celsius
temperature
temperatureTs <- ts(temperature, start = c(2008, 1), end=c(2019, 6), frequency = 12)

########################### Analyse Des Donn�es ################################
############################ Exploration de Times-Series #########################
data(AirPassengers)
class(AirPassengers)
start(AirPassengers)
boxplot(temperatureTs~cycle(temperatureTs))

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
plot(quantiteElectricite, type = "h", ylab = "Quantite d'electricite produite (Megawatt/h)", xlab = "Temps �coul� (mois)", 
     main = "Dispersion des valeurs de la production electrique 
     � partir de 2008-01-01")

#plot du prix de la vente d'�lectricit�
plot(prixElectricite, type = "h", ylab = "Prix de vente pour 5000Kw", xlab = "Temps �coul� (mois)", 
     main = "Dispersion des valeurs du prix de vente de l'�lectricit� 
     � partir de 2008-01-01")

#plot de la temp�rature
plot(temperature, type = "h", ylab = "Temp�rature en Celsius", xlab = "Temps �coul� (mois)", 
     main = "Dispersion des valeurs de la temp�rature 
     � partir de 2008-01-01")

############################ Graphique Quantile-Quantile ####################

#Graphique quantite d'electricite
qqnorm(quantiteElectricite, main ="Diagramme de probabilit�s normal de la production �lectrique")
qqline(quantiteElectricite)

#Graphique prix de vente 
qqnorm(prixElectricite, main ="Diagramme de probabilit�s normal de la vente d'�lectricit�")
qqline(prixElectricite)

#Graphique Temp�rature
qqnorm(temperature, main ="Diagramme de probabilit�s normal de la temp�rature")
qqline(temperature)

#### La moyenne, l'�cart-type, la variance et la taille de l'�chantillon #####

#Quantite d'electricite
summary(quantiteElectricite)
length(quantiteElectricite)

#Prix de l'electricite 
summary(prixElectricite)
length(prixElectricite)

#Temperature
summary(temperature)
length(temperature)

########################### Fin de l'analyse des donn�es #######################

########################### Mod�le et hypoth�se ################################
##Voir histogramme des variables pour pos� la loi 




########################### Fin Mod�le et hypoth�se ############################

########################### R�gression lin�aire ################################

## Temp�rature ind�pendante, production �lectrique d�pendante
modeLineaireProduction <- lm(quantiteElectricite~ temperature)
summary(modeLineaireProduction)

#Coefficient 
modeLineaireProduction$coefficients

#R^2
summary(modeLineaireProduction)$r.sq

#Creation du graphique
par(mfrow = c(1,1))
plot(temperature,quantiteElectricite, main = "Production �lectrique mensuelle selon la temp�rature moyenne",
     xlab = "Temp�rature en Celsius", ylab = "Production �lectrique (Mw/h)")
abline(modeLineaireProduction)

#Analyse de la variance 
anova(modeLineaireProduction)

#Analyse des r�sidus
par(mfrow = c(2,2))
plot(modeLineaireProduction)

## production �lectrique ind�pendante, prix d�pendante 
modeLineairePrix <- lm(prixElectricite~ quantiteElectricite)
summary(modeLineairePrix)

#Coefficient 
modeLineairePrix$coefficients

#R^2
summary(modeLineairePrix)$r.sq

#Creation du graphique
par(mfrow = c(1,1))
plot(quantiteElectricite,prixElectricite, main = "Prix de l'�lectricit� selon la quantit� produite",
     xlab = "Production �lectrique (Mw/h)", ylab = "Prix de l'�tricit� pour 5000kw")
abline(modeLineairePrix)

#Analyse de la variance 
anova(modeLineairePrix)

#Analyse des r�sidus
par(mfrow = c(2,2))
plot(modeLineairePrix)

##R�gression lineaire multiple (Prix d�pendant, quantit� et temp�rature ind�pendante)
modelLineaireMult <- lm(prixElectricite~ quantiteElectricite + temperature)
summary(modelLineaireMult)

#Coefficient
modelLineaireMult$coefficients

#R^2
summary(modelLineaireMult)$r.sq

#Creation du graphique
library(scatterplot3d) #� installer � l'aide de install.packages("scatterplot3d")
library(RColorBrewer) #� installer � l'aide de install.packages("RColorBrewer")

# get colors for labeling the points
plotvar <- prixElectricite # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"PuBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color

# scatter plot
plot.angle <- 45
scatterplot3d(quantiteElectricite, temperature, prixElectricite, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=2, 
              col.axis="gray", col.grid="gray", main = "Prix de l'�lectricit� selon la temp�rature et la quantit� produite",
              xlab = "Production �lectrique (Mw/h)", ylab = "Temp�rature en Celsius", zlab = "Prix de l'�lectricit� pour 5000Kw")

#Analyse de la variance
anova(modelLineaireMult)

#Analyse des r�sidus
par(mfrow = c(2,2))
plot(modelLineaireMult)

########################### Fin R�gression lin�aire ############################





