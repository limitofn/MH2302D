######## Installation packages ##########
## Loading required package: ggplot2 , ggfortify, tseries, forecast-- utiliser install.packages("ggplot2") ou l'installateur de Rstudio
library(ggplot2)
library(ggfortify)
library(tseries)
library(forecast)
######## Set le path ##########
setwd(dir="C:/GitHub/MH2302D")

######################### Entrï¿½e des donnï¿½es ################################# 
donnees <- read.csv("1899371_1856799.csv", header = TRUE, sep = ";",dec = ",")
donnees

summary(donnees)

########################### Variable #######################################
quantiteElectricite <- donnees$ï¿½lectricitï¿½.produites..Mï¿½gawatt.heures.
quantiteElectricite
quantiteElectriciteTs <- ts(quantiteElectricite, start = c(2008, 1), end=c(2019, 6), frequency = 12)

prixElectricite <- donnees$Prix.de.vente.de.l.ï¿½lectricitï¿½..5000Kw.
prixElectricite
prixElectriciteTs <- ts(prixElectricite, start = c(2008, 1), end=c(2019, 6), frequency = 12)

temperature <- donnees$Tempï¿½rature.moyen.en.Celsius
temperature
temperatureTs <- ts(temperature, start = c(2008, 1), end=c(2019, 6), frequency = 12)

########################### Analyse Des Donnï¿½es ################################
############################ Exploration de Times-Series #########################

#Temperature
boxplot(temperatureTs~cycle(temperatureTs))
decomposeTemperature <- decompose(temperatureTs,"multiplicative")
autoplot(decomposeTemperature)

#Production Electricite
boxplot(quantiteElectriciteTs~cycle(quantiteElectriciteTs))
decomposeProdElec <- decompose(quantiteElectriciteTs,"multiplicative")
autoplot(decomposeProdElec)

#Prix Electricite
boxplot(prixElectriciteTs~cycle(prixElectriciteTs))
decomposePrixElec <- decompose(prixElectriciteTs,"multiplicative")
autoplot(decomposePrixElec)

#Dickers Fuller Test : Elles sont toutes stationnaires! On rejette l'hypothese nulle si p > 0.05 l'hypothese alternative est la stationnarite
#Temperature
adf.test(temperatureTs)

#Production Electricite :
adf.test(quantiteElectriciteTs)

#Prix Electricite :
adf.test(prixElectriciteTs)

############################ Histogramme des variables #########################
#Aka distribution des donnees

#histogramme de la quantite d'electricite
hist( quantiteElectricite,col="red",main="Histogramme de la production moyenne d'électricté",
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

#plot du prix de la vente d'ï¿½lectricitï¿½
plot(prixElectricite, type = "h", ylab = "Prix de vente pour 5000Kw", xlab = "Temps écoulé (mois)", 
     main = "Dispersion des valeurs du prix de vente de l'électricité 
     à partir de 2008-01-01")

#plot de la tempï¿½rature
plot(temperature, type = "h", ylab = "Température en Celsius", xlab = "Temps écoulé (mois)", 
     main = "Dispersion des valeurs de la température 
     à partir de 2008-01-01")

############################ Graphique Quantile-Quantile ####################

#Graphique quantite d'electricite
qqnorm(quantiteElectricite, main ="Diagramme de probabilitïés normal de la production électrique")
qqline(quantiteElectricite)

#Graphique prix de vente 
qqnorm(prixElectricite, main ="Diagramme de probabilités normal de la vente d'électricité")
qqline(prixElectricite)

#Graphique Tempï¿½rature
qqnorm(temperature, main ="Diagramme de probabilités normal de la température")
qqline(temperature)

#### La moyenne, l'ï¿½cart-type, la variance et la taille de l'ï¿½chantillon #####

#Quantite d'electricite
summary(quantiteElectricite)
sd(quantiteElectricite) #ecart-type
sd(quantiteElectricite)/mean(quantiteElectricite) #variance
length(quantiteElectricite) #taille

#Prix de l'electricite 
summary(prixElectricite)
sd(prixElectricite) #ecart-type
sd(prixElectricite)/mean(prixElectricite) #variance
length(prixElectricite)

#Temperature
summary(temperature)
sd(temperature) #ecart-type
sd(temperature)/mean(temperature) #variance
length(temperature)

########################### Fin de l'analyse des donnï¿½es #######################

########################### Modï¿½le et hypothï¿½se ################################
##Voir histogramme des variables pour posï¿½ la loi 




########################### Fin Modï¿½le et hypothï¿½se ############################

########################### Rï¿½gression linï¿½aire ################################

## Tempï¿½rature indï¿½pendante, production ï¿½lectrique dï¿½pendante
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

#Analyse des rï¿½sidus
par(mfrow = c(2,2))
plot(modeLineaireProduction)

## production ï¿½lectrique indï¿½pendante, prix dï¿½pendante 
modeLineairePrix <- lm(prixElectricite~ quantiteElectricite)
summary(modeLineairePrix)

#Coefficient 
modeLineairePrix$coefficients

#R^2
summary(modeLineairePrix)$r.sq

#Creation du graphique
par(mfrow = c(1,1))
plot(quantiteElectricite,prixElectricite, main = "Prix de l'ï¿½lectricitï¿½ selon la quantitï¿½ produite",
     xlab = "Production ï¿½lectrique (Mw/h)", ylab = "Prix de l'ï¿½tricitï¿½ pour 5000kw")
abline(modeLineairePrix)

#Analyse de la variance 
anova(modeLineairePrix)

#Analyse des rï¿½sidus
par(mfrow = c(2,2))
plot(modeLineairePrix)

##Rï¿½gression lineaire multiple (Prix dï¿½pendant, quantitï¿½ et tempï¿½rature indï¿½pendante)
modelLineaireMult <- lm(prixElectricite~ quantiteElectricite + temperature)
summary(modelLineaireMult)

#Coefficient
modelLineaireMult$coefficients

#R^2
summary(modelLineaireMult)$r.sq

#Creation du graphique
library(scatterplot3d) #ï¿½ installer ï¿½ l'aide de install.packages("scatterplot3d")
library(RColorBrewer) #ï¿½ installer ï¿½ l'aide de install.packages("RColorBrewer")

# get colors for labeling the points
plotvar <- prixElectricite # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"PuBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color

# scatter plot
plot.angle <- 45
scatterplot3d(quantiteElectricite, temperature, prixElectricite, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=2, 
              col.axis="gray", col.grid="gray", main = "Prix de l'ï¿½lectricitï¿½ selon la tempï¿½rature et la quantitï¿½ produite",
              xlab = "Production ï¿½lectrique (Mw/h)", ylab = "Tempï¿½rature en Celsius", zlab = "Prix de l'ï¿½lectricitï¿½ pour 5000Kw")

#Analyse de la variance
anova(modelLineaireMult)

#Analyse des rï¿½sidus
par(mfrow = c(2,2))
plot(modelLineaireMult)

########################### Fin Rï¿½gression linï¿½aire ############################





