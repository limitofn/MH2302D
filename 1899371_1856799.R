######## Installation packages ##########
## Loading required package: ggplot2 , ggfortify, tseries, forecast, corrplot-- utiliser install.packages("ggplot2") ou l'installateur de Rstudio
library(ggplot2)
library(ggfortify)
library(tseries)
library(forecast)
library(corrplot)
######## Set le path ##########
setwd(dir="C:\\Users\\marca\\Desktop\\MH2302D")

######################### Entr�e des donn�es ################################# 
donnees <- read.csv("1899371_1856799.csv", header = TRUE, sep = ";",dec = ",")
donnees

summary(donnees)


# ggplot(donnees,aes(Temp�rature.moyen.en.Celsius, y = �lectricit�.produites..M�gawatt.heures.))+geom_point()
########################### Variable #######################################
quantiteElectricite <- donnees$�lectricit�.produites..M�gawatt.heures.
quantiteElectricite
quantiteElectriciteTs <- ts(quantiteElectricite, start = c(2008, 1), end=c(2019, 6), frequency = 12)
subsetElectriciteTs <- window(quantiteElectriciteTs, start=c(2017,1), end=c(2018,12))

prixElectricite <- donnees$Prix.de.vente.de.l.�lectricit�..5000Kw.
prixElectricite
prixElectriciteTs<- ts(prixElectricite, start = c(2008, 1), end=c(2019, 6), frequency = 12)
subsetprixElectriciteTs <- window(prixElectriciteTs, start=c(2017,1), end=c(2018,12))

temperature <- donnees$Temp�rature.moyen.en.Celsius
temperature
temperatureTs <- ts(temperature, start = c(2008, 1), end=c(2019, 6), frequency = 12)
subsetTemperatureTs <- window(temperatureTs, start=c(2017,1), end=c(2018,12))

############################ Plots #################################
plot(subsetTemperatureTs,subsetElectriciteTs)
plot(subsetElectriciteTs,subsetprixElectriciteTs)
plot(subsetTemperatureTs,subsetprixElectriciteTs)

########################### Analyse Des Donn�es ################################
############################ Exploration de Times-Series #########################

#Temperature
boxplot(temperatureTs~cycle(temperatureTs))
decomposeTemperature <- decompose(temperatureTs,"additive")
autoplot(decomposeTemperature)

#Production Electricite
boxplot(quantiteElectriciteTs~cycle(quantiteElectriciteTs))
decomposeProdElec <- decompose(quantiteElectriciteTs,"additive")
autoplot(decomposeProdElec,title = "Decomposition Production Electricite")

#Prix Electricite
boxplot(prixElectriciteTs~cycle(prixElectriciteTs))
decomposePrixElec <- decompose(prixElectriciteTs,"additive")
autoplot(decomposePrixElec)

#Dickers Fuller Test : Elles sont toutes stationnaires! On rejette l'hypothese nulle si p > 0.05 l'hypothese alternative est la stationnarite
#Temperature
adf.test(temperatureTs, k= 0)

#Production Electricite :
adf.test(quantiteElectriciteTs, k=0)

#Prix Electricite :
adf.test(prixElectriciteTs, k=0)


#Correlation temperature et prix electricite :
corrTempPrix = ccf(temperatureTs,quantiteElectriciteTs)
corrTempPrix

#Correlation prix vente et Production
corrPrixProd = ccf(prixElectriciteTs,quantiteElectriciteTs)
corrPrixProd

#correlation temperature et production
corrTempProd = ccf(temperatureTs,quantiteElectriciteTs)



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
qqnorm(prixElectricite, main ="Diagramme de probabilites normal de la vente d'electricite")
qqline(prixElectricite)

#Graphique Temperature
qqnorm(temperature, main ="Diagramme de probabilites normal de la temperature")
qqline(temperature)

#### La moyenne, l'ecart-type, la variance et la taille de l'echantillon #####

#Quantite d'electricite
summary(quantiteElectricite)
sd(quantiteElectricite) #ecart-type
sd(quantiteElectricite)/mean(quantiteElectricite) #coefficient variance
var(quantiteElectricite) #variance
length(quantiteElectricite) #taille

#Prix de l'electricite 
summary(prixElectricite)
sd(prixElectricite) #ecart-type
sd(prixElectricite)/mean(prixElectricite) # coefficient variance
var(prixElectricite) #variance
length(prixElectricite)

#Temperature
summary(temperature)
sd(temperature) #ecart-type
sd(temperature)/mean(temperature) #coefficient variance
var(temperature) #variance
length(temperature)

########################### Fin de l'analyse des donn�es #######################

########################### Modele et hypothese ################################



####Prix �lectricit�
##formatage des donnees pour correle avec une droite normale
subsetprixElectriciteTs <- window(prixElectriciteTs, start=c(2013,1), end=c(2019,6))
##graphique des donnees formatees
hist(subsetprixElectriciteTs)
qqnorm(subsetprixElectriciteTs)
qqline(subsetprixElectriciteTs)
##moyenne, variance, �cart-type
summary(subsetprixElectriciteTs)
S<- sd(subsetprixElectriciteTs) #ecart-type
n<- length(subsetprixElectriciteTs)#le nombre d'observations
m<- mean(subsetprixElectriciteTs)#moyenne
sigma <- var(subsetprixElectriciteTs) #variance
##Test Shapiro-Wilk
shapiro.test(subsetprixElectriciteTs)
##D�termination des parametres
#Moyenne theorique
qt(1-0.025,n-1)
Lm=m-qt(1-0.025,n-1)*S/sqrt(n)
Um=m+qt(1-0.025,n-1)*S/sqrt(n)
L
U
mu0= (L + U)/2
#Variance theorique
Lv=(n-1)*S^2/qchisq(1-0.025,n-1)
Uv=(n-1)*S^2/qchisq(0.025,n-1)
L
U
##Hypoth�se H0:??=mu0 contre H1:??>mu0  
t.test(subsetprixElectriciteTs, mu = Lm, alternative = "greater")
####

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





