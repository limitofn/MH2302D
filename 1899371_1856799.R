######## Installation packages ##########
## Loading required package: ggplot2 , ggfortify, tseries, forecast, corrplot-- utiliser install.packages("ggplot2") ou l'installateur de Rstudio
library(ggplot2)
library(ggfortify)
library(tseries)
library(forecast)
library(corrplot)
######## Set le path ##########
setwd(dir="C:\Users\Guillaume Proulx\Desktop\Git hub\MH2302D")

######################### Entree des donnees ################################# 
donnees <- read.csv("1899371_1856799.csv", header = TRUE, sep = ";",dec = ",")
donnees

summary(donnees)


# ggplot(donnees,aes(Temperature.moyen.en.Celsius, y = ï¿½lectricitï¿½.produites..Mï¿½gawatt.heures.))+geom_point()
########################### Variable #######################################
quantiteElectricite <- donnees$Électricité.produites..Mégawatt.heures.
quantiteElectricite
quantiteElectriciteTs <- ts(quantiteElectricite, start = c(2008, 1), end=c(2019, 6), frequency = 12)
subsetElectriciteTs <- window(quantiteElectriciteTs, start=c(2017,1), end=c(2018,12))

prixElectricite <- donnees$Prix.de.vente.de.l.électricité..5000Kw.
prixElectricite
prixElectriciteTs<- ts(prixElectricite, start = c(2008, 1), end=c(2019, 6), frequency = 12)
subsetprixElectriciteTs <- window(prixElectriciteTs, start=c(2017,1), end=c(2018,12))

temperature <- donnees$Température.moyen.en.Celsius
temperature
temperatureTs <- ts(temperature, start = c(2008, 1), end=c(2019, 6), frequency = 12)
subsetTemperatureTs <- window(temperatureTs, start=c(2017,1), end=c(2018,12))

############################ Plots #################################
plot(subsetTemperatureTs,subsetElectriciteTs)
plot(subsetElectriciteTs,subsetprixElectriciteTs)
plot(subsetTemperatureTs,subsetprixElectriciteTs)

########################### Analyse Des Donnees ################################
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
plot(quantiteElectricite, type = "h", ylab = "Quantite d'electricite produite (Megawatt/h)", xlab = "Temps ï¿½coulï¿½ (mois)", 
     main = "Dispersion des valeurs de la production electrique 
     ï¿½ partir de 2008-01-01")

#plot du prix de la vente d'ï¿½lectricitï¿½
plot(prixElectricite, type = "h", ylab = "Prix de vente pour 5000Kw", xlab = "Temps ï¿½coulï¿½ (mois)", 
     main = "Dispersion des valeurs du prix de vente de l'ï¿½lectricitï¿½ 
     ï¿½ partir de 2008-01-01")

#plot de la tempï¿½rature
plot(temperature, type = "h", ylab = "Tempï¿½rature en Celsius", xlab = "Temps ï¿½coulï¿½ (mois)", 
     main = "Dispersion des valeurs de la tempï¿½rature 
     ï¿½ partir de 2008-01-01")

############################ Graphique Quantile-Quantile ####################

#Graphique quantite d'electricite
qqnorm(quantiteElectricite, main ="Diagramme de probabilitï¿½s normal de la production ï¿½lectrique")
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

########################### Fin de l'analyse des donnï¿½es #######################

########################### Modele et hypothese ################################

####Quantite electrique
n<- length(quantiteElectricite) #le nombre d'observations
x <- sum(quantiteElectricite)
lambda <- n/x # lambda
####

####Température
##formatage des donnees pour correle avec une droite normale
subsetTemperatureTs <- window(temperatureTs, start=c(2009,1), end=c(2009,12))
##graphique des donnees formatees
hist(subsetTemperatureTs)
qqnorm(subsetTemperatureTs)
qqline(subsetTemperatureTs)
##moyenne, variance, ecart-type
summary(subsetTemperatureTs)
S<- sd(subsetTemperatureTs) #ecart-type
n<- length(subsetTemperatureTs)#le nombre d'observations
m<- mean(subsetTemperatureTs)#moyenne
sigma <- var(subsetTemperatureTs) #variance
##Test Shapiro-Wilk
shapiro.test(subsetTemperatureTs)
##Determination des parametres
#Moyenne theorique
qt(1-0.025,n-1)
Lm=m-qt(1-0.025,n-1)*S/sqrt(n)
Um=m+qt(1-0.025,n-1)*S/sqrt(n)
Lm
Um
mu0= (Lm + Um)/2
#Variance theorique
Lv=(n-1)*S^2/qchisq(1-0.025,n-1)
Uv=(n-1)*S^2/qchisq(0.025,n-1)
Lv
Uv
##Hypothese H0:u0=mu0 contre H1:u1>mu0  
t.test(subsetTemperatureTs, mu = Lm, alternative = "greater")
####

####Prix electricite
##formatage des donnees pour correle avec une droite normale
subsetprixElectriciteTs <- window(prixElectriciteTs, start=c(2013,1), end=c(2019,6))
##graphique des donnees formatees
hist(subsetprixElectriciteTs)
qqnorm(subsetprixElectriciteTs)
qqline(subsetprixElectriciteTs)
##moyenne, variance, ï¿½cart-type
summary(subsetprixElectriciteTs)
S<- sd(subsetprixElectriciteTs) #ecart-type
n<- length(subsetprixElectriciteTs)#le nombre d'observations
m<- mean(subsetprixElectriciteTs)#moyenne
sigma <- var(subsetprixElectriciteTs) #variance
##Test Shapiro-Wilk
shapiro.test(subsetprixElectriciteTs)
##Determination des parametres
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
##Hypothese H0:u0=mu0 contre H1:u1>mu0  
t.test(subsetprixElectriciteTs, mu = Lm, alternative = "greater")
####

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
plot(temperature,quantiteElectricite, main = "Production ï¿½lectrique mensuelle selon la tempï¿½rature moyenne",
     xlab = "Tempï¿½rature en Celsius", ylab = "Production ï¿½lectrique (Mw/h)")
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





