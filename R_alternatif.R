library(lubridate)
setwd(dir="C:\\Users\\marca\\Desktop\\MH2302D")


donneesTot <- read.csv("BonnesDonnes.csv")

#colnames(donnees) <- c('id','date','prod','temp','prix')

donneesTot$date<-as.Date(donneesTot$date)

donneesTot$annee <- as.factor(year(donneesTot$date))

#Preparation des donnees sous formes de Time series pour analyse de la cyclicique et de l'auto correlation
tempTs <- ts(donneesTot$temp, start = c(2008, 1), end=c(2018, 2), frequency = 12)
prodTs <- ts(donneesTot$prod, start = c(2008, 1), end=c(2018, 2), frequency = 12)
prixTs <- ts(donneesTot$prix, start = c(2008, 1), end=c(2018, 2), frequency = 12)

#Selection des annees a observer
donnees <- donneesTot[which(donneesTot$annee == 2016 |donneesTot$annee == 2017),]





############################ Histogramme des variables #########################
#Aka distribution des donnees

#histogramme de la quantite d'electricite
hist( donneesTot$prod,col="red",main="Histogramme de la production moyenne d'electricte",
      xlab="Intervalle de megawatt/heure",ylab="Effectif", labels = T)

#Histogramme du prix de l'electricite
hist( donneesTot$prix,col="green",main="Histogramme du prix de vente moyen de l'electricite",
      xlab="Intervalle de prix",ylab="Effectif", labels = T)

#Histogramme de la temperature
hist( donneesTot$temp,col="orange",main="Histogramme de la temperature moyenne",
      xlab="Intervalle de temperature",ylab="Effectif", labels = T)






#Illustration du cycle de la temperature et de la production d'electricite

boxplot(tempTs~cycle(tempTs))

boxplot(prodTs~cycle(prodTs))

boxplot(prixTs~cycle(prixTs))





##### Tests de normalite

#Test normalite temperature
shapiro.test(donnees$temp)
#Test normalite Production
shapiro.test(donnees$prod)
#test Normalite prix
shapiro.test(donnees$prix)


#variance et moyenne
#temp
var(donnees$temp)
mean(donnees$temp)

#prod
var(donnees$prod)
mean(donnees$prod)

#prix
var(donnees$prix)
mean(donnees$prix)

########Generation des relations et regression polynomiale

# Temp et prod

str(donnees)
ggplot(donnees, aes(x = temp, y = prod))+geom_point()+geom_smooth()

model <- lm(donnees$prod ~ poly(donnees$temp,3))

plot(model)
summary(model)

head(donnees,12)

# Prix et temp
ggplot(donnees, aes(x = temp, y = prix))+geom_point()+geom_smooth()
# Ne donne rien

#Polynomiale
modelTempPrix <- lm(donnees$prix ~ poly(donnees$temp,3))
summary(modelTempPrix)

#On voit ici que ca marche pas tant

#Generation d'un modele time series pour temp et prod :






