library(lubridate)
setwd(dir="C:\\Users\\marca\\Desktop\\MH2302D")


donnees <- read.csv("BonnesDonnes.csv")

#colnames(donnees) <- c('id','date','prod','temp','prix')

donnees$date<-as.Date(donnees$date)

donnees$annee <- as.factor(year(donnees$date))

ggplot(donnees, aes(x = temp, y = prod))+geom_point()+geom_smooth()

ggplot(donnees, aes(x = temp, y = prix))+geom_point()+geom_smooth()
donnees <- donnees[which(donnees$annee == 2017 |donnees$annee == 2018),]

str(donnees)

ggplot(donnees, aes(x = temp, y = prod))+geom_point()+geom_smooth()

model <- lm(donnees$prod ~ poly(donnees$temp,3))

plot(model)

#Test normalite temperature
shapiro.test(donnees$temp)
#Test normalite Production
shapiro.test(donnees$prod)
#test Normalite prix
shapiro.test(donnees$prix)

#En gros il ne semble rien y avoir de normalement distribue

summary(model)

head(donnees,12)



