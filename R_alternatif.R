donnees <- read.csv("1899371_1856799.csv", header = TRUE, sep = ";",dec = ",")

colnames(donnees) <- c('id','date','prod','temp','prix')

donnees$date<-as.Date(donnees$date)

donnees$annee <- as.factor(year(donnees$date))

ggplot(donnees, aes(x = temp, y = prod))+geom_point()+geom_smooth()


donnees <- donnees[which(donnees$annee == 2018),]

str(donnees)

ggplot(donnees, aes(x = temp, y = prod))+geom_point()+geom_smooth()

model <- lm(donnees$prod ~ poly(donnees$temp,3))

plot(model)

summary(model)

head(donnees,12)



