#ZADANIE 2
AP <- read.delim("airpollution.txt")
attach(AP)
library(ggplot2)
summary(Mortality)
summary(Education)
summary(NonWhite)
summary(income)
summary(JanTemp)
summary(JulyTemp)
summary(NOx)


a<-AP[,c("Mortality","NOx")]
plot(NOx, Mortality)
model1<-lm(Mortality~NOx, data=a)
abline(model1)

summary(model1)
b1<-coef(model1)[1]
a1<-coef(model1)[2] #wsp nachylenia prostej, blad = 0.1757, otrzymany model zle opisuje dane
a1

plot(log(NOx), Mortality)
model2<-lm(Mortality~log(NOx), data=a)
abline(model2)
summary(model2)
b2<-coef(model2)[1]
a2<-coef(model2)[2] #wsp nachylenia prostej, blad = 6.41, opisuje dane lepiej ale b³¹D jest duzy

rstudent(model2)
reszty<- rstudent(model2)
reszty
library(car)
ares<-a[-c(29, 47, 49), ]


plot(ares$NOx, ares$Mortality)
model3<-lm(Mortality~log(NOx), data=ares)
abline(model3)
summary(model3)
summary(model3)$r.squared
b3<-coef(model3)[1]
a3<-coef(model3)[2]
