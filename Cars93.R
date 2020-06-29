#1a
library(MASS)
data("Cars93")
attach(Cars93)
head(Cars93)
min.cena <- Min.Price*1000*3.35
spalanie.miasto <- 100*3.8 /(MPG.city*1.6)
spalanie.poza <- 100*3.8/(MPG.highway*1.6)
waga <- Weight *0.4536

#1b
summary(min.cena)
q95 <- quantile(Min.Price, 0.95)
a <- Cars93[,c("Make", "Min.Price")]
b <- a[a$Min.Price > q95,]
b

#1c
#Wykres s³upkowy
table(Type)
count <- table(Type)
slup=barplot(count, ylab="Ilosc", xlab= "Type", 
        main="Wykres s³upkowy zmiennej Type")
text(slup,2,count)

#Wykres ko³owy
kolo=pie(count, main="Wykres ko³owy zmiennej Type")

#14 zaliczono do kategorii Sporty


#wykresy skrzynkowe zuzycia paliwa dla US nonUS w mieœcie
spalanie<-Cars93[,c("Origin", "MPG.city")]
spalanie
nonUS<-spalanie[spalanie$Origin=="non-USA",]
US<-spalanie[spalanie$Origin=="USA",]
boxplot(nonUS$MPG.city, ylab ="MPG", main =  "Wykres skrzynkowy zu¿ycia paliwa aut pochodz¹cych spoza USA")
boxplot(US$MPG.city, ylab="MPG", main =  "Wykres skrzynkowy zu¿ycia paliwa aut pochodz¹cych z USA")


#Wykres rozrzutu ceny podstawowej wersji samochodu od jego zu¿ycia benzyny w miescie oraz
#wykres rozrzutu zu¿ycia benzyny w mieœcie w funkcji ¿uzycia benzyny na autostradzie.+ wsp
#korelacji
par(mfrow=c(1,2))
plot(Cars93$MPG.city, Cars93$Min.Price, xlab="MPG.city", ylab="Min.Price", main="Cena podstawowa samochodu od jego 
zuzycia benzyny w mieœcie", col="black")
text(30,44,round(cor(MPG.city, Min.Price), digits = 4))
plot(Cars93$MPG.city, Cars93$MPG.highway, xlab= "MPG.city", ylab="MPG.highway", main="Zu¿ycie benzyny w mieœcie w 
     funkcji zu¿ycia benzyny na autostradzie", col="red")
text(30,47,round(cor(MPG.city, MPG.highway), digits = 4))




library(ggplot2)
ggplot(data=Cars93, aes(x=Weight), ylab="Iloœæ") + geom_histogram()
p<-ggplot(data=Cars93, aes(x=Weight)) + 
  geom_histogram(color="black", fill="white")
p + xlab("Waga") +ylab("Iloœæ") + ggtitle("Histogram czêstoœci dla danych dotycz¹cych wagi samochodu")

#ZADANIE2




