sav <- read.delim("savings.txt",header = TRUE, sep ="", dec = ".")
attach(sav)
summary(Country)
summary(Savings)
summary(Pop15)
summary(Pop75)
summary(dpi)
summary(ddpi)
model1<-lm(Savings~dpi+ddpi+Pop15+Pop75, data=sav)
summary(model1)

reszty<-residuals(model1)
plot(reszty, ylab="Reszty", xlab="Indeks kraju", main="Wykres reszt w modelu1") 
text(reszty, row.names(reszty), cex=0.6, pos=4, col="red")
abline(0, 0)
max(reszty) #kraj nr 46 - 
sav[46,]
min(reszty) #kraj nr 7 - 
sav[7,]

plot(model1)
StudRes<-rstudent(model1)
StudRes
max(rstudent(model1)) #46
min(rstudent(model1)) #7


DFFITS<- dffits(model1)
DFBETA<- dfbeta(model1)
cook<- cooks.distance(model1)
DFFITS
DFBETA
cook
library(olsrr)

ols_plot_dfbetas(model1)
ols_plot_dffits(model1)
ols_plot_cooksd_bar(model1)

sav1<-sav[-c(49), ]
model2<-lm(Savings~dpi+ddpi+Pop15+Pop75, data=sav1)
summary(model2)
summary(model1)
#t-value  jak 0 to nieistotne, im wieksza tym jest szansa ze jest istotne, zwiêkszy³o sie czyli jest wiêksza szansa ¿e zal¿y od DPI
# jest szansa ¿e jest jeszcze wiêksza przez Pr>0
ols_plot_dfbetas(model2)
