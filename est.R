est <- read.delim("realest.txt",header = TRUE, sep ="", dec = ".")
attach(est)
model1<-lm(Price~Bedroom+Space+Room+Lot+Tax+Bathroom+Garage+Condition, data=est)
summary(model1)
model2<-lm(Price~Bedroom)
summary(model2)
predict(model1, data.frame(x1=Bedroom+1), interval="confidence",level=0.95)
predict(model2, data.frame(x1=Bedroom), interval="confidence",level=0.95)
predict(model2, data.frame(x1=Bedroom+1), interval="confidence",level=0.95)
predict(model1, data.frame(Bedroom=3, Space=1500, Room=8, Lot=40, Bathroom=5, Garage=1, Tax=1000, Condition=0), interval = "confidence", level = 0.95)

