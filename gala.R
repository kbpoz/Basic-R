gala1 <- read.delim("gala_data.txt",header = TRUE, sep ="", dec = ".")
attach(gala1)
#a)
model1<-lm(Species~Area+Elevation+Nearest+Scruz, data=gala1)
summary(model1)
res1<-residuals(model1)
plot(res1)
var(res1)
fitted(model1)
model2<-lm(Species~Area+Nearest+Scruz, data=gala1)
res2<-residuals(model2)
var(res2)
fitted(model2)
#Po usuniêciu jednej wartoœci prognozowanej okaza³o siê ¿e wariancja zale¿y od wartoœci prognozowanych

#b)
model3<-lm(sqrt(Species)~Area+Elevation+Nearest+Scruz, data=gala1)
summary(model3)
res3<-residuals(model3)
plot(res3)
var(res3)
#Najwiêksza wartoœæ P value dla Area wiec usuwam Area
model4<-lm(sqrt(Species)~Elevation+Nearest+Scruz, data=gala1)
summary(model4)
res4<-residuals(model4)
plot(res4)
var(res4)

summary(model1)$r.squared
summary(model3)$r.squared
summary(model4)$r.squared

summary(model1)$adj.r.squared
summary(model3)$adj.r.squared
summary(model4)$adj.r.squared
