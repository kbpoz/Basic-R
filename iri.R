iris <- read.delim("iris.txt",header = TRUE, sep =",", dec = ".")
attach(iris)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)

smp <- floor(0.75 * nrow(iris))


set.seed(123)
sample <- sample(seq_len(nrow(iris)), size = smp)

train <- iris[sample, ]
test <- iris[-sample, ] 



tree <- rpart(class~.,data=train,method = 'class')
print(tree)
rpart.plot(tree, box.col=c("white", "red"))

predykcja<-predict(tree, newdata=test, type="class", na.action = na.pass)
confusionMatrix(predykcja,test$class)
