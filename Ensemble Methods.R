data("iris")
View(iris)
library(C50)
library(caret)


###baggings
acc<-c()
for (i in 1:1000)
{
  print(i)
  #data partition
  inTraining<-createDataPartition(iris$Species,p=0.70,list = F)
  training <-iris[inTraining,]
  testing <-iris[-inTraining,]
  #model building
  fittree<-C5.0(training$Species~.,data = training)
  summary(fittree)
  pred<-predict.C5.0(fittree,testing[,-5])
  a<-table(testing$Species,pred)
  acc<-c(acc,sum(diag(a))/sum(a))
}
summary(acc)
boxplot(acc)

###bagging and boosting
acc<-c()
for (i in 1:1000) 
{
  print(i)
  inTraining<-createDataPartition(iris$Species,p=0.70,list = F)
  training <-iris[inTraining,]
  testing <-iris[-inTraining,]
  fittree<-C5.0(training$Species~.,data = training,trials=10)###Trails is boosting
  summary(fittree)
  pred<-predict.C5.0(fittree,testing[,-5])
  a<-table(testing$Species,pred)
  acc<-c(acc,sum(diag(a))/sum(a))
}
summary(acc)
boxplot(acc)
