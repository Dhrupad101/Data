data("iris")
install.packages("C50")
library(C50)
library(caret)
#create partitions 
inTraining<-createDataPartition(iris$Species,p=0.65,list = F)
training <-iris[inTraining,]
testing <-iris[-inTraining,]

#model
model<-C5.0(training$Species~.,data = training) #species is the dependent variable
summary(model)

#predication
head(testing)
pred<-predict.C5.0(model,testing[,-5]) 

a<-table(testing$Species,pred)
sum(diag(a))/sum(a) # accuracyt testing

plot(model)
