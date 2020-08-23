library(readxl)
library(caret)
library(C50)
library(randomForest)

clothing<-read.csv(file.choose())
dim(clothing)
table(is.na(clothing))
str(clothing)
summary(clothing)
head(clothing)
colnames(clothing)
View(clothing)
plot(clothing[,-c(7,10,11)])
library(psych)
multi.hist(clothing[,-c(7,10,11)])
check<-cut(clothing$Sales,breaks =2,right=FALSE,labels = F)
head(check)
nclothing<-data.frame(clothing,"sales1"=check)
nclothing<-nclothing[,-1]
colnames(nclothing)
nclothing$sales1<-as.factor(nclothing$sales1)
View(nclothing)
accuracy<-c()
for (i in 1:1000) 
{
  print (i)
  part<-createDataPartition(nclothing$sales1,p=0.75,list = F)
  train<-nclothing[part,]
  test<-nclothing[-part,]
  dt<-C5.0(train$sales1~.,data = train)
  pred<-predict.C5.0(dt,test[,-11])
  a<-table(test$sales1,pred)
  accuracy<-c(accuracy,sum(diag(a))/sum(a))
}

summary(accuracy)
boxplot(accuracy)

###bagging and boosting
acc<-c()
for (i in 1:1000) 
{
  print(i)
  inTraining<-createDataPartition(nclothing$sales1,p=0.75,list = F)
  training <-nclothing[inTraining,]
  testing <-nclothing[-inTraining,]
  fittree<-C5.0(train$sales1~.,data = train,trials=10)###Trails is boosting
  summary(fittree)
  pred<-predict.C5.0(fittree,testing[,-11])
  a<-table(testing$sales1,pred)
  acc<-c(acc,sum(diag(a))/sum(a))
}
summary(acc) ### 95% accuracy 
boxplot(acc)

# Training using 'random forest' algorithm
set.seed(111)
inTraining<-createDataPartition(nclothing$sales1,p=0.75,list = F)
training <-nclothing[inTraining,]
testing <-nclothing[-inTraining,]
model <- train(sales1 ~ .,
               data = training,
               method = 'rf',
               trControl = trainControl(method = 'cv',number = 5))
model


model1 <- randomForest(sales1~.,data=training,ntree=1000)
model1


#Check if any missing values in test data before predection
summary(testing)

#Importance of variable - Lower Gini
print(importance(model1))

head(testing)

#Prediction - With Model1
pred <- predict(model, newdata = testing[,-11])
confusionMatrix(testing$sales1,pred)

#Prediction - With Model2
pred1 <- predict(model1, newdata = testing[,-11])
confusionMatrix(testing$sales1,pred1)
