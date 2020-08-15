# install.packages('e1071')
library(kernlab)
library(e1071) 
library(caret)


Train.sal<-read.csv(file.choose())
Test.Sal<-read.csv(file.choose())

dim(Train.sal)
dim(Test.Sal)
str(Train.sal)
str(Test.Sal)
table(is.na(Train.sal))
Train.sal <-na.omit(Train.sal)
table(is.na(Test.Sal))
Test.Sal <-na.omit(Test.Sal)
#plot(Train.sal)

colnames(Train.sal)

cols <- c("age","educationno","capitalgain","capitalloss","hoursperweek","Salary")
Train.sal<-Train.sal[,cols]
colnames(Train.sal)
head(Train.sal)
table(Train.sal$Salary)

Test.Sal<-Test.Sal[,cols]
colnames(Test.Sal)
#plot(Train.sal)

Train.sal[-6] <- scale(Train.sal[-6]) 
Test.Sal[-6] <- scale(Test.Sal[-6]) 


kernals <- c("rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", "besseldot", "anovadot")
#types <- c("C-svc","nu-svc","C-bsvc","spoc-svc","kbb-svc","one-svc","eps-svr","nu-svr","eps-bsvr")
types <- c("C-bsvc","spoc-svc","kbb-svc","one-svc","eps-svr","nu-svr","eps-bsvr")
name <- c()
accuracy <- c()

colnames(Train.sal)

for(i in kernals)
{
  for(j in types)
  {
    name <- rbind(name,data.frame(paste("Kernal = ",i," - Type = ",j)))
    classifier<-ksvm(Salary ~.,data = Train.sal,kernel = i,type=j)
    classifier
    
    y_pred <- predict(classifier, newdata <- Test.Sal[-6]) 
    y_pred
    
    acc <- mean(y_pred==Test.Sal$Salary)*100
    
    accuracy <- rbind(accuracy,data.frame(acc))                  
    
  }
}




names(name) <- c("Name")
names(accuracy) <- c("Accuracy Percentage")
cbind(n,accuracy)





# Sal_class<-ksvm(Salary~.,data=Train.sal,kernel="rbfdot")
# Sal_prediction<-predict(Sal_class, Test.Sal)
# head(Sal_prediction)
# agr<-Sal_prediction==Test.Sal$Salary
# prop.table(table(agr))
# 
# round(prop.table(table(Train.sal$Salary))*100,digits = 1)
# library(DMwR)
# Smoted<-SMOTE(Salary~.,data=Train.sal,perc.under = 100)
# round(prop.table(table(Smoted$Salary))*100,digits = 1)
# #library(caret)
# #cor(Train.sal)



#### Forest fire data set


# library(e1071) 
# library(kernlab)
# library(caret)
ffdata<-read.csv(file.choose())
dim(ffdata)
str(ffdata)
summary(ffdata)
table(is.na(ffdata))
colnames(ffdata)
ffdata<-ffdata[,c(3:10,31)]
head(ffdata)
table(ffdata$size_category)
library(caret)
datapart <-createDataPartition(ffdata$size_category,p=0.75,list=F)
train_ff <-ffdata[datapart,]
table(train_ff$size_category)
test_ff<-ffdata[-datapart,]
table(test_ff$size_category)
colnames(train_ff)

train_ff[-9] <- scale(train_ff[-9]) 
test_ff[-9] <- scale(test_ff[-9]) 
head(train_ff,15)
tail(train_ff,10)

library(e1071)
library(kernlab)

kernals <- c("rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", "besseldot", "anovadot")
types <- c("C-svc","nu-svc","C-bsvc","spoc-svc","kbb-svc","one-svc","eps-svr","nu-svr","eps-bsvr")

name <- c()
accuracy <- c()

for(i in kernals)
{
  for(j in types)
  {
    name <- rbind(name,data.frame(paste("Kernal = ",i," - Type = ",j)))
    classifier<-ksvm(size_category ~.,data = train_ff,kernel = i,type=j)
    classifier
    
    y_pred <- predict(classifier, newdata <- test_ff[-9]) 
    y_pred
    
    acc <- mean(y_pred==test_ff$size_category)*100
    
    accuracy <- rbind(accuracy,data.frame(acc))                  
    
  }
}


names(name) <- c("Name")
names(accuracy) <- c("Accuracy Percentage")
cbind(name,accuracy)

           