library(readxl)
library(caret)
library(C50)

Clothing<-read.csv('C:/Users/dhrup/OneDrive/Documents/Datasets/Company_Data.csv')
#eda
dim(Clothing)
summary(Clothing)
str(Clothing)
hist(Clothing$Sales)
boxplot(Clothing$Sales,horizontal = T)
barplot(Clothing$Sales)
table(is.na(Clothing))

Sales1<-cut(Clothing$Sales,breaks = 2,labels = F)
Sales1<-as.factor(Sales1)
Clothing1<-data.frame(Sales1,Clothing[,-1])
colnames(Clothing1)

acc<-c()
for (i in 1:1000) 
  {
  print(i)  
  part<-createDataPartition(Clothing1$Sales1,p=0.80,list = F)
  training<-Clothing1[part,]
  testing<-Clothing1[-part,]
  tree1<-C5.0(training$Sales1~.,data = training,trails=10,control=C5.0Control(noGlobalPruning = TRUE,winnow=TRUE,minCases = 2,CF=0.75))

  pred<-predict.C5.0(tree1,testing[,-1])
  a<-table(testing$Sales1,pred)
  acc<-c(acc,sum(diag(a))/sum(a))
}

summary(acc)
plot(tree1)
summary(tree1)
#40%  Price
#38%  ShelveLoc
#17%  Age
#5%  Advertising
#2%  Income
#2%  US
#<1%  CompPrice

boxplot(acc)
