library(caret)
library(class)
library(DMwR)

###KNN assignment "glass data"
data<-read.csv(file.choose())
round(prop.table(table(data$Type))*100,digits = 1)


#normalize
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

data.m<-data.frame(sapply(data[,-10],normalize))
data.n<-cbind(data.m,Type=data[,10])
data.n$Type<-as.factor(data.n$Type)

#partition

index<-createDataPartition(data.n$Type,p=0.75,list =F)
data.train<-data.n[index,]
data.test<-data.n[-index,]

#label
train.label<-data.n[index,10]
test.label<-data.n[-index,10]

#model
acc<-c()
for (i in 2:25) 
{
  print(i)
  pred_knn<-knn(train = data.train,test = data.test,cl=train.label,k=i)
  a<-table(test.label,pred_knn)
  acc<-acc<-c(acc,sum(diag(a))/sum(a))    
}
acc
plot(acc,type = "b")


pred_knn<-knn(train = data.train,test = data.test,cl=train.label,k=10)
install.packages('gmodels')
library(gmodels)
CrossTable(pred_knn,test.label,chisq = F)


### zoo data 

data<-read.csv(file.choose())
dim(data)
str(data)
table(is.na(data))
summary(data)
library(psych)
plot(data[,-1])
multi.hist(data[,-1])
round(prop.table(table(data$type))*100,digits = 1)

data.m<-data.frame(sapply(data[,-c(1,18)],normalize))
data.n<-cbind(data.m,Type=data[,18])
colnames(data.n)
dim(data.n)
#partition

index<-createDataPartition(data.n$Type,p=0.75,list =F)
data.train<-data.n[index,]
data.test<-data.n[-index,]


#label
train.label<-data.n[index,17]
test.label<-data.n[-index,17]

#model
acc<-c()
for (i in 2:25) 
{
  print(i)
  pred_knn<-knn(train = data.train,test = data.test,cl=train.label,k=i)
  a<-table(test.label,pred_knn)
  acc<-acc<-c(acc,sum(diag(a))/sum(a))    
}
acc
plot(acc,type = "b")
