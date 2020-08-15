##Neural_Networks_5o_startups 

library(readxl)
# Load the data
data<-read.csv("C:/Users/Acer/Downloads/Datasets/50_Startups.csv")
#EDA
str(data)
head(data)
summary(data)
plot(data)

#install.packages('mlr')
library("mlr")
#Creating Dummy features for catergorical variable  
d<-createDummyFeatures(data$State,cols = 'd')
new_data<-cbind(data[-4],d)
head(new_data)
colnames(new_data)
#Renaming col
names(new_data)[1]<-"RD"
names(new_data)[2]<-"Admin"
names(new_data)[3]<-"Marketing"
colnames(new_data)
#Creating normalizationfunction to normalize the data set
normalize <- function(x) {  
  return((x - min(x)) / (max(x) - min(x)))
}
library(stats)
#Applying normalizationfunction 
n.data<- as.data.frame(lapply(new_data, normalize))  
head(n.data)
library(caret)
set.seed(7)
#Creating train and test data in 70:30 using the y variable 
datapart<-createDataPartition(n.data$Profit,p=0.75,list=F)
data_train<-n.data[datapart,]
data_test<-n.data[-datapart,]
library(neuralnet)
#creating and testing models
#Single hidden layer
model<-neuralnet(formula = Profit~RD+Admin+California+Florida
                 +New.York+Marketing,data = data_train)
windows()
plot(model)
#error=0.07
model_results <- compute(model, data_test[-4])
predicted_Profit <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_Profit, data_test$Profit)
#Acc=0.70
#Two hidden layer
model_1<-neuralnet(formula = Profit~RD+Admin+California+Florida
                 +New.York+Marketing,data = data_train,
                 hidden = c(5,2))

windows()
plot(model_1)
#error=0.02

model_results_2<- compute(model_1, data_test[-4])
predicted_Profit <- model_results_2$net.result
cor(predicted_Profit, data_test$Profit)
#Acc=0.90

#Two hidden layer and backward propogation
model_2<-neuralnet(formula = Profit~RD+Admin+California+Florida
                   +New.York+Marketing,data = data_train,
                   hidden = c(5,2),
                   algorithm = 'backprop',
                   learningrate = 0.01,
                   linear.output = FALSE
)
window()
plot(model_2)
#error=0.09

model_results_<- compute(model_2, data_test[-4])
predicted_Profit <- model_results_$net.result
cor(predicted_Profit, data_test$Profit)
#Acc=0.91

#three hidden layer and activation function 
model_3<-neuralnet(formula = Profit~RD+Admin+California+Florida
                   +New.York+Marketing,data = data_train,
                   hidden=c(5,3,5,1),act.fct = "logistic",linear.output = FALSE)
windows()
plot(model_3)
#error=1.04
model_results_3<- compute(model_3, data_test[-4])
predicted_Profit <- model_results_3$net.result
cor(predicted_Profit, data_test$Profit)
#Acc=0.94

model_4<-neuralnet(formula = Profit~RD+Admin+California+Florida
                   +New.York+Marketing,data = data_train,
                   hidden=c(5,3,5,1),
                   act.fct = "tanh",
                   linear.output = FALSE)
plot(model_4)
#error-0.01
model_results_4<-compute(model_4,data_test[-4])
perdicted_Profit<-model_results_4$net.result
cor(perdicted_Profit,data_test$Profit)
#Acc 0.39

#model_3 is the best model with accuracy of 0.94