###DT 
##Fraud data

Fraud_check <- read.csv(file.choose())
dim(Fraud_check)
table(is.na(Fraud_check))
str(Fraud_check)
summary(Fraud_check)

#Convert target variable Sales into categorical variable
Fraud_check$Taxable_Income <- ifelse(Fraud_check$Taxable.Income <= 30000, 0,1)
Fraud_check <- Fraud_check[,-3]
head(Fraud_check)

#Picking the best Features
table(Fraud_check[,c("Taxable_Income","Undergrad")]) #Keep
table(Fraud_check[,c("Taxable_Income","Marital.Status")]) #Keep
table(Fraud_check[,c("Taxable_Income","Urban")]) #Keep

#Checking distribution of each continuous variable
install.packages("fields")
library(fields)

bplot.xy(Fraud_check$Taxable_Income, Fraud_check$City.Population) #Keep
summary(Fraud_check$City.Population)

bplot.xy(Fraud_check$Taxable_Income, Fraud_check$Work.Experience) #Keep
summary(Fraud_check$Work.Experience)


# Converting 'High Sales' to a factor
Fraud_check$Taxable_Income <- factor(Fraud_check$Taxable_Income)
table(Fraud_check$Taxable_Income)

# Set a random seed
set.seed(12345)

#Creating Training and Test data sets
inTrainingLocal <- createDataPartition(Fraud_check$Taxable_Income,p=.75,list = F) # 75% data is training dataset
Fraud_check_train <- Fraud_check[inTrainingLocal,]
Fraud_check_test <- Fraud_check[-inTrainingLocal,]

# Building model on training data 
library(C50)
model1 <- C5.0(Fraud_check_train[,-6],Fraud_check_train$Taxable_Income)
windows()
plot(model1) # Tree graph
# Training accuracy
pred_train <- predict(model1,Fraud_check_train)

mean(Fraud_check_train$Taxable_Income==pred_train) #0.793 Accuracy

library(caret)
confusionMatrix(pred_train,Fraud_check_train$Taxable_Income)

predc5.0_test <- predict(model1,Fraud_check_test) # predicting on test data
mean(predc5.0_test==Fraud_check_test$Taxable_Income) #0.793 Accuracy
confusionMatrix(predc5.0_test,Fraud_check_test$Taxable_Income)
library(gmodels)
# Cross tablez
CrossTable(Fraud_check_test$Taxable_Income,predc5.0_test)

