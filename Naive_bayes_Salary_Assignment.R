#Naive_Bayes_Salary
library(readxl)
library(e1071)
library(gmodels)

data_train<-read.csv('C:/Users/Acer/Downloads/Datasets/SalaryData_Train.csv')
data_test<-read.csv('C:/Users/Acer/Downloads/Datasets/SalaryData_Test.csv')

dim(data_train)
na_check_train<-table(is.na(data_train))
na_check_train
str(data_train)
summary(data_train)



dim(data_test)
str(data_test)
na_check_test<-table(is.na(data_test))
na_check_test
summary(data_test)

colnames(data_train)
head(data_train)


Classifier<-naiveBayes(data_train,data_train$Salary)
Pred<-predict(Classifier,data_test)

CrossTable(Pred,data_test$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
#Acc-0.978,0.945

Classifier_1<-naiveBayes(data_train,data_train$Salary,laplace = 1)
Pred_Class<-predict(Classifier_1,data_test)
CrossTable(Pred_Class,data_test$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
#Acc-0.985,0.974

Classifier_2<-naiveBayes(data_train,data_train$Salary,laplace = 2)
Pred_Class1<-predict(Classifier_2,data_test)

CrossTable(Pred_Class1,data_test$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
#Acc-0.982,0.971

Classifier_3<-naiveBayes(data_train,data_train$Salary,laplace = 5)
Pred_Class2<-predict(Classifier_3,data_test)

CrossTable(Pred_Class2,data_test$Salary,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
#Acc-0.979,0.965

#Classifier_1 model has the best accuracy