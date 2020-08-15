###Logistic Regression 

#Bank data set

bank<-read.csv(file.choose(),sep = ";")
sum(is.na(bank))
bank<-na.omit(bank) # Omitting NA values from the Data 

dim(bank)
str(bank)
head(bank)
summary(bank)
colnames(bank)
head(bank$y,100)
#checking output variable
nrow(bank[which(bank$y=="yes"),])
nrow(bank[which(bank$y=="no"),])
barplot(table(bank$y))
round(prop.table(table(bank$y))*100,digits = 1)
#converting yes into 1 and no into 0 
bank$y <- as.numeric(ifelse(bank$y=="yes",1,0))
table(bank$y)

library(caret)
#create data partition 
dpart<-createDataPartition(bank$y,p=.5,list = F)
trainingdata_bank <-bank[dpart,]
table(trainingdata_bank$y)
testingdata_bank <-bank[-dpart,]
table(testingdata_bank$y)

#building a model
logit <-glm(y~.,data = trainingdata_bank,family = 'binomial')
summary(logit)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(logit))

#prediction score
predicted <- predict(logit, testingdata_bank[,-17], type="response")
head(predicted)

# Model Accuracy
accuracy <- confusionMatrix(testingdata_bank$y,predicted)
accuracy_percetage <- (sum(accuracy[[1,1]],accuracy[[2,2]])/nrow(testingdata_bank))*100
accuracy_percetage

library(pROC)
roccurve <- roc(testingdata_bank$y ~ predicted)
windows()
plot(roccurve)

auc <- auc(testingdata_bank$y ~ predicted)
auc


#credit card data set
#Was the application for a credit card accepted?
credit <-read.csv(file.choose())
dim(credit)
sum(is.na(credit))
credit_1<-na.omit(credit) 
dim(credit_1)
str(credit_1)
summary(credit_1)
credit_1[,1]
credit_1 <- credit_1[,-1]
head(credit_1)
dim(credit_1)

#checking output variable
nrow(credit_1[which(credit_1$card=="yes"),])
nrow(credit_1[which(credit_1$card=="no"),])
barplot(table(credit_1$card))
round(prop.table(table(credit_1$card))*100,digits = 1)
str(credit_1)
credit_1$card <- as.numeric(ifelse(credit_1$card=="yes",1,0))
credit_1$owner <- as.numeric(ifelse(credit_1$owner=="yes",1,0))
credit_1$selfemp <- as.numeric(ifelse(credit_1$selfemp=="yes",1,0))
str(credit_1)
boxplot(credit_1)
hist(credit_1$expenditure)
hist(credit_1$months)

#x  <- as.matrix(credit_1)
#rc <- rainbow(nrow(x), start = 0, end = .3)
#cc <- rainbow(ncol(x), start = 0, end = .3)
#hv <- heatmap(x, col = cm.colors(256), scale = "column",
 #             RowSideColors = rc, ColSideColors = cc, margins = c(5,10))

#install.packages('corrplot')              

library('corrplot')

c <-cor(credit_1)
corrplot(c,method = 'circle')
corrplot(c,method = 'color')
corrplot.mixed(c, lower.col = "black", number.cex = .7)


library(caret)
#create data partition 
dpart_cred<-createDataPartition(credit_1$card,p=.75,list = F)
trainingdata_cred <-credit_1[dpart_cred,]
table(trainingdata_cred$card)
testingdata_cred <-credit_1[dpart_cred,]
table(testingdata_cred$card)

# #building a model
# logit_cred <-glm(card~.,data = trainingdata_cred,family = 'binomial')
# summary(logit_cred)
# # To calculate the odds ratio manually we going r going to take exp of coef(model)
# exp(coef(logit_cred))
# 
# #prediction score
# predicted_cred <- predict(logit_cred, testingdata_cred[,-1], type="response")
# head(predicted_cred)

library(smbinning)
library(InformationValue)
library(Information)


final <- create_infotables(data=trainingdata_cred, y="card", bins=10, parallel=F)$Summary
IV <- create_infotables(data=trainingdata_cred, y="card", bins=10, parallel=F)$Summary$IV

n <- data.frame(ifelse(IV < 0.03,"Not Predictive",ifelse(IV < 0.1,"Somewhat Predictive","Highly Predictive")))
names(n) <- "Label"

final_IV <- cbind(final,n)
final_IV


#Build Logit Models and Predict
formula_nn <- paste("card",paste(final_IV[which(final_IV$Label=="Highly Predictive"),1],collapse ="+"),sep="~")

summary(trainingdata_cred$expenditure)
k <- log(trainingdata_cred$expenditure)
trainingdata_cred$expenditure <- ifelse(k == "-Inf",0,k)

summary(trainingdata_cred$active)
l <- log(trainingdata_cred$active)
trainingdata_cred$active <- ifelse(l == "-Inf",0,l)

summary(trainingdata_cred$reports)
m <- log(trainingdata_cred$reports)
trainingdata_cred$reports <- ifelse(m == "-Inf",0,m)

logitMod <- glm(formula_nn, data=trainingdata_cred, family = binomial("logit"), maxit = 100)
summary(logitMod)


predicted <- plogis(predict(logitMod, testingdata_cred[,-1]))  # predicted scores
head(predicted)
# or
predicted <- predict(logitMod, testingdata_cred[,-1], type="response")  # predicted scores
head(predicted)

# Model Accuracy
accuracy <- confusionMatrix(testingdata_cred$card,predicted)
accuracy_percetage <- (sum(accuracy[[1,1]],accuracy[[2,2]])/nrow(testingdata_cred))*100
accuracy_percetage

## ROC Curve

library(pROC)
roccurve <- roc(testingdata_cred$card ~ predicted)
windows()
plot(roccurve)

auc <- auc(testingdata_cred$card ~ predicted)
auc
