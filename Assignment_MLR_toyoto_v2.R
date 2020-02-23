library(readxl)
library(car)
library(MASS)
Toyota<-read_excel("C:/Users/Acer/Desktop/Dp's/Rstudio/Toyoto.xlsx")
ds1<-Toyota[,-c((1:2),(5:6),8,(10:12),15,(19:38))]
pairs(ds1)
cor(ds1)
mod1<-lm(Price~.,data = ds1)
summary(mod1) #R2 value = 0.863 & Adj.R2 value = 0.863
vif(mod1) #no values are >20
stepAIC(mod1) # door and cc are not significant variables
mod2<-lm(Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + Weight, data = ds1)
summary(mod2) #R2 value = 0.863 & Adj.R2 value = 0.863
plot(mod2)# data point 222 is a outlier
residualPlots(mod2)
qqPlot(mod2)
avPlots(mod2)
influenceIndexPlot(mod2)# data point 222 can be identified in all the Disgnostic Plots
ds1<-ds1[-c(222),]
mod3<-lm(Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + Weight, data = ds1)
summary(mod3)#R2 value = 0.868 & Adj.R2 value = 0.868
plot(mod3)#data point - 960
residualPlots(mod3)
qqPlot(mod3) # points 601 , 960
avPlots(mod3)# points 601 , 960
influenceIndexPlot(mod3)# point 960
ds1<-ds1[-c(960),]
mod4<-lm(Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + Weight, data = ds1)
summary(mod4)#R2 value = 0.872 & Adj.R2 value = 0.872
plot(mod4) # data point 601
residualPlot(mod4)
qqPlot(mod4)# data point 601
avPlots(mod4)# data point 601
influenceIndexPlot(mod4)
ds1<-ds1[-c(601),]
mod5<-lm(Price~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + Weight, data = ds1)
summary(mod5)#R2 value = 0.877 & Adj.R2 value = 0.876  - satisfactory
Age<-readline(prompt = "Please enter age of the car: ")
Age<-as.integer(Age)
KM<-readline(prompt = "Please enter KM of the car: ")
KM<-as.integer(KM)
HP<-readline(prompt = "Please enter HP of the car: ")
HP<-as.integer(HP)
Gear<-readline(prompt = "Please enter number of Gears in the car: ")
Gear<-as.integer(Gear)
QT<-readline(prompt = "Please enter amount Quarterly tax: ")
QT<-as.integer(QT)
Weight<-readline(prompt = "Please enter Weight of the car: ")
Weight<-as.integer(Weight)
Predicted_Price<-predict(mod5,newdata=data.frame(Age_08_04=Age,KM=KM,HP=HP,Gears=Gear,Quarterly_Tax=QT,Weight=Weight))
#Predicted_Price<-predict(mod5,newdata=data.frame(Age_08_04=6,KM=9750,HP=110,Gears=5,Quarterly_Tax=100,Weight=1110))

cat("Predicted selling price for your car is : $",Predicted_Price) 

#Predicted_Price is $18827.54 for a car with below categories
#aged=6,km=9750,HP=110,gears=5,Q.taxes=100& weighing 1110

#Error calculation
Predictions<-predict(mod5)
Errors<-data.frame(ds1,'Predictions'=Predictions,'Errors'=ds1$Price-Predictions)
