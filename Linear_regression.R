
#Plots
library("lattice")
attach(WC_AT)
dotplot(Waist,main="Dot Plot of AT",col="dodgerblue4")
dotplot(AT,main="Dot Plot of Waist",col="dodgerblue4")
boxplot(Waist,col="dodgerblue4")
boxplot(AT,col="dodgerblue4")

#Regression model and summary
reg.model<-lm(AT~Waist,data = WC_AT)
summary(reg.model)

#predict intervals
pred<-predict(reg.model,newdata = data.frame(Waist=c(76.85,82)))

##Error computation
pred_E<-predict(reg.model)
Error<-data.frame(WC_AT,"Pred"= pred_E,"Error"=WC_AT$AT-pred_E)
#we have created a data frame adding 2 columns,Pred and Error,error is a outcome of actual - predict



