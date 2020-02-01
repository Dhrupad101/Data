
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
predict(reg.model,data.frame(Waist=c(50,110)))



