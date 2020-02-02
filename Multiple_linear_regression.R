
#scatter plan matrix
ds<-Cars
pairs(ds)

#correlation
cor(ds)

#Regression model and summary
mod.car<-lm(MPG~VOL+HP+SP+WT,data=ds)
summary(mod.car)
#r value 0.77 , adj r value 0.76

#Muticollinearity
library(car)
vif(mod.car)

#Diagnotic plots
  #residual,QQ,std.residual,cook's distance
plot(mod.car)

#regressor vs residuals
residualPlots(mod.car)

#added variable plot
avPlots(mod.car)

#QQ plot
qqPlot(mod.car)

#deletion diagnostics
influenceIndexPlot(mod.car)

#slice row

ds<-Cars[-c(77),]

#new data set post the deletion of rows
newmod.car<-lm(MPG~VOL+HP+SP+WT,data=ds)
summary(newmod.car)
#R value 0.81 , adj R value0.80

#plot check
plot(newmod.car)

residualPlots(newmod.car)
avPlots(newmod.car)
qqPlot(newmod.car)

#diagnostic plots
influenceIndexPlot(newmod.car)
vif(newmod.car)
