### MLR computer data set
library(car)
library(caret)
library(MASS)
computer <- read.csv(file.choose())
dim(computer)
head(computer)
table(is.na(computer))
computer<-computer[-1]
str(computer)
attach(computer)
computer$cd<- as.numeric(ifelse(cd=="yes",1,0))
computer$multi<- as.numeric(ifelse(multi=="yes",1,0))
computer$premium<- as.numeric(ifelse(premium=="yes",1,0))
summary(computer)
plot(computer)
pairs(computer)
cor(computer)
colnames(computer)
computer<- computer[,-c(6:8)]
dim(computer)
pairs(computer)
cor(computer)
boxplot(computer)
boxplot(price)
hist(price)
dotplot(price)
barplot(price)

### Scatter plot matrix along with Correlation Coefficients
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(computer,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")

mod <- lm(price~.,data = computer)
summary(mod)
#0.712 r^2 
mod_rmse <- sqrt(mean(mod$residuals^2))
vif(mod) # Original model
#no collinearity 
stepAIC(mod) 
plot(mod)
qqPlot(mod)
avPlots(mod)
influenceIndexPlot(mod)
influencePlot(mod) # observation number 1441

mod1<- lm(price~.,data = computer[-1441,])
summary(mod1)
#0.7128 r^2 
mod1_rmse <- sqrt(mean(mod1$residuals^2))

plot(mod1)
qqPlot(mod1)
avPlots(mod1)
influenceIndexPlot(mod1)
influencePlot(mod1) # observation number 1701

mod2<- lm(price~.,data = computer[-c(1441,1701),])
summary(mod2)
#0.7135 r^2 
mod2_rmse <- sqrt(mean(mod2$residuals^2))

plot(mod2)
qqPlot(mod2)
avPlots(mod2)
influenceIndexPlot(mod2)
influencePlot(mod2) # observation number 994

mod3<- lm(price~.,data = computer[-c(1441,1701,994),])
summary(mod3)
#0.7135
mod3_rmse <- sqrt(mean(mod3$residuals^2))
qqPlot(mod3)
avPlots(mod3)
influenceIndexPlot(mod3)
influencePlot(mod3) # observation number 1043

mod4<- lm(price~.,data = computer[-c(1441,1701,994,1043),])
summary(mod4)
#0.7134 r^2
mod4_rmse <- sqrt(mean(mod4$residuals^2))

qqPlot(mod4)
avPlots(mod4)
influenceIndexPlot(mod4)
influencePlot(mod4) # observation number 20

mod5<- lm(price~.,data = computer[-c(1441,1701,994,1043,20),])
summary(mod5)
#0.7139 r^2
mod5_rmse <- sqrt(mean(mod5$residuals^2))

qqPlot(mod5)
avPlots(mod5)
influenceIndexPlot(mod5)
influencePlot(mod5) # observation number 1123

mod6<- lm(price~.,data = computer[-c(1441,1701,994,1043,20,1123),])
summary(mod6)
##0.7137 r^2
mod6_rmse <- sqrt(mean(mod6$residuals^2))

hist(residuals(mod5))
pred<-predict(mod5)

RMSE_tab<-data.frame("RMSE" = rbind(mod_rmse,mod1_rmse,mod2_rmse,mod3_rmse,mod4_rmse,mod5_rmse,mod6_rmse))
View(RMSE_tab)

mod7 <- lm(log(price)~.,data = computer[-c(1441,1701,994,1043,20),])
summary(mod7)
#0.715 r^2 
mod7_rmse <- sqrt(mean(mod7$residuals^2))
hist(residuals(mod7))
                
                