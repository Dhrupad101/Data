##SLR 

# #1) Calories_consumed-> predict weight gained using calories consumed
# 2) Delivery_time -> Predict delivery time using sorting time 
# 3) Emp_data -> Build a prediction model for Churn_out_rate 
# 4) Salary_hike -> Build a prediction model for Salary_hike
### Do the necessary transformations for input variables for getting better R^2 value for the model prepared.

#Calories data set
Cals<-read.csv(file.choose())
dim(Cals)
attach(Cals)
y<-Calories.Consumed
x<-Weight.gained..grams.
hist(y)
hist(x)

cor(y,x)
plot(y,x)
reg <- lm(y ~ x) # lm(Y ~ X)

summary(reg)
#r^2 vaule 0.88

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
rmse<-sqrt(sum(reg$residuals^2)/nrow(Cals))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = Cals, aes(x = x, y = y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Cals, aes(x=x, y=pred))

# ggplot(Cals,aes(x,y))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

reg_log <- lm(y ~ log(x)) # lm(Y ~ log(X)

summary(reg_log)
#r^2 value 0.79
pred_log<--predict(reg_log)

reg_log$residuals
log_rmse<-sqrt(sum(reg_log$residuals^2)/nrow(Cals))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

ggplot(data = Cals, aes(x = x, y = y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Cals, aes(x=x, y=pred_log))


reg_expo <- lm(log(y) ~ x) # lm(log(Y) ~ X)

summary(reg_expo)
#r^2 value 0.86
pred_expo<--predict(reg_expo)
expo<-exp(pred_expo)
error<-y-expo
error
expo_rsme<-sqrt(sum(err^2)/nrow(Cals))  #RMSE

confint(reg_expo,level=0.95)
predict(reg_expo,interval="confidence")

ggplot(data = Cals, aes(x = x, y = y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Cals, aes(x=x, y=pred_expo))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg_poly <- lm(log(y) ~ x+I(x*x)) 
summary(reg_poly)
#r^2 value 0.85
pred_poly<--predict(reg_poly)

expy <- exp(pred_poly)

err = x - expy

poly_rmse<-sqrt(sum(err^2)/nrow(Cals))  #RMSE

confint(reg_poly,level=0.95)
predict(reg_poly,interval="confidence")

# visualization
ggplot(data = Cals, aes(x = x + I(x^2), y = log(y))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Cals, aes(x=x+I(x^2), y=expy))

reg3degree<-lm(log(y)~x + I(x*x) + I(x*x*x))

summary(reg3degree)
#r^2 is 0.84
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)

err = x - expy3

poly3_rmse<-sqrt(sum(err^2)/nrow(Cals))  #RMSE

# visualization
ggplot(data = Cals, aes(x = x + I(x^2) + I(x^3), y = y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Cals, aes(x=x+I(x^2)+I(x^3), y=expy3))



RMSE_table<-data.frame(rbind(poly3_rmse,poly_rmse,expo_rmse,log_rmse,rmse))

View(RMSE_table)
## simple linear degree has the lowest RMSE and the highest adj.r^2 rate hence is the best model

#Delivery data set

delivery <-read.csv(file.choose())
dim(delivery)
str(delivery)
attach(delivery)
y<-Delivery.Time
x<-Sorting.Time
cor(y,x)
plot(y,x)
hist(y) #normal distribution
hist(x)

reg<-lm(y~x)
summary(reg)
#r^2 is 66

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
rmse<-sqrt(sum(reg$residuals^2)/nrow(delivery))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")


ggplot(data = delivery, aes(x = x, y = y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery, aes(x=x, y=pred))

reg_log <- lm(y ~ log(x)) # lm(Y ~ log(X)

summary(reg_log)
#r^2 value 0.67
pred_log<--predict(reg_log)
reg_log$residuals
log_rmse<-sqrt(sum(reg_log$residuals^2)/nrow(delivery))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

ggplot(data = delivery, aes(x = log(x), y = y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery, aes(x=log(x), y=pred_log))


reg_expo <- lm(log(y) ~ x) # lm(log(Y) ~ X)

summary(reg_expo)
#r^2 value 0.69
pred_expo<--predict(reg_expo)
expo<-exp(pred_expo)
error <- y - expo
error
expo_rmse<-sqrt(sum(error^2)/nrow(delivery))  #RMSE

confint(reg_expo,level=0.95)
predict(reg_expo,interval="confidence")

ggplot(data = delivery, aes(x = x, y = log(y))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery, aes(x=x, y=expo))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg_poly <- lm(log(y) ~ x+I(x*x)) 
summary(reg_poly)
#r^2 value 0.73
pred_poly<--predict(reg_poly)

expy <- exp(pred_poly)

err = x - expy

poly_rmse<-sqrt(sum(err^2)/nrow(delivery))  #RMSE

confint(reg_poly,level=0.95)
predict(reg_poly,interval="confidence")

# visualization
ggplot(data = delivery, aes(x = x + I(x^2), y = log(y))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery, aes(x=x+I(x^2), y=expy))

reg3degree<-lm(log(y)~x + I(x*x) + I(x*x*x))

summary(reg3degree)
#r^2 is 0.74
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)

err = x - expy3

poly3_rmse<-sqrt(sum(err^2)/nrow(delivery))  #RMSE

# visualization
ggplot(data = delivery, aes(x = x + I(x^2) + I(x^3), y = y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = delivery, aes(x=x+I(x^2)+I(x^3), y=expy3))



RMSE_table<-data.frame(rbind(poly3_rmse,poly_rmse,expo_rmse,log_rmse,rmse))

View(RMSE_table)

#Polynomial wit 3 degree has high adj r^2 value but higher RMSE value

#############Emp data
emp<-read.csv(file.choose())
dim(emp)
head(emp)
attach(emp)
y <-Churn_out_rate
x <-Salary_hike
hist(y)
hist(x)
barplot(y)
barplot(x)
plot(emp)
cor(y,x) # negative co-relation

reg<-lm(y~x)
summary(reg)
#r^2 is 81

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
rmse<-sqrt(sum(reg$residuals^2)/nrow(emp))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")


ggplot(data = emp, aes(x = x, y = y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=x, y=pred))

reg_log <- lm(y ~ log(x)) # lm(Y ~ log(X)

summary(reg_log)
#r^2 value 0.82
pred_log<--predict(reg_log)
reg_log$residuals
log_rmse<-sqrt(sum(reg_log$residuals^2)/nrow(emp))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

ggplot(data = emp, aes(x = log(x), y = y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=log(x), y=pred_log))


reg_expo <- lm(log(y) ~ x) # lm(log(Y) ~ X)

summary(reg_expo)
#r^2 value 0.69
pred_expo<--predict(reg_expo)
expo<-exp(pred_expo)
error <- y - expo
error
expo_rmse<-sqrt(sum(error^2)/nrow(emp))  #RMSE

confint(reg_expo,level=0.95)
predict(reg_expo,interval="confidence")

ggplot(data = emp, aes(x = x, y = log(y))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=x, y=expo))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg_poly <- lm(log(y) ~ x+I(x*x)) 
summary(reg_poly)
#r^2 value 0.97
pred_poly<--predict(reg_poly)

expy <- exp(pred_poly)

err = x - expy

poly_rmse<-sqrt(sum(err^2)/nrow(emp))  #RMSE

confint(reg_poly,level=0.95)
predict(reg_poly,interval="confidence")

# visualization
ggplot(data = emp, aes(x = x + I(x^2), y = log(y))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=x+I(x^2), y=expy))

reg3degree<-lm(log(y)~x + I(x^2) + I(x^3))

summary(reg3degree)
#r^2 is 0.98
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)

err = x - expy3

poly3_rmse<-sqrt(sum(err^2)/nrow(emp))  #RMSE

# visualization
ggplot(data = emp, aes(x = x + I(x^2) + I(x^3), y = y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=x+I(x^2)+I(x^3), y=expy3))



RMSE_table<-data.frame(rbind(poly3_rmse,poly_rmse,expo_rmse,log_rmse,rmse))

View(RMSE_table)

#Polynomial wit 3 degree has high adj r^2 value but higher RMSE value



#############Salary hike data
Sal<-read.csv(file.choose())
dim(Sal)
head(Sal)
attach(Sal)
y <-Salary
x <-YearsExperience
hist(y)
hist(x)
barplot(y)
barplot(x)
plot(Sal)
cor(y,x) # positive co-relation

reg<-lm(y~x)
summary(reg)
#r^2 is 95
pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
rmse<-sqrt(sum(reg$residuals^2)/nrow(Sal))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")


ggplot(data = Sal, aes(x = x, y = y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Sal, aes(x=x, y=pred))

reg_log <- lm(y ~ log(x)) # lm(Y ~ log(X)

summary(reg_log)
#r^2 value 0.84
pred_log<--predict(reg_log)
reg_log$residuals
log_rmse<-sqrt(sum(reg_log$residuals^2)/nrow(Sal))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

ggplot(data = Sal, aes(x = log(x), y = y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Sal, aes(x=log(x), y=pred_log))


reg_expo <- lm(log(y) ~ x) # lm(log(Y) ~ X)

summary(reg_expo)
#r^2 value 0.92
pred_expo<--predict(reg_expo)
expo<-exp(pred_expo)
error <- y - expo
error
expo_rmse<-sqrt(sum(error^2)/nrow(Sal))  #RMSE

confint(reg_expo,level=0.95)
predict(reg_expo,interval="confidence")

ggplot(data = Sal, aes(x = x, y = log(y))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Sal, aes(x=x, y=expo))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))
reg_poly <- lm(log(y) ~ x+I(x^2)) 
summary(reg_poly)
#r^2 value 0.94
pred_poly<--predict(reg_poly)

expy <- exp(pred_poly)

err = x - expy

poly_rmse<-sqrt(sum(err^2)/nrow(Sal))  #RMSE

confint(reg_poly,level=0.95)
predict(reg_poly,interval="confidence")

# visualization
ggplot(data = Sal, aes(x = x + I(x^2), y = log(y))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Sal, aes(x=x+I(x^2), y=expy))

reg3degree<-lm(log(y)~x + I(x^2) + I(x^3))

summary(reg3degree)
#r^2 is 0.94
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)

err = x - expy3

poly3_rmse<-sqrt(sum(err^2)/nrow(Sal))  #RMSE

# visualization
ggplot(data = Sal, aes(x = x + I(x^2) + I(x^3), y = y)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Sal, aes(x=x+I(x^2)+I(x^3), y=expy3))



RMSE_table<-data.frame(rbind(poly3_rmse,poly_rmse,expo_rmse,log_rmse,rmse))

View(RMSE_table)

#Polynomial wit 2 degree has high adj r^2 value lowest RMSE value





























