#import datasets
airquality <-datasets::airquality
#view
View(airquality)
#output of top 5 rows
head(airquality,5)
#output of bottom 5 rows
tail(airquality,n=3)
#summary
summary(airquality)
# $ displays all the variable in the data set eg.Temp
summary(airquality$Temp)
#to visualize Ozone (use plot)
plot(airquality$Ozone)
# visualize 2 variables
plot(airquality$Ozone,airquality$Wind)
#visualize all - plot(dataset name)
plot(airquality)

#point and line can be denoted by "p" and "l" for both "b"
plot(airquality$Ozone, type = "b")

#to name the plots axis and plot name axis=lab,main=header, col = colour
plot(airquality$Ozone, xlab = "ozone concentration", ylab = "no of instance",type = "b",main = "Ozone in NY",col="purple")

#bar plot - barplot(dataset.....)
barplot(airquality$Ozone, xlab = "ozone concentration", ylab = "no of instance",type = "b",main = "Ozone in NY",col="purple")

#horizontal horiz = true
barplot(airquality$Ozone, xlab = "ozone concentration", ylab = "no of instance",type = "b",main = "Ozone in NY",col="purple",horiz=TRUE)

#Histogram = hist
hist(airquality$Ozone, xlab = "ozone concentration", ylab = "no of instance",type = "b",main = "Ozone in NY",col="purple")
hist(airquality$Solar.R,col = "maroon")
# Single box plot
boxplot(airquality$Ozone)
#multiple box plot
boxplot(airquality[,1:4],main= "Multiple box plots")
