install.package("animation")
library(plyr)
x<- runif(50) #generating random numbers from uniform dist(0,1)
y<- runif(50)

data<-cbind(x,y)
plot(data)
km<-kmeans(data,4) #k value is 4  k~sqrt(n/2)
km$centers # shows the characteristics
km$cluster# shows membership
#####
library(animantion)
window()
km<-kmeans.ani(data,9)
##### Find Optimum K value using below  Range can be determined by using the sqrt(n/2)
wss<-c()
for (i in 2:15)wss[i] <-sum(kmeans(data,centers = i)$withinss)
plot(1:15,wss,type = "b",xlab = "no of clusters", ylab = "Avg distance")
