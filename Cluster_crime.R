library(readxl)
crimedata<-read.csv("C:/Users/Acer/Downloads/crime_data.csv")

plot(crimedata)
cd1<-scale(crimedata[,2:5])
cd2<-dist(cd1,method = "euclidean")
#wss
wss<-c()
for (i in 2:15)wss[i]<-sum(kmeans(cd2,centers = i)$withinss)
plot(1:15,wss,type = "b",xlab = "no of clusters", ylab = "Avg distance")    
# elbow =4
km<-kmeans(cd2,4)
km$centers
group<-km$cluster
fin<-data.frame(crimedata,"Clusters"=group)
View(fin)

