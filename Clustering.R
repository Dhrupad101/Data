mydata1<-read.csv("C:/Users/Acer/Desktop/Dp's/Rstudio/Universities.csv")

#Scale 
mydata<-scale(mydata1[,2,7])

#Calculate distance
d<-dist(mydata,method = "euclidean")

#cluster using average linkage
fit<-hclust(d, method = "average")

#display dendogram
plot(fit)

#cut/ group where k = no of groups you want
group<-cutree(fit,k=4)

#dendogram with red borders
rect.hclust(fit, k=4, border="red")

cluster<-data.frame('Uni'=mydata1[,1],'Cluster'=group)
cluster1<-data.frame('test'=mydata1[,],'Cluseter'=group)

#cluster using centroid linkage
fit2<-hclust(d, method = "centroid")

plot(fit2)
group2<-cutree(fit2,k=4)
rect.hclust(fit2, k=4, border="red")
cluster<-data.frame(mydata1,'Cluster2'=group2)

