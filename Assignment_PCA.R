library(readxl)
library(gdata)

pca<-read.csv("C:/Users/Acer/Downloads/wine.csv")
head(pca)
pca1<-princomp(pca[,c(2:14)],cor=TRUE,scores = TRUE,covmat = NULL)
summary(pca1)
ds<-pca1$scores
head(ds)
pca1$loadings
clusterdata<-ds[,1:3]
colnames(clusterdata)
clusterdata<-dist(clusterdata,method = "euclidean")

wss<-c()
for(i in 1:10)wss[i]<-sum(kmeans(clusterdata,centers = i)$withinss)
plot(1:10,wss,type="b",xlab="no of clusters", ylab="avg distance")

km<-kmeans(clusterdata,3)
group<-km$cluster

fit<-hclust(clusterdata, method = "centroid")
plot(fit)
rect.hclust(fit,k=3,border="red")
group1 <-cutree(fit,k=3)
head(group1)
View(group1)
new_wine<-data.frame(pca,"Types"=group1)
View(new_wine)
