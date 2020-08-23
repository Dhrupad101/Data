### clustering 

##crime data

library(readxl)
crimedata<-read.csv(file.choose())
dim(crimedata)
str(crimedata)
summary(crimedata)
plot(crimedata)
plot(crimedata[,2:5])
hist(crimedata$Murder)
hist(crimedata$Assault)
hist(crimedata$UrbanPop)
hist(crimedata$Rape)
boxplot(crimedata[,2:5])
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

### Airlines data

airline<- read_excel(file.choose(),sheet = "data")
View(airline)
dim(airline)
table(is.na(airline))
str(airline)
summary(airline)
plot(airline[,-1])
library(psych)
multi.hist(airline[,-1])

#scaling the data 

cd1<- scale(airline[,-1])
cd2 <- dist(cd1,method = 'euclidean')
#cluster using average linkage
fit<-hclust(cd2, method = "average")

#display dendogram
plot(fit)


#cut/ group where k = no of groups you want
group<-cutree(fit,k=4)

#dendogram with red borders
rect.hclust(fit, k=4, border="red")


rect.hclust(fit, k=10
            , border="red")


group<-cutree(fit,k=10)

# cluster<-data.frame('airline'=mydata1,'Cluster'=group)
cluster <-cbind.data.frame(airline,'cluster'=group)

#cluster using centroid linkage
fit2<-hclust(cd2, method = "centroid")

plot(fit2)
group2<-cutree(fit2,k=4)
rect.hclust(fit2, k=4, border="red")
cluster <-cbind.data.frame(airline,'cluster2'=group2)
View(cluster)
hist(cluster$cluster2)
table(cluster$cluster2)
dim(cd2)

sqrt(nrow(airline)/2)
#44

km<-kmeans(airline[,-1],44) #k value is 44  k~sqrt(n/2)
km$centers # shows the characteristics
km$cluster# shows membership
#####
library(animation)
window()
km<-kmeans.ani(airline,44)
##### Find Optimum K value using below  Range can be determined by using the sqrt(n/2)
wss<-c()
for (i in 2:44)wss[i] <-sum(kmeans(airline,centers = i)$withinss)
plot(1:44,wss,type = "b",xlab = "no of clusters", ylab = "Avg distance")
#elbow at 7


km<-kmeans(airline[,-1],7) 
km$centers # shows the characteristics
km$cluster# shows membership

final<-cbind.data.frame(cluster,"k.means clust"= km$cluster)
View(final)
