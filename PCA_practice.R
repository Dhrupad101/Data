install.packages("gdata")
library(readxl)
library(gdata)
PCA<-read.csv("C:/Users/Acer/Desktop/Dp's/Rstudio/PCA.csv")
pca<-princomp(PCA[,2:7],cor = TRUE,scores = TRUE,covmat = NULL) #excluded the first column
summary(pca)# displays all the components with the cumilative %, this may give you an idea for the number of component required for analysis 
pca$scores # displays the scores for each component
pca$loadings # displays weights which are  + or -influence of the variable to the component weights

plot(pca$scores[,1:2],col="Blue",pch=18,cex=0.3,lwd=3)
text(pca$scores[,1:2],labels = c(1:25),cex = 1)
