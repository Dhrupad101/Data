library(readxl)
Titanic<-read.csv("C:/Users/Acer/Desktop/Dp's/Rstudio/Titanic.csv")
install.packages("arules")
library(arules)
Titanic<-Titanic[,-c(1)]
rules<-apriori(Titanic)
arules::inspect(rules)
rules.sorted<-sort(rules,by="lift")
arules::inspect(rules.sorted)


##Rules with rhs containing Survived only
rules<-apriori(Titanic,parameter = list(minlen=1,supp=0.2,conf=0.5) #minlen may help
               ,appearance=list(rhs=c("Survived=No","Survived=Yes")
                                ),control=list(verbose=F))
arules::inspect(rules)

  