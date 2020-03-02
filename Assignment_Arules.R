library(readxl)
library(arules)

Data_movie<-read.csv("C:/Users/Acer/Downloads/my_movies.csv")
Data_groceries<-read.csv("C:/Users/Acer/Downloads/groceries.csv")
Data_books<-read.csv("C:/Users/Acer/Downloads/book.csv")

View(Data_movie)
movie<-Data_movie[,-c(1:5)]
      rules_movie<-apriori(as.matrix(movie),parameter = list(minlen=1,supp=0.4,conf=0.5),appearance = list(rhs=c("LOTR1=1")),control =list(verbose=F))
arules::inspect(rules_movie)


rules<-apriori(Data_groceries)
sorted.rules<-sort(rules,by = "lift")
inspect(sorted.rules)
