  ##recommendation engine

library("recommenderlab")
library(caTools)

book <- read.csv('D:/Datasets/book.csv')
dim(book)
head(book)
str(book)
hist(book$Book.Rating)
colnames(book)
table(is.na(book))

#converting into a data matrix to perform recommendation engine

data_matrix <- as(book[-1],'realRatingMatrix')

#data_matrix_norm =normalize(data_matrix)
#data_matrix_normz =normalize(data_matrix,method="Z-score")


#Popularity based 

book_recomm_model1 <- Recommender(data_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(book_recomm_model1, data_matrix[413:414], n=5)
popular_5 =as(recommended_items1, "list")
head(popular_5)

# Collaborative Filtering

#User Based Collaborative Filtering

#book_recomm_model2 <- Recommender(data_matrix_norm, method="UBCF")
book_recomm_model2 <- Recommender(data_matrix, method="UBCF",param=list(method="Cosine",nn=10))

#Predictions for two users 
recommended_items2 <- predict(book_recomm_model2
                              , data_matrix[114:115], n=5)

recommended_5 =as(recommended_items2, "list")

head(recommended_5)