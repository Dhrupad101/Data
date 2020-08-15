##Association rules
library(readxl)
library(caret)
library(arules)
library(arulesViz)

#data set books

books <- read.csv('D:/Datasets/books.csv')
dim(books)
str(books)
summary(books)
head(books)
colnames(books)
table(is.na(books)) # 0 NaNs 

books_t <-read.transactions('D:/Datasets/books.csv',format = 'basket',header = T)
inspect(books_t)

bk_rules<-apriori(books,parameter = list(con=0.40,supp=0.1,minlen=3),control =list(verbose=F))
arules::inspect(bk_rules)
#11132 rules observed - confidence of 40% and support of 10% for 3 items

bk_rules1<-apriori(books,parameter = list(con=0.65,supp=0.4,minlen=2),control =list(verbose=F))
arules::inspect(bk_rules1)
#11242 rules observed - confidence of 65% and support of 40% for 2 items
 
bk_rules2<-apriori(books,parameter = list(con=0.775,supp=0.80,minlen=4),control =list(verbose=F))
arules::inspect(bk_rules2)
#10637 rules observed - confidence of 77% and support of 80% for 4 items

bk_rules3<-apriori(books,parameter = list(con=0.95,supp=0.95,minlen=4),control =list(verbose=F))
arules::inspect(bk_rules3)
#10637 rules observed - confidence of 95% and support of 95% for 4 items

bk_rules4<-apriori(books,parameter = list(con=0.98,supp=0.98,minlen=6),control =list(verbose=F))
arules::inspect(bk_rules4)
#7007 rules observed - confidence of 98% and support of 98% for 6 items

bk_rules5<-apriori(books,parameter = list(con=0.09,supp=0.09,minlen=5),control =list(verbose=F))
arules::inspect(bk_rules5)
#9317 rules observed - confidence of 09% and support of 09% for 5 items

bk_rules6<-apriori(books,parameter = list(con=0.07,supp=0.01,minlen=10),control =list(verbose=F))
arules::inspect(bk_rules6)
#110 rules observed - confidence of 7% and support of 1% for 10 items


rules.sorted<-sort(bk_rules6,by="lift")
arules::inspect(rules.sorted)
 

df<-data.frame(lhs=labels(lhs(rules.sorted)),rhs=labels(rhs(rules.sorted)),rules.sorted@quality)
head(df)



#windows()
#plot(bk_rules5,method = "scatterplot")
#plot(bk_rules5,method = "grouped")
#plot(bk_rules5,method = "graph")
#plot(bk_rules4,method = "mosaic")


###################################################################

## data set groceries
groceries <- read.transactions(file.choose(),format="basket")

dim(groceries)
head(groceries)

inspect(groceries[1:10])
# itemFrequencyPlot can be applicable only for transaction data 

# count of each item from all the transactions 
itemFrequencyPlot(groceries,topN=20)


rules_groceries<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
arules::inspect(rules_groceries)
#118 rules observed - confidence of 0.2% and support of 5% for 3 items

window()
plot(rules_groceries,method = "scatterplot")
plot(rules_groceries,method = "grouped")
plot(rules_groceries,method = "graph")
plot(rules_groceries,method = "mosaic")

rules_groceries1<-apriori(groceries,parameter = list(support = 0.01,confidence = 0.5,minlen=3))
arules::inspect(rules_groceries1)
#2 rules observed - confidence of 1% and support of 50% for 3 items

window()
plot(rules_groceries1,method = "scatterplot")
plot(rules_groceries1,method = "grouped")
plot(rules_groceries1,method = "graph")
plot(rules_groceries1,method = "mosaic")


rules_groceries2<-apriori(groceries,parameter = list(support = 0.01,confidence = 0.01,minlen=3))
arules::inspect(rules_groceries2)
#3 rules observed - confidence of 1% and support of 1% for 3 items

window()
plot(rules_groceries2,method = "scatterplot")
plot(rules_groceries2,method = "grouped")
plot(rules_groceries2,method = "graph")
plot(rules_groceries2,method = "mosaic")

rules_groceries3<-apriori(groceries,parameter = list(support = 0.006,confidence = 0.01,minlen=2))
arules::inspect(rules_groceries3)
rules.sorted_groceries<-sort(rules_groceries3,by="lift")
arules::inspect(rules.sorted_groceries)
#77 rules observed - confidence of 0.6% and support of 1% for 2 items

window()
plot(rules_groceries3,method = "scatterplot")
plot(rules_groceries3,method = "grouped")
plot(rules_groceries3,method = "graph")
plot(rules_groceries3,method = "mosaic")


df_gorceries<-data.frame(lhs=labels(lhs(rules.sorted_groceries)),rhs=labels(rhs(rules.sorted_groceries)),rules.sorted_groceries@quality)
head(df_gorceries)


######################################################

## data set movies 

movies <-read.csv(file.choose())

dim(movies)
head(movies)
colnames(movies)
movie_df <- movies[-c(1:5)]
summary(movie_df)

barplot(movie_df$Sixth.Sense)
barplot(movie_df$Gladiator)
barplot(movie_df$LOTR1)
barplot(movie_df$Harry.Potter1)
barplot(movie_df$Patriot)
barplot(movie_df$LOTR2)
barplot(movie_df$Harry.Potter2)
barplot(movie_df$LOTR)
barplot(movie_df$Braveheart)
barplot(movie_df$Green.Mile)

table(is.na(movie_df)) # 0 NaNs 

rules_movies_1<-apriori(movie_df,parameter = list(con=0.03,supp=0.07,minlen=2),control =list(verbose=F))
arules::inspect(rules_movies_1)
#5110 rules

rules_movies_2<-apriori(movie_df,parameter = list(con=0.5,supp=0.7,minlen=3),control =list(verbose=F))
arules::inspect(rules_movies_2)
#5020 rules

rules_movies_3<-apriori(movie_df,parameter = list(con=0.89,supp=0.95,minlen=3),control =list(verbose=F))
arules::inspect(rules_movies_3)
#5020 rules 

rules_movies_4<-apriori(movie_df,parameter = list(con=0.07,supp=0.95,minlen=3),control =list(verbose=F))
arules::inspect(rules_movies_4)
#5020 rules

rules_movies_5<-apriori(movie_df,parameter = list(con=0.07,supp=0.95,minlen=4),control =list(verbose=F))
arules::inspect(rules_movies_5)
#4460 rules with minlen = 4

rules_movies_6<-apriori(movie_df,parameter = list(con=0.50,supp=0.95,minlen=4),control =list(verbose=F))
arules::inspect(rules_movies_6)
#4460 rules

rules_movies_7<-apriori(movie_df,parameter = list(con=0.99,supp=0.99,minlen=4),control =list(verbose=F))
arules::inspect(rules_movies_7)
#


plot(rules_movies,method='graph')

rules.sorted<-sort(rules_movies,by="lift")
arules::inspect(rules.sorted)


df_movies<-data.frame(lhs=labels(lhs(rules.sorted)),rhs=labels(rhs(rules.sorted)),rules.sorted@quality)
head(df_movies)

