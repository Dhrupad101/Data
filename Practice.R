1+1
2+3*4
3^2
exp(1)
sqrt(10)
pi
2*pi*6378
x<-1
y<-3
z<-5

my_vect<-c(1,2,3,4,'a',TRUE)

my_vect<-c(0,2,1,5)
my_vect[4]

x<-c(2,0,0,4)

x[1]
x[-1]

x[1] <-3 ; x
x[-1]<-5 ; x

y<-c(2,10,9,5)

y<9

y[4]<-1; y

y[y<9]<-2;y

don<-c(2,3,4,5,6,7,8,9)
don[5]<-99
don

#Data Frame contruction 
df<- data.frame(x=1:3,y=c("a","b","c"))

df1<-data.frame(x=c(1,4,9),y=c("a","b","c"))
df1
View(df1)

#Slicing (extracting value from a data frame)
#rows and columns
# send the saved programme to rachitmishra82@gmail.com
#Frist row
df[1,c(1,2)]
#2nd colum
df[c(1:3),c(1,2)]
#1st row,2nd row and 2nd Column
df[c(1,2),2]
#1st cloumn, 2nd row,2 column
df[2,c(1,2)]
#3rdrow
df[3,c(1,2)]
#first row
df[1,]
#2nd column
df[,2]

