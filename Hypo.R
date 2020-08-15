#Hpyothesis testing

#Cutlets data set

library(readxl)
cutlets_ds<-read.csv("C:/Users/Acer/Downloads/Datasets/Hypo/Cutlets.csv")
dim(cutlets_ds)
str(cutlets_ds)
na_test<-table(is.na(cutlets_ds))
print(na_test) #0 null/nans
summary(cutlets_ds)
unit_a<-cutlets_ds$Unit.A
unit_b<-cutlets_ds$Unit.B
boxplot(cutlets_ds)
hist(unit_a)
skewness(unit_a)
kurtosis(unit_a)
hist(unit_b)
skewness(unit_b)
kurtosis(unit_b)
mean_a<-mean(unit_a)
mean_b<-mean(unit_b)

#Ho:Ma=Mb
#Ha:Ma!=Mb
t.test(unit_a,unit_b,alternative = "two.sided",mu=0)
#alternate hpyo is ture as both mean are not same, hence units A and b are not significant in nature.


##labtat data set 
labtat<-read.csv("C:/Users/Acer/Downloads/Datasets/Hypo/labTaT.csv")
dim(labtat)
str(labtat)
summary(labtat)
head(labtat)
names(labtat)[1]<-"lab1"
names(labtat)[2]<-"lab2"
names(labtat)[3]<-"lab3"
names(labtat)[4]<-"lab4"
boxplot(labtat)
head(labtat)

aovresults<-aov(lab1~.,data = labtat)

summary(aovresults)

aov2<-aov(lab2~.,data = labtat)
summary(aov2)
# Lab2 and lab3 are significant

aov3<-aov(lab3~.,data = labtat)
summary(aov3)
aov4<-aov(lab4~.,data = labtat)
summary(aov4)
  ## only the mean TAT for lab2 and lab3 are similar.

##Buyer ratio data set 
BuyR<-read.csv("C:/Users/Acer/Downloads/Datasets/Hypo/BuyerRatio.csv")
str(BuyR)
dim(BuyR)
head(BuyR)
chisq.test(BuyR[-1])
#p value is 0.6603
#P value is greater than aplha hence we accept null hypothesis i.z All proportions are equal.

############
#custmor order form data

cust<-read.csv("C:/Users/Acer/Downloads/Datasets/Hypo/Costomer+OrderForm.csv",header =T )
attach(cust)
dim(cust)
str(cust)
na_test.c<-table(is.na(cust))
print(na_test.c)
head(cust)
summary(cust)        
p<-table(Phillippines)
sp<-stack(p)
i<-table(Indonesia)
si<-stack(i)
m<-table(Malta)
sm<-stack(m)
I<-table(India)
sI<-stack(I)
new<-cbind.data.frame(sp[2],sp[1],si[1],sm[1],sI[1])
names(new)<-c(" ","Phillippines","Indonesia","Malta","India")
head(new)
new1<-as.data.frame(t(new))
chisq.test(new[-1])
#P-value is 0.0277
#As pvalue is less than alpha(0.05),we reject null hpyothesis
#the defective % varies from center to center

###########################################################

##Faltoons data set

Faltoons<-read.csv("C:/Users/Acer/Downloads/Datasets/Hypo/Faltoons.csv")
attach(Faltoons)
dim(Faltoons)
str(Faltoons)
summary(Faltoons)
k