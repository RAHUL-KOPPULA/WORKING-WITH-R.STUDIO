setwd("D:/DataSet")
mydata <- read.csv("iris.csv", header = TRUE, sep = ",")
subset(mydata, petal_length > 1.7)
subset(mydata, species == "setosa" & petal_length > 1.7)
#Find mean points by team
aggregate(mydata$petal_length, by=list(mydata$species), FUN=mean)


#mport a CSV file from a URL using Base R:
data2 <- read.csv('https://raw.githubusercontent.com/statology/Miscellaneous/main/basketball_data.csv')
head(data2)

#Reading text fle from disk

data <- read.table(file = "D://MyPath//Data_Analysis.txt", header = TRUE)
df<-read.table(file="D:/7090/min.txt",header=TRUE)
df
url<-"https://github.com/itsfoss/text-files/blob/master/sample_log_file.txt"
data<-read.table(url,header=TRUE,sep=',')
data
df<-data.frame(var1=c(1,2,3),
               var2=c(4,5,6),
               var3=c(7,8,9))
df
write.table(df,file="https://filesamples.com/samples/document/txt/sample2.txt")
df
df<-data.frame(name=c("rahul","koppula","aids"),
              age=c(23,22,21))
df
write.csv(df,"C:/Users/Student/Downloads/neraj.csv")
df
install.packages('readx')
library(readxl)
data<-read_excel("C:/Users/Student/Downloads/asparas.xls")
data
#rreaing an xml file
install.packages('XML')
library(XML)
library(xml)
RES<-xmlParse(file ="C:/Users/Student/Downloads/metadata-xxe.xml")
print(RES)
df<-xmlToDataFrame("C:/Users/Student/Downloads/metadata-xxe.xml")
df
data(iris)
#creating seperate boxplots for eCH ATTRIBUTE
par(mfrow=c(1,4))
for (i in 1:4)
  {
    boxplot(iris[,i],main=names(iris)[i])}
View(iris)
#data distribution using scatterplots
install.packages("ggplot2")
library("ggplot2")
ggplot(iris,aes(Sepal.Length,Sepal.Width))+
  geom_point()
#data to load
data(iris)
#pair_wise catterplots of all 4 attributes
pairs(iris)
#creating an Histogram
set.seed(1)
#define data
x1=rnorm(1000,mean=0.8,sd=0.2)
x2=rnorm(1000,mean=0.4,sd=0.1)
#plots of two histograms in same grsph
hist(x1,col='red',xlim=c(0,1.5),main='Multiple histograms',xlab='x')
hist(x2,col='green',add=TRUE)
#add legend
legend('topright',c('x1 variable','x2 variable'),fill=c('red','green'))
#creating Bar Graphs
ggplot(mtcars,aes(x=cyl))+
  geom_bar()
#creating pie charts
count<-c(5,25,30,40)
pie(count,labels = count)
pie(count,labels=paste()(count,"%"))
#create a corrlstion mstrix
#a correlstion mstrix is a square  table thst shows hte correlation coeffecients between
df<-data.frame(assists=c(4,5,6,7,8),
               rebounds=c(25,4,80,0,1),
               points=c(12,33,55,77,99))
df 
attributes(df)
cor(df)
#the corrplot function
#we can use the corrplot fun() oin corrplot package in r to visual correlation
install.packages("corrplot")
library("corrplot")
corrplot(cor(df))
#logistic algorithm
mydata<-read.csv("http://www.karlin.mff.cuni.cz/~pesta/prednasky/NMFM404/Data/binary.csv")
#to display the atributes of dataframe
attributes(mydata)
View(mydata)
#to show the column names of dataframe
ls(mydata)
#to display the structure of data
str(mydata)
head(mydata)
summary(mydata)
mydata$rank<-factor(mydata$rank)
mydata$rank
mylogit<-glm(admit~gre+gpa+rank,data=mydata,family = "binomial")
mylogit
summary(mylogit)
newdata=data.frame(gre=250,gpa=9.01,rank="4")
newdata
#use model to predict values
predict(mylogit,newdata,type='response')
data<-mtcars[,c("mpg","hp","disp","drat")]
head(data)
View(data)
pairs(data,pch=0,col="black")
model<-lm (mpg~disp+hp+drat,data=data)
model
#the distribution of model residuals should be appoximately normal
hist(residuals(model),col="steelblue")
summary(model)
str(states.data$region)
states.data$region<-factor(states.data$region)
sat.region<-lm(csat~region,data=states.data)
coef(summary(sat.region))

rm(lists=ls())
setwd('D:/')
library('class')
library('caret')
diabetes=read.csv('D:/r programs/diabetes 311.csv')
View(diabetes)

str(diabetes)
mean(diabetes$Age)
summary(diabetes)

train=diabetes[1:500,]
test=diabetes[501:768,]
pred_test=knn(train[,-9],test[,-9],train$Outcome,k=2)
pred_test

confusion=table(pred_test,test$Outcome)
sum(diag(confusion))/nrow(test)
confusionMatrix(pred_test,test$Outcome)
# k means alghorithm
install.packages("factoextra")
library(factoextra)
library(cluster)
df<-USArrests
df<-na.omit(df)
df<-scale(df)
fviz_nbclust(df,kmeans,method='wss')
set.seed(1)
km<-kmeans(df,centers=4,nstart=25)
km
fviz_cluster(km,data=df)
aggregate(USArrests,by=list(cluster=km$cluster),mean)
final_data<-cbind(USArrests,cluster=km$cluster)
head(final_data)