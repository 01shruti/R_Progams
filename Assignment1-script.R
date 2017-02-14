##################################################################################################################################
#To explore dataset orange juice for some specific functions
#Author: Shruti Goyal (16200726) Full Time
##################################################################################################################################

#rm() is used to remove other objects from the environment
rm(list=ls())

#To check the working directory
getwd()
#A specific working drectory needs to be set for the loading of dataset
setwd("G:/Smurfit/Course Materials/Data Mining/Assignments Week 4")
#'oj' will load the data into R from a csv file
oj <- read.table("oj.csv",header=TRUE,sep=",")
#'oj2' & 'oj3' are also used to load data into R from csv file but both will generate different outputs
oj2 <- read.csv("oj.csv",header=TRUE)
oj3 <- read.csv2("oj.csv",header=TRUE)

##################################################################################################################################
#dim() to calculate number of rows and columns
dim(oj)
#attributes() to generate the details of the dataset attributes
attributes(oj)
attach(oj)

##################################################################################################################################
#Following commands will generate statistics data
mean(oj$price)
sd(oj$price)
range(oj$price)

#summary() will generate basic statistics variables for each of the variable in dataset
summary(oj)

median(oj$logmove)

##################################################################################################################################
#factor() will result in types of category data
brands <- factor(oj$brand)
table(brands)

##################################################################################################################################
#hist() to generate histograms for price respective of brand
hist(oj$price[oj$brand=='dominicks'])
hist(oj$price[oj$brand=='minute.maid'])
hist(oj$price[oj$brand=='tropicana'])

#To generate a combined histogram for each brand
install.packages("ggplot2")
library(ggplot2)
ggplot(oj, aes(x= oj$price, fill = brands)) + geom_histogram(binwidth=1,position = "dodge")

##################################################################################################################################
#boxplot() to generate plot between price and brand
boxplot(price~brand, data=oj, col=(c("green","gold","pink")),main="Boxplot",xlab="Price and brand")

##################################################################################################################################
#To generate scatterplot by two ways pairs() and
pairs(oj$logmove~oj$price,main="Plot for logmove against price for each brand",pch=21,col=rainbow(3)[unclass(brands)])
plot(oj$logmove~oj$price,main="Plot for logmove against price for each brand",xlab="Price",ylab="logmove",pch=21,col=rainbow(3)[unclass(brands)])
legend("bottomleft",inset = .005, title = "Brands", c("Dominicks","Minute maid","Tropicana"),fill=rainbow(3),horiz=FALSE)

##################################################################################################################################
#To calculate mean price of orange juice sold each week and plotting a time series graph for same
meanprice <- tapply(oj$price,oj$week,FUN=mean,na.rm=TRUE)
plot.ts(meanprice,xaxt="n",pch=16,type="l",col="green",main="Time series graph between mean price and week",xlab = "Week",ylab="Mean Price")

##################################################################################################################################
#To extract mean weekly price of orange juice according to each brand
tapply(oj$price, INDEX=list(oj$week,brands),FUN=mean,na.rm=TRUE)

##################################################################################################################################
#To generate a plot which compares mean of price of orange juice for all brands versus each individual brand
indbrand <- tapply(oj$price, INDEX=list(oj$week,oj$brand),FUN=mean,na.rm=TRUE)
allbrands <- tapply(oj$price,oj$week,FUN=mean,na.rm=TRUE)
frame <- cbind(allbrands,indbrand)
write.table(df, file = 'brand.csv',sep = ",",row.names = T)
library(readr)
branddata <- read_csv("brand.csv",col_names = FALSE, skip = 1)
ggplot(data = branddata, aes(branddata$X1,xlab="Week")) + 
  geom_line(aes(y = branddata$X2, colour = "All Brands")) +
  geom_line(aes(y = branddata$X3, colour = "Dominicks")) + 
  geom_line(aes(y = branddata$X4, colour = "Minute Maid")) + 
  geom_line(aes(y = branddata$X5, colour = "Tropicana")) +
  xlab("Week") +ylab("Price")

##################################################################################################################################
#To determine changes between number of units sold when there is advertising campaign
adcap <- factor(oj$feat)
tapply(oj$logmove,adcap,FUN=sum,na.rm=TRUE)
#Affected sales by advertisement based on brands
sales <- tapply(exp(oj$logmove), oj[,c("feat","brand")], sum)
print(sales)
mosaicplot(salestable,col=rainbow(3), main = "Affected sales on using advertisement", xlab = "Advertisement (0- without promotion, 1- With promotion)", ylab = "Brand")

##################################################################################################################################
#To plot graph between mean weekly price without a promotion and with a promotion
pdata <- tapply(oj$logmove,list(Week=oj$week,oj$feat),FUN=mean,na.rm=TRUE)
df1 <- cbind(pdata)
write.table(df1, file = 'Promotion.csv',sep = ",",row.names = T)
library(readr)
prom <- read_csv("G:/R_programs_git/R_Progams/Promotion.csv", col_names = FALSE, skip = 1)
plot(prom$X1,prom$X2,type="l",col="red",main="Line plot between mean weekly price without promotion and with promotion",xlab = "Week", ylab = "Promotions")
lines(prom$X1,prom$X3,col="green")
legend("bottomright",c("Without Promotion","With Promotion"),fill= c("red","green"),horiz=FALSE)
