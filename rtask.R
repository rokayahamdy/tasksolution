library(mice)
library(tidyr)
library(tidyverse)
library(ggplot2)
#read file
getwd()
TaskDataSet<-read.csv("G4_howell.csv")
#get dimension
dim(TaskDataSet)
#view dataset
View(TaskDataSet)
#number of rows
nrow(TaskDataSet)
#number of colums
ncol(TaskDataSet)
#Data structure
str(TaskDataSet)
#Data summary
summary(TaskDataSet)
#Get first N rows   n=10
head(TaskDataSet)
#Get last N rows   n=10
tail(TaskDataSet)
#Filter the dataset according to specific criteria
filter1<-TaskDataSet[TaskDataSet$sex=='M',]
filter2<-TaskDataSet[TaskDataSet$weight>36.5,c(1,2,3)]
filter3<-TaskDataSet[TaskDataSet$weight>36.5&TaskDataSet$height>153.3,-c(1,2)]
#Re-coding means use different values for a variable
TaskDataSet$newsex[TaskDataSet$sex=='F']='Female'
TaskDataSet$newsex[TaskDataSet$sex=='M']='Male'
View(TaskDataSet)
#To Simplify the analysis convert it to factor
TaskDataSet$newsex<-as.factor(TaskDataSet$sex)
#Re-code with multicondition
TaskDataSet$newheight[TaskDataSet$height<160]='short'
TaskDataSet$newheight[TaskDataSet$height>=160]='tall'
View(TaskDataSet)
#Re-code of code
TaskDataSet$newheight2[TaskDataSet$newheight=='short']='1'
TaskDataSet$newheight2[TaskDataSet$newheight=='tall']='2'
View(TaskDataSet)
#Sorting the dataset according to specific columns
sort1<-TaskDataSet[order(TaskDataSet$age),]
sort1
sort2<-TaskDataSet[order(-TaskDataSet$age),]
sort2
#Add NA in all missing data cells.
TaskDataSet<-read.csv('G4_howell.csv',na.strings =c('<Na>'))
View(TaskDataSet)
#To get all locations of NA
complete.cases(TaskDataSet)
#Get all rows contain missing data
TaskDataSet[!complete.cases(TaskDataSet),]
#Removing text in numeric values
TaskDataSet$weight<-gsub('kg','',TaskDataSet$weight)
View(TaskDataSet)
TaskDataSet$weight<-as.numeric(TaskDataSet$weight)
#fill missing values by using multiple imputation 
pre.imputation<-mice(TaskDataSet,m=5,meth=c('','','pmm','pmm'),maxit=20)
#get predict values
per.imputation$imp
#Complete the missing values with the selected predicted values
newTaskDataSet<- complete(pre.imputation,5)
#get all row with missing data for specific variable
is.na(TaskDataSet)
missingvalues<-TaskDataSet[is.na(TaskDataSet$overweight),]
#Get and fill rows of a variable based on NA in another variable
missingweight<-TaskDataSet[is.na(TaskDataSet&weight)&TaskDataSet$age>29,]
TaskDataSet[is.na(TaskDataSet&weight)&TaskDataSet$age>29,weight]='NY'
clean <- drop_na(TaskDataSet)
#Using if-else statement
x <- mean(clean$age)
clean$menage <- as.factor(ifelse(clean$age > x, "old", "young"))
#Calculate the median of a variable with the use of na.rm
median(TaskDataSet[ , ’weight’]) , na.rm = T )
#data visualization
draw1 <- ggplot(TaskDataSet)
draw1 <- ggplot(TaskDataSet, aes(x=age, y=weight))
draw1 + geom_point()
#histogram
draw2 <- ggplot(TaskDataSet)
draw2 <- ggplot(TaskDataSet, aes(Age))
draw2 + geom_histogram(binwidth = 5,color="red",fill="blue",alpha = 0.5)
#bar chart
draw3 <- ggplot(TaskDataSet)
draw3 <- ggplot(TaskDataSet, aes(x=weight, fill = sex))
draw3 + geom_bar()


