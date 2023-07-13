#data prcessing

#importing 
dataset=read.csv('Data.csv')

#indexes start from 1 

#missing data pr work 
#first solution rmove the line empty wali
#but this line may contain vital info

#second method replace the null data with mean of other data

                   #condtion of null   place the avg where it is null                      else place the element
dataset$Age=ifelse(is.na(dataset$Age),ave(dataset$Age,FUN = function(x)mean(x,na.rm = TRUE)),dataset$Age)

dataset$Salary=ifelse(is.na(dataset$Salary),ave(dataset$Salary,FUN = function(x)mean(x,na.rm = TRUE)),dataset$Salary)

#encoding categorical data
#labels of factors
dataset$Country=factor(dataset$Country,levels = c('France','Spain','Germany'),
                       labels =c(1,2,3) )

#spiltting the data
#import a library 
install.packages('caTools')
library(caTools)  #activate for this instnt on;y 
set.seed(123)  #genrates seed t geneneate random numbers
split=sample.split(data)

