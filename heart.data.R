library(tidyverse)
dataset1=read.csv("Heart_Validation.csv")
dataset2=read.csv("heart.data.csv")
#exploring heart validation data
head(dataset1)
glimpse(dataset1) 
length(dataset1)
summary(dataset1)
#exploring heart data
head(dataset2)
glimpse(dataset2) 
length(dataset2)
summary(dataset2)
#finding missing values of both data sets
colSums(is.na(dataset1))
colSums(is.na(dataset2))

ggplot(data=dataset2,
       aes(biking))+
  geom_histogram()
biking_median=median(dataset2$biking, na.rm=TRUE)
dataset2$biking=ifelse(is.na(dataset2$biking),
                   biking_median,
                   dataset2$biking)
colSums(is.na(dataset2))

ggplot(data=dataset2,
       aes(smoking))+
  geom_histogram()
smoking_median=median(dataset2$smoking, na.rm=TRUE)
dataset2$smoking=ifelse(is.na(dataset2$smoking),
                       smoking_median,
                       dataset2$smoking)
colSums(is.na(dataset2))

ggplot(data=dataset2,
       aes(heart.disease))+
  geom_histogram()
heart.disease_median=median(dataset2$heart.disease, na.rm=TRUE)
dataset2$heart.disease=ifelse(is.na(dataset2$heart.disease),
                       heart.disease_median,
                       dataset2$heart.disease)
colSums(is.na(dataset2))

#multiple linear regression
library(caTools)
set.seed(100)
split=sample.split(dataset2$heart.disease, SplitRatio=0.8) #80% training, 20% test set
Training_Set=subset(dataset2,split=TRUE)
Test_Set=subset(dataset2,split=FALSE) 

names(dataset2)
MLR=lm(formula=heart.disease~ .,
       data=Training_Set)
summary(MLR)

#multiple r-squared is 0.9765, adjusted r-squared is 0.9764 for the heart.data

#mean square error
summ=summary(MLR)
MSE=(mean(summ$residuals^2))
paste("Mean squared error", MSE)

#mean squared error 0.492962

#R square
summary(MLR)

#testing set prediction
y_pred=predict(MLR,newdata=Test_set)
data=data.frame(Test_set$heart.disease,y_pred)
head(data)

#validation
new=read_csv('Heart_Validation.csv')
head(new)
new_x=new[c(1:6)]
new_x
data.frame(new[c(6)],predict(MLR,newdata=new_x))


