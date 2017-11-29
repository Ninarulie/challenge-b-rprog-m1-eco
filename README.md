# challenge-b-rprog-m1-eco

---
title: "ChallengeB"
output: html_document
---
TASK 1B: Prediction House prices in Ames, Iowa
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Load datas and packages
```{r Preparations1, include=FALSE}
install.packages("tidyverse","readr","randomForest")
```
```{r preparation1bis, include=FALSE, echo=TRUE}
library(tidyverse)
library(readr)
library(randomForest)
library(dplyr)
```
```{r preparation1ter: train and test, include=FALSE}
train<-read_csv("~/Rprog/train.csv")
test<-read_csv("~/Rprog/test.csv")
```

# We choose a ML technique : randomForest

## Prepare the datas
```{r Praparation2 : missing values, include=FALSE}
colnames(train)

Train2<-train[-1] # -> Here we remove id colomne

remove.vars <- Train2 %>% summarise_all(.funs = funs(sum(is.na(.)))) %>% gather(key = "feature", value = "missing.observations") %>% filter(missing.observations > 100) %>% select(feature) %>% unlist 
# -> Here we remove variables with a lot of missing observations (as we learned is ChallengeA)

Train2 <- Train2 %>% select(- one_of(remove.vars))

Train2 %>% summarise_all(.funs = funs(sum(is.na(.)))) %>% gather(key = "feature", value = "missing.observations") %>% filter(missing.observations > 0)
# -> We remove missing observations (as we learned in challenge A)

Train2 <-Train2 %>% filter(is.na(GarageType) == FALSE, is.na(MasVnrType) == FALSE, is.na(BsmtFinType2) == FALSE, is.na(BsmtExposure) == FALSE, is.na(Electrical) == FALSE)
# ->
```

```{r Praparation3 : convert character to factors, include=FALSE}
Train3 <-Train2%>%mutate_if(is.character,as.factor)
# -> we mutate character variable as factor
```

```{r Praparation4 : No illegal names, include=FALSE}
names(Train3)<-make.names(names(Train3))  
# ->  
```
## LM
```{r RandomForest, include=TRUE}
set.seed(1)
Train.fit<-randomForest(SalePrice~., data=Train3)
print(Train.fit)

```
->

## Prediction 
```{r Prediction, include=TRUE}
predict.RandomForest<-predict(Train.fit, test)
LinearRegression<-lm(data=Train3, SalePrice~.)
predict.LinearRegression<-predict(LinearRegression,test)
summary(predict.RandomForest)
summary(predict.LinearRegression)
```
Les résultats sont plus centrées autour de la moyenne lors de la prédiction de RandomForest

# Task 2B: Overfitting in Machine Learning

## Challenge A dataset and packages we need 
```{r Require Challenge A Task2, echo = TRUE, eval = TRUE, include = TRUE}
library(tidyverse)
library(caret)
set.seed(1)
x<-round(runif(150, min=-2, max=2.5),digits=2)
z<-rnorm(150,mean=0,sd=1)
y<-x^3+z 
yest=x^3
xy<-data.frame(y,x)
training_index<-createDataPartition(xy$x, p=0.8, list=FALSE)
training1<-slice(xy, training_index)
testing<-slice(xy, -training_index)
```
We just copied and pasted what we did dring the challenge A

# Step 1
## Low-flexibility local linear model

```{r Low-flexibility local linear model, include=TRUE}
ll.fit.lowflex<-npreg(bws=0.5, data=training1, regtype="ll", ydat=y, xdat=x)
summary(ll.fit.lowflex)
```
We create a low flexibility local linear model such that all the data of the set training1 are linked the one to the other by the model.

# Step 2
## High-flexibility Local model
```{r High-flexibility local linear model, include=TRUE}
ll.fit.highflex<-npreg(bws=0.01, data=training1, regtype="ll", ydat=y, xdat=x)
summary(ll.fit.highflex)
```
We create a higher flexibility local linear model such that ...

# Step 3

```{r Plot, include=TRUE}
y.high = predict(object = ll.fit.highflex, newdata = training1)
y.low = predict(object = ll.fit.lowflex, newdata = training1)
ggplot() + geom_point (data = training1,aes(x,y)) + geom_line(aes(x,yest), colour = "black", size = 1) +  geom_line (mapping = aes(x = x, y = y.high), colour = "red") + geom_line (mapping = aes(x = x, y = y.low), colour = "blue")
```
We plot the tow models to compare the one to the other.
