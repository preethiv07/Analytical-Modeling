#clear
rm(list = ls())
#read
#working data <- read.csv("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)
#data <- read.csv("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE,sep="/t")
data <- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)


#sample 
head(data,10)

library(caTools)
library(class)

set.seed(123)

split <- sample.split(data,SplitRatio = 0.8)
head(split,10)
train <- subset(data,split==T)
test <- subset(data,split==F)

#head(test,10)

#SPLIT DEPENDANT(y/results) attribute and INDEPENDENT predictors

#independant predictor (ignore last firld, field#5)
trainx <- train[,-11]
testx <- test[,-11]

#head(train,10)
head(trainx,10)

#result/y (identify by name of the header)
trainy <- train[11]
testy <- test[11]

head(trainy,10)

## MANDATORY!!!! SCALING 
trainx <- scale(trainx)
testx <- scale(testx)

head(trainx,10)
head(trainy,10)

cl = trainy[,1, drop = TRUE]
cl
trainy=trainy[,1, drop = TRUE]
trainy

trainx_df <- as.data.frame(trainx)
trainy_df <- as.data.frame(trainy)

dim(trainx)
dim(trainy)
length(trainy)

#CREATE TRAIN MODEL FOR ONE "K" VALUE
#TRAIN MODEL
knn.pred <- knn(trainx, trainx, cl, k=7)
knn.pred
#TRAIN MODEL ACCURACY
sum(trainy == knn.pred)
NROW(trainy)
train_accuracy <- 100 * sum(trainy == knn.pred)/NROW(trainy)
train_accuracy

#1st MODEL TRAIN +ACCURACY FOR RANGE OF "K" VALUE FROM 1 to 15

train_accuracy=1
i=1
set.seed(0)
for (i in 1:15){
   knn.pred <- knn(trainx, trainx, trainy, k=i)
   train_accuracy[i] <- 100 * sum(trainy == knn.pred)/NROW(trainy)
   a=i
   cat(a,'=',train_accuracy[i],'')
}

#clear
rm(list = ls())
#read
#working data <- read.csv("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)
#data <- read.csv("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE,sep="/t")
data <- read.table("credit_card_data.txt", stringsAsFactors = FALSE, header = FALSE)


#sample 
head(data,10)

library(caTools)
library(class)

set.seed(123)

split <- sample.split(data,SplitRatio = 0.8)
head(split,10)
train <- subset(data,split==T)
test <- subset(data,split==F)

#head(test,10)

#SPLIT DEPENDANT(y/results) attribute and INDEPENDENT predictors

#independant predictor (ignore last firld, field#5)
trainx <- train[,-11]
testx <- test[,-11]

#head(train,10)
head(trainx,10)

#result/y (identify by name of the header)
trainy <- train[11]
testy <- test[11]

head(trainy,10)

## MANDATORY!!!! SCALING 
trainx <- scale(trainx)
testx <- scale(testx)

head(trainx,10)
head(trainy,10)

trainy=trainy[,1, drop = TRUE]
trainy
testy=testy[,1, drop = TRUE]
testye


trainx_df <- as.data.frame(trainx)
trainy_df <- as.data.frame(trainy)

dim(trainx)
dim(trainy)
length(trainy)

#CREATE TRAIN MODEL FOR ONE "K" VALUE
#TRAIN MODEL
knn.pred <- knn(trainx, trainx, cl, k=7)
#knn.pred
#TRAIN MODEL ACCURACY
sum(trainy == knn.pred)
NROW(trainy)
train_accuracy <- 100 * sum(trainy == knn.pred)/NROW(trainy)
train_accuracy

#1st MODEL TRAIN +ACCURACY FOR RANGE OF "K" VALUE FROM 1 to 15

train_accuracy=1
i=1
set.seed(0)
for (i in 1:15){
   knn.pred <- knn(trainx, trainx, trainy, k=i)
   train_accuracy[i] <- 100 * sum(trainy == knn.pred)/NROW(trainy)
   a=i
   cat(a,'=',train_accuracy[i],'')
}

set.seed(123)

#TEST MODEL 1 k value
knn.mod <- knn(train=trainx, test=testx, cl=trainy, k=i)
knn.mod

#2nd MODEL TEST +ACCURACY FOR RANGE OF "K" VALUE FROM 1 to 15
test_accuracy=1
i=1
for (i in 1:15){
  knn.mod <- knn(train=trainx, test=testx, cl=trainy, k=i)
  test_accuracy[i] <- 100 * sum(testy == knn.mod)/NROW(testy)
  k=i
  cat(k,'=',test_accuracy[i],'
')
}
compare <- cbind(train_accuracy,test_accuracy)
compare
