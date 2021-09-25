#Data set: credit_card_data.txt
# find good classifier:
#####3.1 (b)###
#####	splitting the data into training, validation, and test data sets ##
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

#Split Data 
dim(data)
split <- sample.split(data,SplitRatio = 0.7)
train <- subset(data,split==T) #TRAINING

dim(train)

# reamining 30% is Test and validdation data
test_val <- subset(data,split==F)

# split reamining 30% equally between  Test and validdation data
split2 <- sample.split(test_val,SplitRatio = 0.5)
test <- subset(test_val,split2==T) #TEST
val <- subset(test_val,split2==F) #VALIDATION

dim(test)
dim(val)

# 654 =416(train)+ 109(test)+129(validation)

#head(test,10)

#SPLIT DEPENDANT(y/results) attribute and INDEPENDENT predictors

#independant predictor (ignore last firld, field#5)
trainx <- train[,-11]
testx <- test[,-11]
valx <- val[,-11]


#head(train,10)
#head(trainx,10)

#result/y (identify by name of the header)
trainy <- train[11]
testy <- test[11]
valy <- val[11]


## MANDATORY!!!! SCALING 
trainx <- scale(trainx)
testx <- scale(testx)
valx <- scale(valx)

#head(trainx,10)
#head(testx,10)
#head(valx,10)

trainy=trainy[,1, drop = TRUE]
trainy
testy=testy[,1, drop = TRUE]
trainy
valy=valy[,1, drop = TRUE]
trainy

dim(trainx)
dim(trainy)
length(trainy)


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

#TEST MODEL for ONE k value
knn.mod <- knn(train=trainx, test=testx, cl=trainy, k=3)
knn.mod
#TEST MODEL ACCURACY
test_accuracy <- 100 * sum(testy == knn.mod)/NROW(testy)
test_accuracy

set.seed(123)

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

set.seed(123)

#VALIDATION MODEL 1 k value
knn.val <- knn(train=trainx, valx, cl=trainy, k=3)
knn.val
#VALIDATIOn MODEL ACCURACY
val_accuracy <- 100 * sum(valy == knn.val)/NROW(valy)
val_accuracy

set.seed(123)

#3rd MODEL VAIDATION +ACCURACY FOR RANGE OF "K" VALUE FROM 1 to 15
val_accuracy=1
j=1
for (j in 1:15){
  knn.val <- knn(train=trainx, test=valx, cl=trainy, k=j)
  val_accuracy[j] <- 100 * sum(valy == knn.val)/NROW(valy)
  k=j
  cat(k,'=',val_accuracy[j],'
')
}

compare <- cbind(train_accuracy,test_accuracy,val_accuracy)
compare

#set.seed(100)
#plot(range(1:15),train_accuracy)

#p=1
#for (p in 1:15){
#	plot(p,val_accuracy[p])
	#plot(p,val_accuracy[p], type="l", col="green", lwd=1, xlab="K-value", ylab="Accuracy")
	#lines(p,val_accuracy[p], col="red", lwd=2)
	#title("Accuarcy measurement by data set")
#}
