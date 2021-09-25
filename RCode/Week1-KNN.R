library(kknn)

#read#
#data<- read.csv("credit_card_data-headers.txt",header=TRUE,stringsAsFactors = TRUE,sep="\t")
data<- read.table("credit_card_data-headers.txt",header=TRUE,stringsAsFactors = TRUE,sep="\t")


#view in data frame
str(data)

#get media, mean, 1st, 2nnd and 3rd quartle
summary(data)

# to get demographics of data
dim(data)

#undersand the distribution of the results
table(data$R1)

library(kknn)

#data[-i,] =data except for the ith data point.  
#data[-i,11] = response of all but the ith data point
#data[-i,1:10] are the predictors for all but the ith data point.  

km = 20

model <- train.kknn(R1~.,data,km=km,scale=TRUE)

slotNames(model)


#for (i in 1:nrow(data)) {
#    train.data <- data[-i,]
#    validation.data <- data.train[i,]
#    knn.fit <- kknn(as.factor(data[-i,]), train.data, validation.data, k = 40,
#                    kernel = "rectangular", scale = TRUE)
