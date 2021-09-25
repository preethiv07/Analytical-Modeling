###Question 5.1

###Input : http://www.statsci.org/data/general/uscrime.txt

####ASK: test to see whether there are any outliers in the last column (# of crimes per 100k)

####OPTIONS: Use the grubbs.test function in the outliers package in R.


##########CLEAR##########
rm(list = ls())
set.seed(100)

##########LIBRARY##########
#install.packages("outliers")
library(outliers)
data<- read.table("5_1uscrime.txt",header=TRUE,stringsAsFactors = FALSE,sep="\t")
head(data,10)

#Scatterplot
plot(data$Crime)

# Grubbs test allows to detect whether the highest or lowest value in a dataset is an outlier.
###DETECT HIGH OUTLIER###
Outhigh <- grubbs.test(data$Crime)
Outhigh 

## if the p-value <= (a=0.05),then the null hypothesis is rejected 
## THEN we will conclude that the lowest/highest value is an outlier. 

### p-value >=0.05,null hypothesis is not rejected,
### we do not reject the hypothesis that the lowest/highest value is not an outlier.


###DETECT LOW OUTLIER
Outlow <- grubbs.test(data$Crime,opposite = TRUE)
Outlow

dataY=data$Crime
which(dataY== 1993)
which(dataY== 342)

head(data,27)

###ORDER CRIME DATA
attach(data)
newdata <- data[order(Crime),]
newdata
