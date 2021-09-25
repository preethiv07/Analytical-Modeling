####K means####
####Report the best combination of predictors####
#### your suggested value of k####
####how well your best clustering predicts flower type###

##########IMPORT LIBRARY##########
library(caTools)
library("factoextra")#elbow chart
library("cluster") # to use cludgap for optimal k value
#install.packages("dplyr") ## install
library("dplyr") #summarize data
library("SwarmSVM") #predict package

##########CLEAR##########
rm(list = ls())

##########INGEST DATA##########
#data <-data("iris")
#data <- read.csv('iris.csv',header = T)
iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"))

head(iris,10)

dim(iris)

set.seed(100)

irisx=iris[,-5]
irisy=iris[5]

head(irisx,10)
head(irisy,10)

####ELBOW CHART - OPTIMAL VALUE OF K ####
#install.packages("factoextra")
#fviz_nbclust(irisx, kmeans, method = "wss") +geom_vline(xintercept = 3, linetype = 2)

#fviz_nbclust(irisx, kmeans, method = "silhouette")

# compute gap statistic
#set.seed(123)
#gap_stat <- clusGap(irisx, FUN = kmeans, nstart = 25,K.max = 10, B = 50)

# Print the result
#print(gap_stat, method = "firstmax")
#fviz_gap_stat(gap_stat) #clusters =6

##Output : K=3

##########TRAIN THE MODEL##########

split <- sample.split(iris,SplitRatio = 0.8)
train <- subset(iris ,split==T)
test <- subset(iris ,split==F)

irisx=iris [,-5]
irisy=iris [5]

testx=test [,-5]
testy=test[5]

kmeans_basic <- kmeans(irisx, centers = 3)
#to see the attributes of keamsn
names(kmeans_basic)


#Kmeans model
kmeans_basic_table <- data.frame(kmeans_basic$size, kmeans_basic$centers)
kmeans_basic_df <- data.frame(Cluster = kmeans_basic$cluster, iris)
#head(kmeans_basic_df,10)
#head(kmeans_basic_table,10)

# Example ggplot
#ggplot(data = kmeans_basic_df, aes(y = Cluster)) + geom_bar(aes(fill = irisy)) +ggtitle("Count of Clusters by Species") + theme(plot.title = element_text(hjust = 0.5))

#CLuster Plot
kmeans_clust1 <- kmeans(irisx, centers = 3,nstart=25)
fviz_cluster(kmeans_clust1, data = irisx, geom = c("point"),ellipse.type = "euclid")

kmeans_basic_df %>% mutate(Cluster = kmeans_basic$cluster) %>% group_by(Cluster) %>%   summarize(mean(kmeans_basic$clusters))

#BEST COMBINATION OF PREDICTORS
#Attribute Information:
#1. sepal length in cm
#2. sepal width in cm
#3. petal length in cm
#4. petal width in cm
#5. class:

head(iris,5)
km1 = kmeans(irisx$X5.1, 3, nstart=100)#Sepal length
km2 = kmeans(irisx$X3.5, 3, nstart=100)#Sepal width
km3 = kmeans(c(irisx$X3.5, irisx$X5.1), 3, nstart=100)#sepal length and sepal width
km4 = kmeans(irisx$X1.4, 3, nstart=100)#petal length
km5 = kmeans(irisx$X0.2, 3, nstart=100)#petal width
km6 = kmeans(c(irisx$X1.4, irisx$X0.2), 3, nstart=100)#petal length and petalwidth


Total within groups sum of squares (tot.withinss) 
km1$tot.withinss
km1 #betwee_sss/total_ss=84.5%#Sepal length
km2#betwee_sss/total_ss=81.1%#Sepal width
km3#betwee_sss/total_ss=92.0.1%#Sepal width and length
km4#betwee_sss/total_ss=94.7.1%#petal length 
km5#betwee_sss/total_ss=94.3.1%#petal width 
km6#betwee_sss/total_ss=91.6.1%#petal width and petal length

##WINNDER IS Petal Length with 94.7%

##########CLUSTERING PREDICTION##########
#k means data summarise data points
iris.df=data.frame(iris)
test.df=data.frame(testx)


#iris.df
#original data (Supervised output)
iris.df %>% group_by(Iris.setosa) %>%  summarise(n = n())
#Unsupervised ouytput
#head(kmeans_basic_df ,50)
####1:setosa
####2: versicolor
####3:virginica
kmeans_basic_df %>% group_by(Cluster) %>%  summarise(n = n())
plot(irisx$X1.4, irisx$X0.2, pch=21, bg=c("red","green3","blue")[unclass(iris$Iris.setosa)], main="Iris Data")


#plot(wt, mpg, main="Scatterplot Example",xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)

#No predict option for new dataset in unsupervised model

#TEST DATA
kmeans_test <- kmeans(testx, centers = 3)
kmeans_test_table <- data.frame(kmeans_test$size, kmeans_test$centers)
kmeans_test_df <- data.frame(Cluster = kmeans_test$cluster, testx)
kmeans_test_df %>% group_by(Cluster) %>%  summarise(n = n())