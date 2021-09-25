###Question 6.2###

###Input : July through October daily-high-temperature data for Atlanta for 1996 through 2015,\
####ASK: use a CUSUM approach to identify when unofficial summer ends 
####(i.e., when the weather starts cooling off) each year.  
####OPTIONS: You can use R if you’d like, OR Excel spreadsheet


##########CLEAR##########
rm(list = ls())

##########LIBRARY##########
library(zoo)

##########INGEST FILE##########


data<- read.table("6_2temps.txt",header=TRUE,stringsAsFactors = FALSE,sep="\t")
#summary(data)
length(data) # no of columns

head(data,10)

##########ggplot(data)##########
date_avgs <- rowMeans(data[c(2:length(data))], dims=1, na.rm=T)
cbind(data[1],date_avgs)

##########CHECK NUMBER OF ROWS##########
n <- length(date_avgs) # 123 data points
ntest <- nrow(data[1])

x_t <- date_avgs

mean_x_t <- mean(x_t)

mean_x_t

# set up based on analysis in excel
C <- 5

#Threshold for Temp drop  set to 82.

#plot average
#plot(date_avgs,ylim=c(50,110),xlab="date",ylab="Avg",type="l")

# as we are seeing decrease in temperature, we calculate mean - data

mean_data <- mean_x_t-date_avgs 

# subtract C from the difference score
s_t <- mean_data - C

s_t

precusum <- 0 * s_t
cusum <- append(precusum, 0)

cusum



# read in the temp data

data<- read.table("6_2temps.txt",header=TRUE,stringsAsFactors = FALSE,sep="\t")
#summary(data)
length(data) # no of columns

#head(data,10)
#head(data[2:length(data)],5)

#ggplot(data)
date_avgs <- rowMeans(data[c(2:length(data))], dims=1, na.rm=T)
cbind(data[1],date_avgs)

# no of rows
n <- length(date_avgs) # 123 data points
ntest <- nrow(data[1])

#x_t => point of observation
x_t <- date_avgs

#calculate mean
mean_x_t <- mean(x_t)
mean_x_t

# set up based on analysis in excel
C <- 5

#SET UP Threshold for Temp drop  set to 82.
#plot average
#plot(date_avgs,ylim=c(50,110),xlab="date",ylab="Avg",type="l")

# as we are seeing decrease in temperature, we calculate mean - data

mean_data <- mean_x_t-date_avgs 

# subtract C from the difference score
s_t <- mean_data - C

s_t

#precusum <- 0 * s_t
cusum <- append(0, 0)
cusum
cusum[1]

for (i in 1:length(s_t)) 
     {
  ifelse(cusum[i] + s_t[i-1] > 0, cusum[i+1] <- cusum[i] + s_t[i-1], cusum[i+1] <- 0) 
}

cusum
length(cusum[-1])
cbind(data[1],cusum[-1])

which(cusum >= 82)

data[116, 1]

#use in final plot
plot(cusum[-1],ylab='CUSUM')

########HAS ATLANTA CLIMATE GOT WARMER BY YEARS?##########
# categorize the 20 years data into 5 buckets of 5 years

yearset_1 <-data[2:6]
head(yearset_1)
yearset_2 <-data[7:11]
head(yearset_2)
yearset_3 <-data[12:16]
head(yearset_3)
yearset_4 <-data[17:length(data)]
head(yearset_4)

yearset1_mu= rowMeans(yearset_1, dims=1, na.rm=T)
yearset1_mu
yearset2_mu= rowMeans(yearset_2, dims=1, na.rm=T)
yearset3_mu= rowMeans(yearset_3, dims=1, na.rm=T)
yearset4_mu= rowMeans(yearset_4, dims=1, na.rm=T)


Day=data[1]
#Day

#plot(yearset1_mu,type="l",ylab="Avg. Temperature")
#plot(yearset1_mu, type="o", col="green", pch="l", lty=1, ylim=c(60,110) )

### Use RED for 2nd 5 years.
#points(yearset2_mu, col="red", pch="*")
#lines(yearset2_mu, col="red",lty=1)

### Use GREEN for 3nd 5 years.
#lines(yearset3_mu, col="blue",lty=1)

### Use GREEN for 4th 5 years.
#lines(yearset4_mu, col="black",lty=1)



###add legend
legend(1,108,legend=c("y1","y2","y3","y4"), col=c("green","red","blue","black"),
                                   pch=c("o","*","+"),lty=c(1,1,1), ncol=1)

# average of all days in each year set

yearset1_overall_mu=mean(yearset1_mu)
yearset2_overall_mu=mean(yearset2_mu)
yearset3_overall_mu=mean(yearset3_mu)
yearset4_overall_mu=mean(yearset4_mu)


rbind(yearset1_overall_mu,yearset2_overall_mu,yearset3_overall_mu,yearset4_overall_mu)

#####APPROACH#2
temps_df <- read_delim('6_2temps.txt', delim = '\t') %>% 
        dplyr::select(., -DAY) %>% 
        unlist() %>% 
        as.vector() %>% 
        ts(start = 1996, frequency = 123)