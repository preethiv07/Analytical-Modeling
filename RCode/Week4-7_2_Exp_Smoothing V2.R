###Question 7.2###

###Input : 20 years of daily high temperature data for Atlanta (July through October)
####ASK: build and use an exponential smoothing model to help make a judgment of whether the unofficial end of summer has gotten later over the 20 years.
####Part of the point of this assignment is for you to think about how you might use exponential smoothing to answer this question
#### combine it with other models if you’d like to. There’s certainly more than one reasonable approach

####OPTIONS: You can use R if you’d like, OR Excel spreadsheet
####you can use either HoltWinters (simpler to use) or 
####the smooth package’s es function (harder to use, but more general).  
####In es, the Holt-Winters model uses model=”AAM” in the function call 
####(the first and second constants are used “A”dditively, and the third (seasonality) is used “M”ultiplicatively

########Approach#1: Generate a Exponential Smooth data
#####Add CUSUM model to detect if the sumer had gotten later over years

##########CLEAR##########
rm(list = ls())

##########LIBRARY##########
#install.packages("TSstudio")
library(TSstudio) #Plot time series data
library(zoo)
library("dplyr") # Summarize data



##########INGEST FILE##########

data<- read.table("6_2temps.txt",header=TRUE,stringsAsFactors = FALSE,sep="\t")

head(data,10)

##########DATA PREP FOR TIME SERIES ANALYSIS##########

TEMP<- read.table("6_2temps.txt",header=TRUE,stringsAsFactors = FALSE,sep="\t") %>% 
	dplyr::select (., -DAY) %>% # every field expect DAY
        unlist() %>% 
        as.vector()  %>%# create vector of timeseries years one after another
	        ts(start = 1996, frequency = 123)

head(TEMP,10)

#plot(TEMP) # Plot original data by years

holtsmooth = HoltWinters(TEMP)

#Coefficients for HoltWinters
holtsmooth

#print the parameters
print(paste0('Accuracy(Sum of Squared Errors): ', holtsmooth$SSE))

#As Alpha is close to 1, forecast are based on not much randomness in system and depends more on the current data point and not on baseline
print(paste0('Holt Winters smoothing(Alpha): ', holtsmooth$alpha))

print(paste0('Holt Winters Trend(Beta): ', holtsmooth$beta))
print(paste0('Holt Winters Seasonality(Gamma): ', holtsmooth$gamma))

#Plot the Holt Winters results
#plot(holtsmooth) #The plot shows the original time series in black, and the forecasts as a red line.

#predict future Temp
#predictmn <- predict(holtsmooth, n.ahead=30)
#plot(predictmn)

##########CUSUM to predict if SUMMER ended later in the last 20 years##########

# set up C value
C <- 4
#SET UP Threshold for Temp drop  set to 82.


#Extract each year
date_avg96 <- data$X1996
date_avg97 <- data$X1997
date_avg98 <- data$X1998
date_avg99 <- data$X1999
date_avg00 <- data$X2000
date_avg01 <- data$X2001
date_avg02 <- data$X2002
date_avg03 <- data$X2003
date_avg04 <- data$X2004
date_avg05 <- data$X2005
date_avg06 <- data$X2006
date_avg07 <- data$X2007
date_avg08 <- data$X2008
date_avg09 <- data$X2009
date_avg10 <- data$X2010
date_avg11 <- data$X2011
date_avg12 <- data$X2012
date_avg13 <- data$X2013
date_avg14 <- data$X2014
date_avg15 <- data$X2015



#x_t => point of observation
x_t96 <- date_avg96
x_t97 <- date_avg97
x_t98 <- date_avg98
x_t99 <- date_avg99
x_t00 <- date_avg00
x_t01 <- date_avg01
x_t02 <- date_avg02
x_t03 <- date_avg03
x_t04 <- date_avg04
x_t05 <- date_avg05
x_t06 <- date_avg06
x_t07 <- date_avg07
x_t08 <- date_avg08
x_t09 <- date_avg09
x_t10 <- date_avg10
x_t11 <- date_avg11
x_t12 <- date_avg12
x_t13 <- date_avg13
x_t14 <- date_avg14
x_t15 <- date_avg15


#calculate mean
mean_x_t96 <- mean(x_t96)
mean_x_t97 <- mean(x_t97)
mean_x_t98 <- mean(x_t98)
mean_x_t99 <- mean(x_t99)
mean_x_t00 <- mean(x_t00)
mean_x_t01 <- mean(x_t01)
mean_x_t02 <- mean(x_t02)
mean_x_t03 <- mean(x_t03)
mean_x_t04 <- mean(x_t04)
mean_x_t05 <- mean(x_t05)
mean_x_t06 <- mean(x_t06)
mean_x_t07 <- mean(x_t07)
mean_x_t08 <- mean(x_t08)
mean_x_t09 <- mean(x_t09)
mean_x_t10 <- mean(x_t10)
mean_x_t11 <- mean(x_t11)
mean_x_t12 <- mean(x_t12)
mean_x_t13 <- mean(x_t13)
mean_x_t14 <- mean(x_t14)
mean_x_t15 <- mean(x_t15)




mean_x_t96
mean_x_t97
mean_x_t98
mean_x_t99

# as we are seeing decrease in temperature, we calculate mean - data

mean_data96 <- mean_x_t96-date_avg96
mean_data97 <- mean_x_t97-date_avg97
mean_data98 <- mean_x_t98-date_avg98
mean_data99 <- mean_x_t99-date_avg99
mean_data00 <- mean_x_t00-date_avg00
mean_data01 <- mean_x_t01-date_avg01
mean_data02 <- mean_x_t02-date_avg02
mean_data03 <- mean_x_t03-date_avg03
mean_data04 <- mean_x_t04-date_avg04
mean_data05 <- mean_x_t05-date_avg05
mean_data06 <- mean_x_t06-date_avg06
mean_data07 <- mean_x_t07-date_avg07
mean_data08 <- mean_x_t08-date_avg08
mean_data09 <- mean_x_t09-date_avg09
mean_data10 <- mean_x_t10-date_avg10
mean_data11 <- mean_x_t11-date_avg11
mean_data12 <- mean_x_t12-date_avg12
mean_data13 <- mean_x_t13-date_avg13
mean_data14 <- mean_x_t14-date_avg14
mean_data15 <- mean_x_t15-date_avg15







# subtract C from the difference score
s_t96 <- mean_data96 - C
s_t97 <- mean_data97 - C
s_t98 <- mean_data98 - C
s_t99 <- mean_data99 - C
s_t00 <- mean_data00 - C
s_t01 <- mean_data01 - C
s_t02 <- mean_data02 - C
s_t03 <- mean_data03 - C
s_t04 <- mean_data04 - C
s_t05 <- mean_data05 - C
s_t06 <- mean_data06 - C
s_t07 <- mean_data07 - C
s_t08 <- mean_data08 - C
s_t09 <- mean_data09 - C
s_t10 <- mean_data10 - C
s_t11 <- mean_data11 - C
s_t12 <- mean_data12 - C
s_t13 <- mean_data13 - C
s_t14 <- mean_data14 - C
s_t15 <- mean_data15 - C



cusum96 <- append(0, 0)
cusum97 <- append(0, 0)
cusum98 <- append(0, 0)
cusum99 <- append(0, 0)
cusum00 <- append(0, 0)
cusum01 <- append(0, 0)
cusum02 <- append(0, 0)
cusum03 <- append(0, 0)
cusum04 <- append(0, 0)
cusum05 <- append(0, 0)
cusum06 <- append(0, 0)
cusum07 <- append(0, 0)
cusum08 <- append(0, 0)
cusum09 <- append(0, 0)
cusum10 <- append(0, 0)
cusum11 <- append(0, 0)
cusum12 <- append(0, 0)
cusum13 <- append(0, 0)
cusum14 <- append(0, 0)
cusum15 <- append(0, 0)



for (i in 1:length(s_t96)) 
     {
  ifelse(cusum96[i] + s_t96[i-1] > 0, cusum96[i+1] <- cusum96[i] + s_t96[i-1], cusum96[i+1] <- 0) 
  ifelse(cusum97[i] + s_t97[i-1] > 0, cusum97[i+1] <- cusum97[i] + s_t97[i-1], cusum97[i+1] <- 0) 
  ifelse(cusum98[i] + s_t98[i-1] > 0, cusum98[i+1] <- cusum98[i] + s_t98[i-1], cusum98[i+1] <- 0) 
  ifelse(cusum99[i] + s_t99[i-1] > 0, cusum99[i+1] <- cusum99[i] + s_t99[i-1], cusum99[i+1] <- 0) 
  ifelse(cusum00[i] + s_t00[i-1] > 0, cusum00[i+1] <- cusum00[i] + s_t00[i-1], cusum00[i+1] <- 0) 
  ifelse(cusum01[i] + s_t01[i-1] > 0, cusum01[i+1] <- cusum01[i] + s_t01[i-1], cusum01[i+1] <- 0) 
  ifelse(cusum02[i] + s_t02[i-1] > 0, cusum02[i+1] <- cusum02[i] + s_t02[i-1], cusum02[i+1] <- 0) 
  ifelse(cusum03[i] + s_t03[i-1] > 0, cusum03[i+1] <- cusum03[i] + s_t03[i-1], cusum03[i+1] <- 0) 
  ifelse(cusum04[i] + s_t04[i-1] > 0, cusum04[i+1] <- cusum04[i] + s_t04[i-1], cusum04[i+1] <- 0) 
  ifelse(cusum05[i] + s_t05[i-1] > 0, cusum05[i+1] <- cusum05[i] + s_t05[i-1], cusum05[i+1] <- 0) 
  ifelse(cusum06[i] + s_t06[i-1] > 0, cusum06[i+1] <- cusum06[i] + s_t06[i-1], cusum06[i+1] <- 0) 
  ifelse(cusum07[i] + s_t07[i-1] > 0, cusum07[i+1] <- cusum07[i] + s_t07[i-1], cusum07[i+1] <- 0) 
  ifelse(cusum08[i] + s_t08[i-1] > 0, cusum08[i+1] <- cusum08[i] + s_t08[i-1], cusum08[i+1] <- 0) 
  ifelse(cusum09[i] + s_t09[i-1] > 0, cusum09[i+1] <- cusum09[i] + s_t09[i-1], cusum09[i+1] <- 0) 
  ifelse(cusum10[i] + s_t10[i-1] > 0, cusum10[i+1] <- cusum10[i] + s_t10[i-1], cusum10[i+1] <- 0) 
  ifelse(cusum11[i] + s_t11[i-1] > 0, cusum11[i+1] <- cusum11[i] + s_t11[i-1], cusum11[i+1] <- 0) 
  ifelse(cusum12[i] + s_t12[i-1] > 0, cusum12[i+1] <- cusum12[i] + s_t12[i-1], cusum12[i+1] <- 0) 
  ifelse(cusum13[i] + s_t13[i-1] > 0, cusum13[i+1] <- cusum13[i] + s_t13[i-1], cusum13[i+1] <- 0) 
  ifelse(cusum14[i] + s_t14[i-1] > 0, cusum14[i+1] <- cusum14[i] + s_t14[i-1], cusum14[i+1] <- 0) 
  ifelse(cusum15[i] + s_t15[i-1] > 0, cusum15[i+1] <- cusum15[i] + s_t15[i-1], cusum15[i+1] <- 0) 
}

which(cusum96 >= 82)
which(cusum97 >= 82)
which(cusum98 >= 82)
which(cusum99 >= 82)
which(cusum00 >= 82)
which(cusum01 >= 82)
which(cusum02 >= 82)
which(cusum03 >= 82)
which(cusum04 >= 82)
which(cusum05 >= 82)
which(cusum06 >= 82)
which(cusum07 >= 82)
which(cusum08 >= 82)
which(cusum09 >= 82)
which(cusum10 >= 82)
which(cusum11 >= 82)
which(cusum12 >= 82)
which(cusum13 >= 82)
which(cusum14 >= 82)
which(cusum15 >= 82)
#cusum14

data96 <- data$X1996
print(paste0('1996 summer end date: ',data[100,1]))
print(paste0('1997 summer end date: ',data[112,1]))
print(paste0('1998 summer end date: ',data[116,1]))
print(paste0('1999 summer end date: ',data[104,1]))
print(paste0('2000 summer end date: ',data[102,1]))
print(paste0('2001 summer end date: ',data[110,1]))
print(paste0('2002 summer end date: ',data[110,1]))
print(paste0('2003 summer end date: ',data[109,1]))
print(paste0('2004 summer end date: ',data[111,1]))
print(paste0('2005 summer end date: ',data[118,1]))
print(paste0('2006 summer end date: ',data[111,1]))
print(paste0('2007 summer end date: ',data[118,1]))
print(paste0('2008 summer end date: ',data[117,1]))
print(paste0('2009 summer end date: ',data[110,1]))
print(paste0('2010 summer end date: ',data[99,1]))
print(paste0('2011 summer end date: ',data[105,1]))
print(paste0('2012 summer end date: ',data[107,1]))
print(paste0('2013 summer end date: ',data[118,1]))
print(paste0('2014 summer end date: ',data[123,1]))
print(paste0('2015 summer end date: ',data[98,1]))

#plot(data[100,1],type="o")###Question 7.2###

###Input : 20 years of daily high temperature data for Atlanta (July through October)
####ASK: build and use an exponential smoothing model to help make a judgment of whether the unofficial end of summer has gotten later over the 20 years.
####Part of the point of this assignment is for you to think about how you might use exponential smoothing to answer this question
#### combine it with other models if you’d like to. There’s certainly more than one reasonable approach

####OPTIONS: You can use R if you’d like, OR Excel spreadsheet
####you can use either HoltWinters (simpler to use) or 
####the smooth package’s es function (harder to use, but more general).  
####In es, the Holt-Winters model uses model=”AAM” in the function call 
####(the first and second constants are used “A”dditively, and the third (seasonality) is used “M”ultiplicatively

########Approach#1: Generate a Exponential Smooth data
#####Add CUSUM model to detect if the sumer had gotten later over years

##########CLEAR##########
rm(list = ls())

##########LIBRARY##########
#install.packages("TSstudio")
library(TSstudio) #Plot time series data
library(zoo)
library("dplyr") # Summarize data



##########INGEST FILE##########

data<- read.table("6_2temps.txt",header=TRUE,stringsAsFactors = FALSE,sep="\t")

head(data,10)

##########DATA PREP FOR TIME SERIES ANALYSIS##########

TEMP<- read.table("6_2temps.txt",header=TRUE,stringsAsFactors = FALSE,sep="\t") %>% 
	dplyr::select (., -DAY) %>% # every field expect DAY
        unlist() %>% 
        as.vector()  %>%# create vector of timeseries years one after another
	        ts(start = 1996, frequency = 123)

head(TEMP,10)

#plot(TEMP) # Plot original data by years

holtsmooth = HoltWinters(TEMP)

#Coefficients for HoltWinters
holtsmooth

#print the parameters
print(paste0('Accuracy(Sum of Squared Errors): ', holtsmooth$SSE))

#As Alpha is close to 1, forecast are based on not much randomness in system and depends more on the current data point and not on baseline
print(paste0('Holt Winters smoothing(Alpha): ', holtsmooth$alpha))

print(paste0('Holt Winters Trend(Beta): ', holtsmooth$beta))
print(paste0('Holt Winters Seasonality(Gamma): ', holtsmooth$gamma))

#Plot the Holt Winters results
#plot(holtsmooth) #The plot shows the original time series in black, and the forecasts as a red line.

#predict future Temp
#predictmn <- predict(holtsmooth, n.ahead=30)
#plot(predictmn)

##########CUSUM to predict if SUMMER ended later in the last 20 years##########

# set up C value
C <- 4
#SET UP Threshold for Temp drop  set to 82.


Extract each year
date_avg96 <- data$X1996
date_avg97 <- data$X1997
date_avg98 <- data$X1998
date_avg99 <- data$X1999
date_avg00 <- data$X2000
date_avg01 <- data$X2001
date_avg02 <- data$X2002
date_avg03 <- data$X2003
date_avg04 <- data$X2004
date_avg05 <- data$X2005
date_avg06 <- data$X2006
date_avg07 <- data$X2007
date_avg08 <- data$X2008
date_avg09 <- data$X2009
date_avg10 <- data$X2010
date_avg11 <- data$X2011
date_avg12 <- data$X2012
date_avg13 <- data$X2013
date_avg14 <- data$X2014
date_avg15 <- data$X2015



#x_t => point of observation
x_t96 <- date_avg96
x_t97 <- date_avg97
x_t98 <- date_avg98
x_t99 <- date_avg99
x_t00 <- date_avg00
x_t01 <- date_avg01
x_t02 <- date_avg02
x_t03 <- date_avg03
x_t04 <- date_avg04
x_t05 <- date_avg05
x_t06 <- date_avg06
x_t07 <- date_avg07
x_t08 <- date_avg08
x_t09 <- date_avg09
x_t10 <- date_avg10
x_t11 <- date_avg11
x_t12 <- date_avg12
x_t13 <- date_avg13
x_t14 <- date_avg14
x_t15 <- date_avg15


#calculate mean
mean_x_t96 <- mean(x_t96)
mean_x_t97 <- mean(x_t97)
mean_x_t98 <- mean(x_t98)
mean_x_t99 <- mean(x_t99)
mean_x_t00 <- mean(x_t00)
mean_x_t01 <- mean(x_t01)
mean_x_t02 <- mean(x_t02)
mean_x_t03 <- mean(x_t03)
mean_x_t04 <- mean(x_t04)
mean_x_t05 <- mean(x_t05)
mean_x_t06 <- mean(x_t06)
mean_x_t07 <- mean(x_t07)
mean_x_t08 <- mean(x_t08)
mean_x_t09 <- mean(x_t09)
mean_x_t10 <- mean(x_t10)
mean_x_t11 <- mean(x_t11)
mean_x_t12 <- mean(x_t12)
mean_x_t13 <- mean(x_t13)
mean_x_t14 <- mean(x_t14)
mean_x_t15 <- mean(x_t15)




mean_x_t96
mean_x_t97
mean_x_t98
mean_x_t99

# as we are seeing decrease in temperature, we calculate mean - data

mean_data96 <- mean_x_t96-date_avg96
mean_data97 <- mean_x_t97-date_avg97
mean_data98 <- mean_x_t98-date_avg98
mean_data99 <- mean_x_t99-date_avg99
mean_data00 <- mean_x_t00-date_avg00
mean_data01 <- mean_x_t01-date_avg01
mean_data02 <- mean_x_t02-date_avg02
mean_data03 <- mean_x_t03-date_avg03
mean_data04 <- mean_x_t04-date_avg04
mean_data05 <- mean_x_t05-date_avg05
mean_data06 <- mean_x_t06-date_avg06
mean_data07 <- mean_x_t07-date_avg07
mean_data08 <- mean_x_t08-date_avg08
mean_data09 <- mean_x_t09-date_avg09
mean_data10 <- mean_x_t10-date_avg10
mean_data11 <- mean_x_t11-date_avg11
mean_data12 <- mean_x_t12-date_avg12
mean_data13 <- mean_x_t13-date_avg13
mean_data14 <- mean_x_t14-date_avg14
mean_data15 <- mean_x_t15-date_avg15







# subtract C from the difference score
s_t96 <- mean_data96 - C
s_t97 <- mean_data97 - C
s_t98 <- mean_data98 - C
s_t99 <- mean_data99 - C
s_t00 <- mean_data00 - C
s_t01 <- mean_data01 - C
s_t02 <- mean_data02 - C
s_t03 <- mean_data03 - C
s_t04 <- mean_data04 - C
s_t05 <- mean_data05 - C
s_t06 <- mean_data06 - C
s_t07 <- mean_data07 - C
s_t08 <- mean_data08 - C
s_t09 <- mean_data09 - C
s_t10 <- mean_data10 - C
s_t11 <- mean_data11 - C
s_t12 <- mean_data12 - C
s_t13 <- mean_data13 - C
s_t14 <- mean_data14 - C
s_t15 <- mean_data15 - C



cusum96 <- append(0, 0)
cusum97 <- append(0, 0)
cusum98 <- append(0, 0)
cusum99 <- append(0, 0)
cusum00 <- append(0, 0)
cusum01 <- append(0, 0)
cusum02 <- append(0, 0)
cusum03 <- append(0, 0)
cusum04 <- append(0, 0)
cusum05 <- append(0, 0)
cusum06 <- append(0, 0)
cusum07 <- append(0, 0)
cusum08 <- append(0, 0)
cusum09 <- append(0, 0)
cusum10 <- append(0, 0)
cusum11 <- append(0, 0)
cusum12 <- append(0, 0)
cusum13 <- append(0, 0)
cusum14 <- append(0, 0)
cusum15 <- append(0, 0)



for (i in 1:length(s_t96)) 
     {
  ifelse(cusum96[i] + s_t96[i-1] > 0, cusum96[i+1] <- cusum96[i] + s_t96[i-1], cusum96[i+1] <- 0) 
  ifelse(cusum97[i] + s_t97[i-1] > 0, cusum97[i+1] <- cusum97[i] + s_t97[i-1], cusum97[i+1] <- 0) 
  ifelse(cusum98[i] + s_t98[i-1] > 0, cusum98[i+1] <- cusum98[i] + s_t98[i-1], cusum98[i+1] <- 0) 
  ifelse(cusum99[i] + s_t99[i-1] > 0, cusum99[i+1] <- cusum99[i] + s_t99[i-1], cusum99[i+1] <- 0) 
  ifelse(cusum00[i] + s_t00[i-1] > 0, cusum00[i+1] <- cusum00[i] + s_t00[i-1], cusum00[i+1] <- 0) 
  ifelse(cusum01[i] + s_t01[i-1] > 0, cusum01[i+1] <- cusum01[i] + s_t01[i-1], cusum01[i+1] <- 0) 
  ifelse(cusum02[i] + s_t02[i-1] > 0, cusum02[i+1] <- cusum02[i] + s_t02[i-1], cusum02[i+1] <- 0) 
  ifelse(cusum03[i] + s_t03[i-1] > 0, cusum03[i+1] <- cusum03[i] + s_t03[i-1], cusum03[i+1] <- 0) 
  ifelse(cusum04[i] + s_t04[i-1] > 0, cusum04[i+1] <- cusum04[i] + s_t04[i-1], cusum04[i+1] <- 0) 
  ifelse(cusum05[i] + s_t05[i-1] > 0, cusum05[i+1] <- cusum05[i] + s_t05[i-1], cusum05[i+1] <- 0) 
  ifelse(cusum06[i] + s_t06[i-1] > 0, cusum06[i+1] <- cusum06[i] + s_t06[i-1], cusum06[i+1] <- 0) 
  ifelse(cusum07[i] + s_t07[i-1] > 0, cusum07[i+1] <- cusum07[i] + s_t07[i-1], cusum07[i+1] <- 0) 
  ifelse(cusum08[i] + s_t08[i-1] > 0, cusum08[i+1] <- cusum08[i] + s_t08[i-1], cusum08[i+1] <- 0) 
  ifelse(cusum09[i] + s_t09[i-1] > 0, cusum09[i+1] <- cusum09[i] + s_t09[i-1], cusum09[i+1] <- 0) 
  ifelse(cusum10[i] + s_t10[i-1] > 0, cusum10[i+1] <- cusum10[i] + s_t10[i-1], cusum10[i+1] <- 0) 
  ifelse(cusum11[i] + s_t11[i-1] > 0, cusum11[i+1] <- cusum11[i] + s_t11[i-1], cusum11[i+1] <- 0) 
  ifelse(cusum12[i] + s_t12[i-1] > 0, cusum12[i+1] <- cusum12[i] + s_t12[i-1], cusum12[i+1] <- 0) 
  ifelse(cusum13[i] + s_t13[i-1] > 0, cusum13[i+1] <- cusum13[i] + s_t13[i-1], cusum13[i+1] <- 0) 
  ifelse(cusum14[i] + s_t14[i-1] > 0, cusum14[i+1] <- cusum14[i] + s_t14[i-1], cusum14[i+1] <- 0) 
  ifelse(cusum15[i] + s_t15[i-1] > 0, cusum15[i+1] <- cusum15[i] + s_t15[i-1], cusum15[i+1] <- 0) 
}

which(cusum96 >= 82)
which(cusum97 >= 82)
which(cusum98 >= 82)
which(cusum99 >= 82)
which(cusum00 >= 82)
which(cusum01 >= 82)
which(cusum02 >= 82)
which(cusum03 >= 82)
which(cusum04 >= 82)
which(cusum05 >= 82)
which(cusum06 >= 82)
which(cusum07 >= 82)
which(cusum08 >= 82)
which(cusum09 >= 82)
which(cusum10 >= 82)
which(cusum11 >= 82)
which(cusum12 >= 82)
which(cusum13 >= 82)
which(cusum14 >= 82)
which(cusum15 >= 82)
#cusum14

data96 <- data$X1996
print(paste0('1996 summer end date: ',data[100,1]))
print(paste0('1997 summer end date: ',data[112,1]))
print(paste0('1998 summer end date: ',data[116,1]))
print(paste0('1999 summer end date: ',data[104,1]))
print(paste0('2000 summer end date: ',data[102,1]))
print(paste0('2001 summer end date: ',data[110,1]))
print(paste0('2002 summer end date: ',data[110,1]))
print(paste0('2003 summer end date: ',data[109,1]))
print(paste0('2004 summer end date: ',data[111,1]))
print(paste0('2005 summer end date: ',data[118,1]))
print(paste0('2006 summer end date: ',data[111,1]))
print(paste0('2007 summer end date: ',data[118,1]))
print(paste0('2008 summer end date: ',data[117,1]))
print(paste0('2009 summer end date: ',data[110,1]))
print(paste0('2010 summer end date: ',data[99,1]))
print(paste0('2011 summer end date: ',data[105,1]))
print(paste0('2012 summer end date: ',data[107,1]))
print(paste0('2013 summer end date: ',data[118,1]))
print(paste0('2014 summer end date: ',data[123,1]))
print(paste0('2015 summer end date: ',data[98,1]))

cbind(data[100,1],data[112,1],data[116,1],data[104,1],data[102,1],data[110,1],data[110,1],data[109,1],
data[111,1],data[118,1],data[111,1],data[118,1],data[117,1],data[110,1],data[99,1],data[105,1]
,data[107,1],data[118,1],data[123,1],data[98,1])