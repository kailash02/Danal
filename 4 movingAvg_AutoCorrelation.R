install.packages("zoo")
library(zoo)

##### Q1
# Question 1. Calculate 3 year moving average forecast for the given sales data
# year  1   2   3   4   5   6   7   8   9   10  11  12
# Sales 5.2 4.9 5.5 4.9 5.2 5.7 5.4 5.8 5.9  6  5.2 4.8
sales <- c(5.2,4.9,5.5,4.9,5.2,5.7,5.4,5.8,5.9,6,5.2,4.8)
mov_avg<-rollmean(sales,3,align="right")

################  20BRS1208   ###############
mov_avg
forecasting_err <- sales - mov_avg

######## mean absolute error
mae <- mean(abs(forecasting_err))

############ mean squared error
mse <- mean(forecasting_err^2)

################ root mean squared error
rmse <- sqrt(mse)

############# mean absolute percentage error
mape <- mean(abs(forecasting_err/sales))*100

############ Printing
mae
mse
rmse
mape


####### Q2
# Question 2. Calculate 4 year moving average for the given sales data 
# year  2003  2004  2005  2006  2007  2008  2009  2010  2011  2012
# Sales 4     6     5     8     9     5     4     3     7     8
sales <- c(4,6,5,8,9,5,4,3,7,8)
mov_avg <- rollmean(sales, 4, align = "right")
print(mov_avg)

################  20BRS1208   ###############
forecasting_err <- sales - mov_avg

######## mean absolute error
mae <- mean(abs(forecasting_err))

############ mean squared error
mse <- mean(forecasting_err^2)

################ root mean squared error
rmse <- sqrt(mse)

############# mean absolute percentage error
mape <- mean(abs(forecasting_err/sales))*100

mae
mse
rmse
mape


############ Auto correlation
# Question 3. Explain what is autocorrelation, give the implementation steps with
# the help of an example. Bring out the inference from your results obtained.
install.packages("tseries")
library(tseries)
v1<-c(799,1174.8,865.1,1334.6,635.4,918.5,685.5,998.6,784.2,985,882.8,1071)
v1.timeseries <- ts(v1,start = c(2012,1),frequency = 12)
plot(v1.timeseries)
print(acf(v1))
