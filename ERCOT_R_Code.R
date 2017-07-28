#Importing the data set
data=read.csv("C:/Users/User/Desktop/UIC/Spring 2017/IDS 476- Business Forecasting using Time Series/ERCOT_Hourly_Load_Data.csv")
attach(data)

#Checking for missing values
sum(is.na(data))

#Summarizing the data
summary(data)

#Loading required libraries
library(vars)
library(astsa)
library(forecast)
library(MTS)
library(TTR)
library(tseries)

#Function to find correlation
correlationTable = function(dataset) {
  cross = matrix(nrow = length(dataset), ncol = length(dataset))
  for(column1Id in 1:length(dataset)){
    column1 = dataset[[column1Id]]
    print(column1Id)
    for(column2Id in 1:length(dataset)) {
      column2 = dataset[[column2Id]]
      if(column1Id == column2Id){
        break;
      } else {
        correlation = ccf(column1, column2, lag.max = 0)
        cross[column1Id, column2Id] = correlation$acf[1]
      }
    }
  }
  cross
}

#Removing the timestamp for calculating the correlation
data1=data[-c(1)]

#Printing the correlation table
corr = correlationTable(data1)
corr

#Function to find correlated time series
findCorrelated = function(orig, highCorr){
  match = highCorr[highCorr[,1] == orig | highCorr[,2] == orig,]
  match = as.vector(match)
  match[match != orig]
}

#Setting a threshold of 0.70 and finding other time series correlated to a particular series
highCorr = which(corr > 0.70 , arr.ind = TRUE)
match = findCorrelated(3, highCorr)
match 

#Plotting similar time series
bound = function(graphs, orign, match) {
  graphOrign = graphs[[orign]]
  graphMatch = graphs[match]
  allValue = c(graphOrign)
  for(m in graphMatch){
    allValue = c(allValue, m)
  }
  c(min(allValue), max(allValue))
}

plotSimilar = function(graphs, orign, match){
  lim = bound(graphs, orign, match)
  
  graphOrign = graphs[[orign]]
  plot(ts(graphOrign), ylim=lim, xlim=c(1,length(graphOrign)+25), lwd=3)
  title(paste("Similar to", orign, "(black bold)"))
  
  cols = c()
  names = c()
  for(i in 1:length(match)) {
    m = match[[i]]
    matchGraph = graphs[[m]]
    lines(x = 1:length(matchGraph), y=matchGraph, col=i)
    
    cols = c(cols, i)
    names = c(names, paste0(m))
  }
  legend("topright", names, col = cols, lty=c(1,1))
}

plotSimilar(data1, 9, match)

#Plotting time series of the sum of demand of all regions
plot(as.ts(data1$ERCOT))

#Transforming the time series into log and square root
log_demand=log(data1$ERCOT)
plot(as.ts(log_demand))

sqrt_demand=sqrt(data1$ERCOT)
plot(as.ts(sqrt_demand))

#Decomposing the time series into trend, seasonality and randomness (noise)
#with yearly periods (24*365=8760)

#Used additive model as the seasonal variations were constant in size
ts_demand = ts(data1$ERCOT, frequency = 8760, start=c(as.Date("01-01-2015  01:00:00")))

decompose_demand = decompose(ts_demand, "additive")

plot(as.ts(decompose_demand$seasonal))
plot(as.ts(decompose_demand$trend))
plot(as.ts(decompose_demand$random))
plot(decompose_demand)

#Removing the seasonality and plotting the time series
ts_demand_adj=ts_demand/decompose_demand$seasonal
plot(ts_demand_adj)

#Plotting ACF
acf(data1$ERCOT, lag.max = NULL, type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)
#Inference-Sinusoidal, check PACF plot

#Plotting PACF
pacf(data1$ERCOT, lag.max = NULL, plot = TRUE, na.action = na.fail)
#Inference-The order of the time series is AR(2)

#Test to check if the time series is stationary
#The Augmented Dickey-Fuller (ADF) test is a formal statistical test for stationarity#
adf.test(ts_demand, alternative = "stationary")

###########Prepare time series for forecasting#########
###We partition the time series into a training set for 
###forecasting and a test set to evaluate accuracy

head(ts_demand)

trainSetStart= c(2010,01) #training set start location in time series (typically the first entry)
trainSetEnd= c(2014,8760) #training set end location in time series (typically covers 70% of time series)
testSetStart= c(2015,01) #test set start location in time series (typically location of entry after training set ends)
testSetEnd= c(2016,8760) #test set end location in time series (typically end of time series)
trainSetStart
demandTrain <- window(ts_demand,start=trainSetStart,end=trainSetEnd) #extract training set
demandTest <- window(ts_demand,start=testSetStart,end=testSetEnd) #extract test set

demandTrain1 <- window(ts_demand,start=c(2010,1),end=c(2014,8760)) #extract training set
demandTest1 <- window(ts_demand,start=c(2015,1),end=c(2016,8760)) #extract test set

#Fitting the AutoARIMA model
AutoARIMA <- auto.arima(demandTrain) #Unlike the holt winters function hw, auto.arima finds the best ARIMA model but does not forecast
#ARIMA(5,1,2)

#Predicting ARIMA(5,1,2) coefficients
arima(x = ts_demand, order = c(5, 1, 2))

fit_AutoARIMA <- forecast(AutoARIMA,h=24) #We are using the best ARIMA model in the previous step to forecast here

plot(fit_AutoARIMA)

#Plotting the forecast for the test data
plot(fit_AutoARIMA, main="Plot of training demand, 
     testing demand, and forecast with 80% and 95% 
     prediction intervals",xlab="Year", 
     ylab="Demand")

#Forecasting for the next 12 periods
sarima.for(demandTest, 24, 5, 1, 2)