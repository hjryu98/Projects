#Time-series Forecasting and Analysis HW6. 2018015350 HyungJin Ryu
##Forecasts for Overall Ice Cream Sales in the United States.

install.packages("dplyr")
install.packages("lmtest")
install.packages("forecast")
library(dplyr)
library(lmtest)
library(forecast)

data <- read.csv("C:/Users/nha98/Documents/practiceR/IceaCreamFrozenDessert.csv", header = T)
head(data)

#1. plot the time series data and select an appropriate transformation, then calculate and observe the SACF and SPACF of the data to check the degree of difference required.

volume <- ts(data['value']) ##Ice Cream Consumption in the United States.
nf <- layout(matrix(c(1,2,3), ncol=1), widths=c(8,8,8), heights=c(3,3,3))
plot.ts(volume) 
plot.ts(log(volume))
plot.ts(diff(log(volume)))
plot.ts(diff(diff(log(volume)), 12))

nf2 <- layout(matrix(c(1,2,3), ncol=1), widths=c(8,8,8), heights=c(3,3,3))
plot.ts(diff(diff(log(volume)), 12))
acf(diff(diff(log(volume)), 12))
pacf(diff(diff(log(volume)), 12))


acf(diff(diff(log(volume)), 12), lag.max = 40)
pacf(diff(diff(log(volume)), 12), lag.max = 40)


fit1 <- arima(log(volume), order = c(2,1,0), seasonal = list(order = c(0,1,1), periods = 12)) ##ARIMA(1,1,1)
fit2 <- arima(log(volume), order = c(1,1,0), seasonal = list(order = c(0,1,1), periods = 12)) ##ARIMA(1,1,1)
fit3 <- arima(log(volume), order = c(2,1,0), seasonal = list(order = c(0,1,2), periods = 12)) ##ARIMA(1,1,1)
fit4 <- arima(log(volume), order = c(1,1,0), seasonal = list(order = c(0,1,2), periods = 12)) ##ARIMA(1,1,1)
fit1
fit2
fit3 ##selected
fit4
##ARIMA(1,1,1) model has the lowest AIC and the highest likelihood

#2.check that selected model is appropriate

selectfit <- arima(log(volume), order = c(2,1,0), seasonal = list(order = c(0,1,1), periods = 12))
tsdiag(selectfit, gof.lag=10)
head(resid(selectfit))
Box.test(resid(selectfit), type="Ljung", lag=10)

##ARIMA(1,1,1) model is suitable

#3. Forecast
fc <- forecast(selectfit, level = 0.95, h = 120)
fc$mean <- exp(fc$mean)
fc$upper <- exp(fc$upper)
fc$lower <- exp(fc$lower)
fc$x <- exp(fc$x)
summary(fc)
plot(fc, xlab='time', ylab='sales', main='Forecasts for Overall Ice Cream Sales in the United States')



