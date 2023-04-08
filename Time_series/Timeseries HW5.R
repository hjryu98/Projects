#Time-series Forecasting and Analysis HW5. 2018015350 HyungJin Ryu
##forecasting of the GDP(Gross Domestic Product) of the USA, and these data exist from 1960 to 2020. 

install.packages("dplyr")
install.packages("lmtest")
install.packages("forecast")
library(dplyr)
library(lmtest)
library(forecast)

data <- read.csv("C:/Users/nha98/Documents/practiceR/gdp_1960_2020.csv", header = T)
head(data)

#1. plot the time series data and select an appropriate transformation, then calculate and observe the SACF and SPACF of the data to check the degree of difference required.
df <- subset(data, country == 'the United States') ##make the subset(gdp data of USA)
df
gdp_data <- ts(df['gdp']) ##time series data of gdp of USA
nf <- layout(matrix(c(1,2,3), ncol=1), widths=c(8,8,8), heights=c(3,3,3))
plot.ts(gdp_data) 
plot.ts(log(gdp_data))
plot.ts(diff(log(gdp_data)))


nf2 <- layout(matrix(c(1,2,3), ncol=1), widths=c(8,8,8), heights=c(3,3,3))
plot.ts(diff(log(gdp_data)))
acf(diff(log(gdp_data)))
pacf(diff(log(gdp_data)))

fit1 <- arima(diff(log(gdp_data)), order = c(1,1,1)) ##ARIMA(1,1,1)
fit2 <- arima(diff(log(gdp_data)), order = c(1,1,0)) ##ARIMA(1,1,0)
fit3 <- arima(diff(log(gdp_data)), order = c(2,1,0)) ##ARIMA(2,1,0)
fit4 <- arima(diff(log(gdp_data)), order = c(3,1,0)) ##ARIMA(3,1,0)
fit1
fit2
fit3
fit4
##ARIMA(1,1,1) model has the lowest AIC and the highest likelihood

#2.check that selected model is appropriate

selectfit <- arima(log(gdp_data), order = c(1,1,1))
tsdiag(selectfit, gof.lag=50)
head(resid(selectfit))
Box.test(resid(selectfit), type="Ljung", lag=10)

##ARIMA(1,1,1) model is suitable

#3. Forecast
fc <- forecast(selectfit, level = 0.95, h = 20)
fc$mean <- exp(fc$mean)
fc$upper <- exp(fc$upper)
fc$lower <- exp(fc$lower)
fc$x <- exp(fc$x)
summary(fc)
plot(fc, xlab='time', ylab='GDP', main='Forecasts of GDP of USA')

