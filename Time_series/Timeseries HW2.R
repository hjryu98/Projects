#Time-Series Analysis HW2. student ID: 2018015350

###1.Plot the random walk and ACF of random walk

set.seed(1)
w <- rnorm(1000)
x <- w

for(t in 2:1000)
  x[t] <- x[t - 1] + w[t]

plot(x, type = 'l') #plot random walk

acf(x) #plot ACF of random walk




###2.Plot the first difference of random walk and ACF of the first difference of random walk

plot(diff(x), type = 'l') #plot the first difference of random walk

acf(diff(x)) #plot ACF of the first difference of random walk




#############Suppose a time series Z_t, with Z_t = 0.7Z_(t-1) + a_t, Z_0 = 1




###3.plot the time series Z_t

set.seed(1)
z <- NULL
z[1] <- 1

a <- rnorm(1000)

for(t in 2:1000)
  z[t] <- 0.7 * z[t - 1] + a[t]

plot(z, type = 'l', main = 'plot of time series z')




###4. plot the ACF of time series Z_t

acf(z, main = "ACF of time series z")






install.packages("forecast")
library(forecast)

ar1 <- arima.sim(n=1000, list(ar = 0.7))
tsdisplay(ar1)
acf(ar1)






################testcase###############
set.seed(1)
testcase <- arima.sim(model = list(ar = 0.7), n = 1000)
plot(testcase, type='l')
acf(testcase)
pacf(testcase)
