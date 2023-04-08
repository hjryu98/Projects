#Time-Series analysis and forecasting HW3. 2018015350 Dept.of IE. HyungJin Ryu

##question 1
##pi_1 = 0.3, pi_2 = 0.5, draw acf and pacf of the following model, AR(2)

install.packages("forecast")
install.packages("astsa")

library(forecast)
library(astsa)

set.seed(1) ##test-case 1 for question 1.

ar1 <- arima.sim(n = 100, list(ar = c(0.3, 0.5)))
tsdisplay(ar1, main = 'Plot1 of question 1')


set.seed(2) ##test-case 2 for question 1

ar2 <- arima.sim(n = 100, list(ar = c(0.3, 0.5)))
tsdisplay(ar2, main = 'Plot2 of question 1')


set.seed(1) ##test-case 3 for question 1

ar3 <- arima.sim(n = 1000, list(ar = c(0.3, 0.5)))
tsdisplay(ar3, main = 'Plot3 of question 1')
sarima.for(ar3, 10, 1, 0, 0)


##question 1 description
##1. This time series model is stationary, because the absolute value of characteristic roots larger than 1 (B_1 = 1.145, B_2 = -1.745)
##2. This is AR(2) model, therefore when time lag is greater than or equal to time lag 3, there emerges PACF truncation.
##3. In ACF, this value decreases exponentially as the time lag increases.
##4. By applying sarima.for() function(The library "astsa" is required), we can predict the next value after the referred time series data, but we should assure that this model is adequate to apply in this forecast.
##5. And in the plot of sarima.for(), we can predict that the data will decrease slowly.


##question 2
##theta_1 = 1.2, theta_2 = -0.4, draw acf and pacf of the following model, MA(2)

set.seed(1) ##test-case 1 for question 2.

ma1 <- arima.sim(n = 100, list(ma = c(1.2, -0.4)))
tsdisplay(ma1, main = 'Plot1 of question 2')


set.seed(2) ##test-case 2 for question 2.

ma2 <- arima.sim(n = 100, list(ma = c(1.2, -0.4)))
tsdisplay(ma2, main = 'Plot2 of question 2')


set.seed(1) ##test-case 3 for question 2.

ma3 <- arima.sim(n = 1000, list(ma = c(1.2, -0.4)))
tsdisplay(ma3, main = 'Plot3 of question 2')
sarima.for(ma3, 10, 1, 0, 0)


##question 2 description
##1. This time series model not satisfies invertibility, because the characteristic roots are complex numbers, and theta_1 is greater than 1.
##2. This is MA(2) model, there emerges ACF truncation after time lag 2.
##3. In PACF, this value decreases exponentially as the time lag increases.(Accurately, decreases in sine curve.)
##4. By applying sarima.for() function(The library "astsa" is required), we can predict the next value after the referred time series data, but we should assure that this model is adequate to apply in this forecast.
##5. And in the plot of sarima.for(), we can predict that the data will decrease slowly, and this result is similar to the result of question 1.


##question 3
##phi_1 = 0.7, theta_1 = -0.2, draw acf and pacf of the following model, ARMA(1, 1)


set.seed(123) ##test-case 1 for question 3.

arma1 <- arima.sim(n = 1000, list(ar = 0.7, ma = -0.2))
tsdisplay(arma1, main = 'Plot1 of question 3')

set.seed(123)
test1 <- arima.sim(n = 1000, list(ar = 0.7))
acf(test1) ##for test

set.seed(249) ##test-case 2 for question 3.

arma1 <- arima.sim(n = 1000, list(ar = 0.7, ma = -0.2))
tsdisplay(arma1, main = 'Plot2 of question 3')


##question 3 description
##1. This model both satisfies stationarity and invertiblity because both constants, phi_1 and theta_1, their absolute value is smaller than 1.
##2. As we know, ACF and PACF of ARMA model decrease slowly, but this characteristic is clear only in ACF(In my insight!)
##3. ACF of AR(1) is almost same as the ACF of ARMA(1, 1) after time lag 2.



library(forecast)
set.seed(1)

model <- arima.sim(n=1000, list(ma=0.1))
tsdisplay(model)
acf(model)










