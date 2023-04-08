#Time-Series analysis and forecasting HW4. 2018015350 Dept.of IE. HyungJin Ryu
#(1 - B)^2(1 - B^12)Z_t = (1 -0.4B)a_t

install.packages("forecast")
library(forecast)

set.seed(1)

mod <- arima.sim(n=100, list(ma=c(-0.8,0.6)))
tsdisplay(mod)


##question 1
##plotting Z_t >>(Zt - Z(t-12))(1-2B+B^2) >> 

set.seed(1)

x1_1 <- NULL
x1_1 <- rnorm(1015)
w1_1 <- x1_1

for(i in 15:1015)
  w1_1[i] <- 2 * w1_1[i - 1] - w1_1[i - 2] + (w1_1[i - 12] - 2 * w1_1[i - 13] + w1_1[i - 14]) + x1_1[i] - 0.4 * x1_1[i - 1]

tsdisplay(w1_1, main = 'plot of the question 1, case 1')
tsdisplay(diff(w1_1), main = 'first-order differencing plot of w1_1' )
tsdisplay(diff(diff(w1_1)), main = 'second-order differencing plot of w1_1' )


set.seed(2)

x1_2 <- NULL
x1_2 <- rnorm(1015)
w1_2 <- x1_2

for(i in 15:1015)
  w1_2[i] <- 2 * w1_2[i - 1] - w1_2[i - 2] + (w1_2[i - 12] - 2 * w1_2[i - 13] + w1_2[i - 14]) + x1_2[i] - 0.4 * x1_2[i - 1]

tsdisplay(w1_2, main = 'plot of the question 1, case 2')
tsdisplay(diff(w1_2), main = 'first-order differencing plot of w1_2' )
tsdisplay(diff(diff(w1_2)), main = 'second-order differencing plot of w1_2' )


##explanation of question 1
##1. This model is same as (Z_t - 2 * Z_(t - 1) + Z_(t - 2)) - (Z_(t - 12) - 2 * Z_(t - 13) + Z_(t - 14)) = a_t - 0.4 * a_(t - 1), and this model can be plotted like above.

##2. Generally, non-stationary time-series model's ACF is slowly decreased. Also, PACF is truncated after time lag 1,
##   therefore this model is non-stationary model.

##3. By differencing two time, we can change this model to stationary model, and we can know the degree of d from 
##   (1 - B)^2, and we can identify that the ACF of this second-order differeciated model has a curve shape(similar 
##   to sine curve)

##4. In ACF of this model, when the arbitrary time lag is divided to 12 with no remainder, the ACF in this time lag
##   is almost 1.

##5. In PACF of this model, when the arbitrary time lag is 12, the PACF in this time lag is almost 1.





##question 2
##plotting W_t = (1 - B)Z_t

set.seed(91)

x2_1 <- NULL
x2_1 <- rnorm(1014)
w2_1 <- x2_1

for(i in 14:1014)
  w2_1[i] <- w2_1[i - 1] + w2_1[i - 12] - w2_1[i - 13] + x2_1[i] - 0.4 * x2_1[i - 1]

tsdisplay(w2_1, main = 'plot of the question 2, case 1')
tsdisplay(diff(w2_1), main = 'first-order differencing plot of w2_1')


set.seed(92)

x2_2 <- NULL
x2_2 <- rnorm(1014)
w2_2 <- x2_2

for(i in 14:1014)
  w2_2[i] <- w2_2[i - 1] + w2_2[i - 12] - w2_2[i - 13] + x2_2[i] - 0.4 * x2_2[i - 1]

tsdisplay(w2_2, main = 'plot of the question 2, case 2')
tsdisplay(diff(w2_2), main = 'first-order differencing plot of w2_2')




##explanation of question 2
##1. This model is same as (W_t - W_(t - 12)) - (W_(t - 1) - W_(t - 13)) = a_t - 0.4 * a_(t - 1), and this model can    be plotted like above.

##2. Generally, non-stationary time-series model's ACF is slowly decreased. Also, in this model's plot, there is linear trend, and we can say that this model is non-stationary.

##3. By differencing one time, we can change this model to stationary model, and we can identify that the ACF of this first-order differeciated model is highly decreased(than ACF of the basic plot.).

##4. In ACF of this model, when the arbitrary time lag is divided to 12 with no remainder, the ACF in this time lag
##   is almost 1.

##5. In PACF of this model, when the arbitrary time lag is 12, the PACF in this time lag is almost 1.








##question 3
##plotting W_t = Z_t - 2Z_(t - 1) + Z_(t - 2)

set.seed(101)

x3_1 <- NULL
x3_1 <- rnorm(1013)
w3_1 <- x3_1

for(i in 13:1013)
  w3_1[i] <- w3_1[i - 12] + x3_1[i] - 0.4 * x3_1[i - 1]

tsdisplay(w3_1, main = 'plot of the question 3, case 1')


set.seed(102)

x3_2 <- NULL
x3_2 <- rnorm(1013)
w3_2 <- x3_2

for(i in 13:1013)
  w3_2[i] <- w3_2[i - 12] + x3_2[i] - 0.4 * x3_2[i - 1]

tsdisplay(w3_2, main = 'plot of the question 3, case 2')



##explanation of question 3
##1. This model is same as W_t - W_(t - 12) = a_t - 0.4 * a_(t - 1), and this model can be plotted like above.

##2. Generally, non-stationary time-series model's ACF is slowly decreased. However, this model's ACF value is highly decreased.

##3. In ACF of this model, when the arbitrary time lag is divided to 12 with no remainder, the ACF in this time lag
##   is almost 1.

##4. In PACF of this model, when the arbitrary time lag is 12, the PACF in this time lag is almost 1.









##question 4
##plotting W_t = Z_t - Z_(t-12)

set.seed(111) ##first case of question 4

x4_1 <- NULL
x4_1 <- rnorm(1000)
for (i in 2:1001)
  w4_1[i] <- 2 * w4_1[i - 1] - w4_1[i - 2] + x4_1[i] - 0.4 * x4_1[i - 1]

tsdisplay(w4_1, main ='IMA(2, 1) plot') ##IMA(2, 1) plot
tsdisplay(diff(w4_1), main ='first-order differencing plot of w4_1') ##first-order differencing plot
tsdisplay(diff(diff(w4_1)), main = 'second-order differencing plot of w4_1') ##second-order differencing plot



set.seed(112) ##first case of question 4

x4_2 <- NULL
x4_2 <- rnorm(1000)
for (i in 2:1001)
  w4_2[i] <- 2 * w4_2[i - 1] - w4_2[i - 2] + x4_2[i] - 0.4 * x4_2[i - 1]

tsdisplay(w4_2, main ='IMA(2, 1) plot') ##IMA(2, 1) plot
tsdisplay(diff(w4_2), main ='first-order differencing plot of w4_2') ##first-order differencing plot
tsdisplay(diff(diff(w4_2)), main = 'second-order differencing plot of w4_2') ##second-order differencing plot


##explanation of question 4
##1. This model is IMA(2, 1) model, and d is 2, and this value is larger than 0, therefore this model is non-stationary

##2. However, by differencing, we can make this model as stationary model. d = 2, therefore by differencing 2 times,
##   this model became stationary.

##3. By differencing, this model changes to as MA(1) model, and this model is stationary.










##question 5
##plotting W_t = (1 - B)^2(1 - B^12)Z_t

set.seed(123)

w5_1 <- arima.sim(n = 1000, list(ma = -0.4)) ##first case of question 5
tsdisplay(w5_1, main = 'plot of the question 5, case 1')

set.seed(124)

w5_2 <- arima.sim(n = 1000, list(ma = -0.4)) ##second case of question 5
tsdisplay(w5_2, main = 'plot of the question 5, case 2')

##explanation of question 5
##1. This model is same as MA(1) model, and MA(q) model is always stationary. Therefore, arima.sim() function can be applied 
##   without the identification whether this model is stationary.('for' loop is not necessary.)

##2. To identify that this model is invertible, we should calculate the characteristic root of this model, and it is 
##   B = 1/0.4. This value is larger than 1, therefore this model establishs invertibility.

##3. When the time lag is larger than 1, ACF truncation emerged.

##4. PACF of this model is exponentially decreased or some sine curves are identified.