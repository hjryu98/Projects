####1. 시험 데이터 시각화
install.packages("dplyr")
install.packages("ggplot2")
install.packages("fitdistrplus")
install.packages("GofCens")
install.packages("WeibullR")
install.packages("SPREDA")
install.packages('boot')
install.packages('lattice')

library(ggplot2)
library(dplyr)
data <- read.csv("C:/Users/nha98/Documents/practiceR/practice1.csv", header = T)
data
par(mfrow=c(3,1))
qqnorm(data$test1)
qqline(data$test1)
qqnorm(data$test2)
qqline(data$test2)
qqnorm(data$test3)
qqline(data$test3)


####2.시험 데이터 통계 분포 적합 결과 각각 지수분포, 와이블 분포, 대수정규 분포
library(fitdistrplus)

##1st test
data$right[data$censored1 =='fail'] = data[data$censored1 == 'fail', 'test1']
svdata1 = data.frame(left = data$test1, right = data$right)

##exp
fit1 = fitdistcens(svdata1, 'exp')
summary(fit1)

##Weibull
fit2 = fitdistcens(svdata1, 'weibull')
summary(fit2)

##lognormal
fit3 = fitdistcens(svdata1, 'lnorm')
summary(fit3)

####첫 번째 시험의 경우에는 aic 와 bic가 제일 작은 대수 정규가 fit한다고 할 수 있다.



##2nd test
data$right2[data$censored2 =='fail'] = data[data$censored2 == 'fail', 'test2']
svdata2 = data.frame(left = data$test2, right = data$right2)
svdata2 = svdata2 / 100

##exp
fit4 = fitdistcens(svdata2, 'exp')
summary(fit4)

##Weibull
fit5 = fitdistcens(svdata2, 'weibull')
summary(fit5)

##lognormal
fit6 = fitdistcens(svdata2, 'lnorm')
summary(fit6)

###두 번째 시험의 경우에는 aic 와 bic가 제일 작은 대수 정규가 fit한다고 할 수 있다.



##3rd test
data$right3[data$censored3 =='fail'] = data[data$censored3 == 'fail', 'test3']
svdata3 = data.frame(left = data$test3, right = data$right3)
svdata3 = svdata3 / 100

##exp
fit7 = fitdistcens(svdata3, 'exp')
summary(fit7)

##Weibull
fit8 = fitdistcens(svdata3, 'weibull')
summary(fit8)

##lognormal
fit9 = fitdistcens(svdata3, 'lnorm')
summary(fit9)

####세 번째 시험의 경우에는 aic 와 bic가 제일 작은 대수 정규가 fit한다고 할 수 있다.

####3. 최적의 통계 분포 선정 결과 >> corner case도 있으나, 전체적으로 고려했을 때 AIC,BIC가 큰 차이가 안나고, 마지막 시험에서만 대수 정규가 나아보였으나, 전체적으로는 와이블 분포가 FIT에 좋아보임
library(GofCens)

##1st test
data$event1[data$censored1 == 'censored'] = 0
data$event1[data$censored1 == 'fail'] = 1

exp_test1 = gofcens(times = data$test1, cens = data$event1, dist = 'exponential')
wei_test1 = gofcens(times = data$test1, cens = data$event1, dist = 'weibull')
lnorm_test1 = gofcens(times = data$test1, cens = data$event1, dist = 'lognormal')

exp_test1$`Test statistics`
wei_test1$`Test statistics`
lnorm_test1$`Test statistics`

##2nd test
data$event2[data$censored2 == 'censored'] = 0
data$event2[data$censored2 == 'fail'] = 1

exp_test2 = gofcens(times = data$test2, cens = data$event2, dist = 'exponential')
wei_test2 = gofcens(times = data$test2, cens = data$event2, dist = 'weibull')
lnorm_test2 = gofcens(times = data$test2, cens = data$event2, dist = 'lognormal')

exp_test2$`Test statistics`
wei_test2$`Test statistics`
lnorm_test2$`Test statistics`


##3rd test
data$event3[data$censored3 == 'censored'] = 0
data$event3[data$censored3 == 'fail'] = 1

exp_test3 = gofcens(times = data$test3, cens = data$event3, dist = 'exponential')
wei_test3 = gofcens(times = data$test3, cens = data$event3, dist = 'weibull')
lnorm_test3 = gofcens(times = data$test3, cens = data$event3, dist = 'lognormal')

exp_test3$`Test statistics`
wei_test3$`Test statistics`
lnorm_test3$`Test statistics`



####각 case에 대한 수명 추정 >> Weibull dist.
library(WeibullR)
library(SPREDA)
library(boot)
library(lattice)

MTTF <- mean(rweibull(15, shape = 1, scale = 0.1048))
MTTF

