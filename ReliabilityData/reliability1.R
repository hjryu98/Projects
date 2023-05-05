####1. ���� ������ �ð�ȭ
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


####2.���� ������ ��� ���� ���� ��� ���� ��������, ���̺� ����, ������� ����
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

####ù ��° ������ ��쿡�� aic �� bic�� ���� ���� ��� ���԰� fit�Ѵٰ� �� �� �ִ�.



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

###�� ��° ������ ��쿡�� aic �� bic�� ���� ���� ��� ���԰� fit�Ѵٰ� �� �� �ִ�.



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

####�� ��° ������ ��쿡�� aic �� bic�� ���� ���� ��� ���԰� fit�Ѵٰ� �� �� �ִ�.

####3. ������ ��� ���� ���� ��� >> corner case�� ������, ��ü������ �������� �� AIC,BIC�� ū ���̰� �ȳ���, ������ ���迡���� ��� ���԰� ���ƺ�������, ��ü�����δ� ���̺� ������ FIT�� ���ƺ���
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



####�� case�� ���� ���� ���� >> Weibull dist.
library(WeibullR)
library(SPREDA)
library(boot)
library(lattice)

MTTF <- mean(rweibull(15, shape = 1, scale = 0.1048))
MTTF
