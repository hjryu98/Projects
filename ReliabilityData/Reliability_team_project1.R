###1 

target = read.csv('C:/Users/nha98/Documents/practiceR/fatigue crack size.csv')
head(target)
target1 = target[target$unit == 1,]
head(target1)


###2 

plot(target1$millions.of.cycles, target1$value)

#linear, parameter estimation
model1_1 = nls(value ~ a * millions.of.cycles + b, start = list(a = 1, b = 0.0001), data = target1)

print(summary(model1_1))

#exponential, parameter estimation
model1_2 = nls(value ~ a * exp(b * millions.of.cycles), start = list(a = 1, b = 0.0001), data = target1)

print(summary(model1_2))

#power, parameter estimation
model1_3 = nls(value ~ a * (millions.of.cycles)^b, start = list(a = 1, b = 0.0001), data = target1)

print(summary(model1_3))

##re-fitting for the three types of models.
model1_1 = nls(value ~ a * millions.of.cycles + b, start = list(a = 7.78788, b = 0.84455), data = target1)
model1_2 = nls(value ~ a * exp(b * millions.of.cycles), start = list(a = 0.86795 , b = 6.68967), data = target1)
model1_3 = nls(value ~ a * (millions.of.cycles)^b, start = list(a = 2.8498, b = 0.2699), data = target1)

lines(target1$millions.of.cycles, predict(model1_1, newdata = target1), col = 'yellow')
lines(target1$millions.of.cycles, predict(model1_2, newdata = target1), col = 'red')
lines(target1$millions.of.cycles, predict(model1_3, newdata = target1), col = 'green')


#MSE
MSE1_1 = sum((predict(model1_1, newdata = target1) - target1$value)^2) / length(target1$value)
MSE1_2 = sum((predict(model1_2, newdata = target1) - target1$value)^2) / length(target1$value)
MSE1_3 = sum((predict(model1_3, newdata = target1) - target1$value)^2) / length(target1$value)

#MAE
MAE1_1 = sum(abs(predict(model1_1, newdata = target1) - target1$value)) / length(target1$value)
MAE1_2 = sum(abs(predict(model1_2, newdata = target1) - target1$value)) / length(target1$value)
MAE1_3 = sum(abs(predict(model1_3, newdata = target1) - target1$value)) / length(target1$value)

#RMSE
RMSE1_1 = sqrt(sum((predict(model1_1, newdata = target1) - target1$value)^2) / length(target1$value))
RMSE1_2 = sqrt(sum((predict(model1_2, newdata = target1) - target1$value)^2) / length(target1$value))
RMSE1_3 = sqrt(sum((predict(model1_3, newdata = target1) - target1$value)^2) / length(target1$value))

#result >> exp.
MSE1_1; MSE1_2; MSE1_3
MAE1_1; MAE1_2; MAE1_3
RMSE1_1; RMSE1_2; RMSE1_3








