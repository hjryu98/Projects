###default packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("fitdistrplus")
install.packages("GofCens")
install.packages("WeibullR")
install.packages("SPREDA")
install.packages('boot')
install.packages('lattice')
install.packages('nlme')

###1 데이터 불러오기 및 데이터 변환

target = read.csv('C:/Users/nha98/Documents/practiceR/example3.csv')
target1 = target[target$CELL == 1,]
target2 = target[target$CELL == 2,]
head(target)


###2 최적의 열화경로 선정

##CELL 1
plot(target1$CYCLE, target1$VOLTAGE)

#linear
model1_1 = nls(VOLTAGE ~ a * CYCLE + b, start = list(a = -0.001, b = 0.00001), data = target1)
lines(target1$CYCLE, predict(model1_1, newdata = target1), col = 'yellow')

#exponential
model1_2 = nls(VOLTAGE ~ a * exp(b * CYCLE), start = list(a = -0.001, b = 0.00001), data = target1)
lines(target1$CYCLE, predict(model1_2, newdata = target1), col = 'red')

#power
model1_3 = nls(VOLTAGE ~ a * CYCLE^b, start = list(a = -0.001, b = 0.00001), data = target1)
lines(target1$CYCLE, predict(model1_3, newdata = target1), col = 'green')

#MSE
MSE1_1 = sum((predict(model1_1, newdata = target1) - target1$VOLTAGE)^2) / length(target1$VOLTAGE)
MSE1_2 = sum((predict(model1_2, newdata = target1) - target1$VOLTAGE)^2) / length(target1$VOLTAGE)
MSE1_3 = sum((predict(model1_3, newdata = target1) - target1$VOLTAGE)^2) / length(target1$VOLTAGE)

#MAE
MAE1_1 = sum(abs(predict(model1_1, newdata = target1) - target1$VOLTAGE)) / length(target1$VOLTAGE)
MAE1_2 = sum(abs(predict(model1_2, newdata = target1) - target1$VOLTAGE)) / length(target1$VOLTAGE)
MAE1_3 = sum(abs(predict(model1_3, newdata = target1) - target1$VOLTAGE)) / length(target1$VOLTAGE)

#RMSE
RMSE1_1 = sqrt(sum((predict(model1_1, newdata = target1) - target1$VOLTAGE)^2) / length(target1$VOLTAGE))
RMSE1_2 = sqrt(sum((predict(model1_2, newdata = target1) - target1$VOLTAGE)^2) / length(target1$VOLTAGE))
RMSE1_3 = sqrt(sum((predict(model1_3, newdata = target1) - target1$VOLTAGE)^2) / length(target1$VOLTAGE))

#result
MSE1_1; MSE1_2; MSE1_3
MAE1_1; MAE1_2; MAE1_3
RMSE1_1; RMSE1_2; RMSE1_3;



##CELL 2
plot(target2$CYCLE, target2$VOLTAGE)

#linear
model2_1 = nls(VOLTAGE ~ a * CYCLE + b, start = list(a = -0.001, b = 0.00001), data = target2)
lines(target2$CYCLE, predict(model2_1, newdata = target2), col = 'yellow')

#exponential
model2_2 = nls(VOLTAGE ~ a * exp(b * CYCLE), start = list(a = -0.001, b = 0.00001), data = target2)
lines(target2$CYCLE, predict(model2_2, newdata = target2), col = 'red')

#power
model2_3 = nls(VOLTAGE ~ a * CYCLE^b, start = list(a = -0.001, b = 0.00001), data = target2)
lines(target2$CYCLE, predict(model2_3, newdata = target2), col = 'green')

#MSE
MSE2_1 = sum((predict(model2_1, newdata = target2) - target2$VOLTAGE)^2) / length(target2$VOLTAGE)
MSE2_2 = sum((predict(model2_2, newdata = target2) - target2$VOLTAGE)^2) / length(target2$VOLTAGE)
MSE2_3 = sum((predict(model2_3, newdata = target2) - target2$VOLTAGE)^2) / length(target2$VOLTAGE)

#MAE
MAE2_1 = sum(abs(predict(model2_1, newdata = target2) - target2$VOLTAGE)) / length(target2$VOLTAGE)
MAE2_2 = sum(abs(predict(model2_2, newdata = target2) - target2$VOLTAGE)) / length(target2$VOLTAGE)
MAE2_3 = sum(abs(predict(model2_3, newdata = target2) - target2$VOLTAGE)) / length(target2$VOLTAGE)

#RMSE
RMSE2_1 = sqrt(sum((predict(model2_1, newdata = target2) - target2$VOLTAGE)^2) / length(target2$VOLTAGE))
RMSE2_2 = sqrt(sum((predict(model2_2, newdata = target2) - target2$VOLTAGE)^2) / length(target2$VOLTAGE))
RMSE2_3 = sqrt(sum((predict(model2_3, newdata = target2) - target2$VOLTAGE)^2) / length(target2$VOLTAGE))

#Result
MSE2_1; MSE2_2; MSE2_3
MAE2_1; MAE2_2; MAE2_3
RMSE2_1; RMSE2_2; RMSE2_3
