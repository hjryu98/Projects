install.packages("zoo")

require(zoo) #to use rollmean function

x <- c(312, 381, 317, 337, 399, 387, 375, 349, 386, 358, 389, 343, 328, 388) #dataset
data <- ts(x, start = 1, end = 14) #numeric vector to time series

ma3 <- rollmean(data, 3, align = 'right', fill = NA) #calculate the moving average for m values 3, 4, 5, respectively 
ma4 <- rollmean(data, 4, align = 'right', fill = NA)
ma5 <- rollmean(data, 5, align = 'right', fill = NA)

#insert NA to first index, and delete last index to allow the moving average to represent the predicted value
ma3.predict <- as.numeric(ma3) 
ma3.predict <- append(NA, ma3.predict)
ma3.predict <- ma3.predict[-length(ma3.predict)]
ma4.predict <- as.numeric(ma4)
ma4.predict <- append(NA, ma4.predict)
ma4.predict <- ma4.predict[-length(ma4.predict)]
ma5.predict <- as.numeric(ma5)
ma5.predict <- append(NA, ma5.predict)
ma5.predict <- ma5.predict[-length(ma5.predict)]

#plot of the dataset and moving averages that represent the predicted values
plot.ts(data, xlab = 't', ylab = 'z_t')
lines(ma3.predict, col = 'red')
lines(ma4.predict, col = 'blue')
lines(ma5.predict, col = 'green')

#find optimal moving average period m, and m = 5 is optimal because MSE5 is a minimum value among these values
MSE3 <- sum((as.numeric(data) - ma3.predict)^2 , na.rm = TRUE) / (length(ma3.predict)-sum(is.na(ma3.predict)))
MSE4 <- sum((as.numeric(data) - ma4.predict)^2 , na.rm = TRUE) / (length(ma4.predict)-sum(is.na(ma4.predict)))
MSE5 <- sum((as.numeric(data) - ma5.predict)^2 , na.rm = TRUE) / (length(ma5.predict)-sum(is.na(ma5.predict)))
MSE3
MSE4
MSE5
