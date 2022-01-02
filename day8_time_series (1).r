
install.packages("MASS")
install.packages("caTools")
install.packages("tseries")

install.packages(c("tseries","MASS","caTools","psych"))
library(tseries)
library(MASS)
library(caTools)
library(psych)

data(AirPassengers)
class(AirPassengers)
#This tells the data series
View(AirPassengers)
AirPassengers
start(AirPassengers)
end(AirPassengers)
#to see interval of time freq. fun
frequency(AirPassengers)
summary(AirPassengers)
#getwd()
#No. of passengers across the time
plot(AirPassengers)
abline(reg = lm(AirPassengers~time(AirPassengers)))
# to observe cycles in the data
# To check stationary of data find cov, var, mean, median#
cycle(AirPassengers)
plot(aggregate(AirPassengers, FUN = mean))

# this will aggregate the cycles and display a year
boxplot(AirPassengers~cycle(AirPassengers))
# To find stationary score

#for smoothing we use log, differntiation,etc.
plot(log(AirPassengers))
abline(reg=lm(log(AirPassengers)~time(AirPassengers)))

# plot(diff(log(AirPassengers)))
# abline(reg=lm(diff(log(AirPassengers))~time(diff(log(AirPassengers)))))

plot(aggregate(log(AirPassengers), FUN = mean))


x= aggregate(log(AirPassengers), FUN = mean)
x
# to check all components


plot(decompose(AirPassengers))

x<- log(AirPassengers)
plot(decompose(x))
plot(diff(log(AirPassengers)))
plot(diff(diff(log(AirPassengers))))
abline(reg=lm(diff(log(AirPassengers))~time(diff(log(AirPassengers)))))

#Augmented Dickey Fuller test

adf.test(diff(AirPassengers),alternative = "stationary", k=0)
adf.test(diff(log(AirPassengers)),alternative = "stationary", k=0) 
# AR I MA
#p  d  q
#acf-auto corr. fun, pacf-partial auto corr. fun.
#acf(AirPassengers)
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))
# acf plot cuts off after the first lag
#so p should be 0
#q shold be 1 or 2
#so (p,d,q) shd. be (0,1,1) 
# ARIMA ,model and predict for next 10 year


fit <- arima(log(AirPassengers), c(1, 1, 1),seasonal = list(order = c(1, 1, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
pred
ts.plot(AirPassengers,pred$pred, log = "y", lty = c(1,3))
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))


#DIFF  ---x(t)-x(t-1)     d=1
AirPassengers-nonsta
log(AirPassengers)-nonsta
diff(lof(AirPassengers))
