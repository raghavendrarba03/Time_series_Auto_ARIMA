data("AirPassengers")
hl<- AirPassengers
library(forecast)
library(tseries)
#summary of time stamps
summary(hl)
cycle(hl)
start(hl)
end(hl)
#Check for missing values
sum(is.na(hl)) # there is zero missing values.

plot(hl)
abline(reg=lm(hl~time(hl)))

#boxplots of the time series.
boxplot(hl~cycle(hl),xlab="Months",ylab="Passengers in K")

#test stationary of the time series data
adf.test(hl, alternative = "stationary",k=12)

plot(hl)
plot(log(hl))
plot(diff(log(hl)))
plot(diff(log(log(hl))))

#test again for stationary of the time series data
adf.test(diff(log(log(hl))),alternative = "stationary",k=12)

#Fit ARIMA MODEL
model1<-auto.arima(fl)
model1

resd<-residuals(model1)

#plot residuals, ACF and PACF
plot(resd)
acf(resd)
pacf(resd)

resd
#Test for stationarity of the residuals
#Test whether residuals are normally distributed
adf.test(resd, alternative = "stationary")

qqnorm(residuals(model1))
qqline(residuals(model1))
#QQ plot of the residual shows that it is good model as the errors 

#now we can forecast using this model
forecast1<- forecast(model1,h=36)
plot(forecast1)

#MAE of prediction
mae=mean(abs(forecast1$residuals),na.rm=TRUE)
mae
