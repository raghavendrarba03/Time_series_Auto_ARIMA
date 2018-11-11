library(forecast)
library(tseries)
#Load in built R data
hl=read.csv("C:\\Users\\Raghavendra Reddy\\Documents\\SENSEX.csv")
#Show the data
hl<-ts(hl, start=c(2016,1), end=c(2018,9), frequency = 12)
plot(hl)

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
boxplot(hl~cycle(hl),xlab="Months",ylab="sensex")

#test stationary of the time series data
adf.test(hl, alternative = "stationary",k=12)

plot(hl)
plot(log(hl))
plot(diff(log(hl)))
hl1<-diff(log(hl))
adf.test(hl1, alternative = "stationary",k=12)
