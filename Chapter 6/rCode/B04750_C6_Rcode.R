#### Data Science Essentials
#### Chapter 6 - Time series Forecasting
#### Author: Sharan Kumar R and Raja B Koushik

# Set your working directory accordingly
setwd("C:/Users/Accelyst/Desktop/R Data Science Essentials/Chapter 6")
getwd()

# reading the dataset
data <- read.csv("Data/msdata.csv")
head(data, 10)
data$date <- as.Date(data$date, "%m/%d/%Y")

# Replacing NA with that particular date's average value
# create day - month column
data$dayMonth <- strftime(data$date, format="%m-%d")
# compute the averae for the day - month combination
data <- transform(data, mavg =ave(maxTemp,dayMonth, FUN=function(x) mean(x,na.rm=TRUE) ))
# replace the missing value with the mean
newdata <- transform(data, maxTemp =ifelse(is.na(maxTemp),mavg,maxTemp))
head(newdata)
# selecting required data
tsdata <- data.frame(newdata$date,newdata$maxTemp)
colnames(tsdata) <- c("date","maxTemp")
head(tsdata, 10)


# Remove NaN values - dont use
tsdata <- tsdata[complete.cases(tsdata), ]

# ensure there is no NA values
mean(tsdata$maxTemp)


# converting to timeseries
ts=ts(tsdata$maxTemp, start=c(1980,1),frequency=365)
head(ts, 10)
plot(ts)
dev.copy(png,filename="ts-plot.png", width=600, height=400);
dev.off ();



# decomposition

newWindow = window(ts, start=c(2007,1), end=c(2009, 12))
plot(newWindow)
dev.copy(png,filename="ts-subset-plot.png", width=600, height=400);
dev.off ();

decompose1 = stl(newWindow, s.window="periodic")
plot(decompose1)
dev.copy(png,filename="ts-decomp-plot.png", width=600, height=400);
dev.off ();

decompose2 = stl(ts, s.window="periodic", t.window=60, robust=TRUE)
plot(decompose2)
dev.copy(png,filename="ts-decomp-plot2.png", width=600, height=400);
dev.off ();

# ARIMA Model for forecasting
a_model=arima(ts, order=c(2,0,0))
aforecast=forecast.Arima(a_model,h=3650)
plot(aforecast)
dev.copy(png,filename="arimaf.png", width=600, height=400);
dev.off ();

a_model=arima(ts, order=c(0,1,1))
aforecast=forecast.Arima(a_model,h=3650)
plot(aforecast)
dev.copy(png,filename="arimaf2.png", width=600, height=400);
dev.off ();

# Packages required for Forecasting
install.packages("forecast")
require("forecast")#Loading Forecast Package


# Building forecasting model
h_model=HoltWinters(ts, beta=FALSE, gamma=0.5)
plot(h_model)
dev.copy(png,filename="holtm1.png", width=600, height=400);
dev.off ();
?HoltWinters
# plot forecasting with confidence interval
forecast <- predict(h_model, n.ahead = 3650, prediction.interval = T, level = 0.1)
plot(h_model, forecast)
dev.copy(png,filename="holtf1.png", width=600, height=400);
dev.off ();

# Building forecasting model
h_model=HoltWinters(ts, beta=FALSE, gamma=0.1)
plot(h_model)
dev.copy(png,filename="holtm2.png", width=600, height=400);
dev.off ();
?HoltWinters
# plot forecasting with confidence interval
forecast <- predict(h_model, n.ahead = 3650, prediction.interval = T, level = 0.1)
plot(h_model, forecast)
dev.copy(png,filename="holtf2.png", width=600, height=400);
dev.off ();

# Forecasting future data
forecast <- as.data.frame(forecast)
predscore <- hforecast$fit
predscore <- as.data.frame(predscore)
head(predscore, 10)




