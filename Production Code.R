#Cleaning the environment
rm(list=ls(all=TRUE))

install.packages("readxl")
library(readxl)

#Viewing the data
electricity.df <-read_excel('Electricity_Monthly_Timeseries.xlsx', sheet = "Final Data")
View(electricity.df)
summary(electricity.df)

#Time series analysis of production data
electricity.ts <- ts(electricity.df$`Total Energy`, start = c(2015,1), end = c(2020,7), frequency = 12)
electricity.ts

#Plot of time series data
plot(electricity.ts, xlab = ('Year'), xlim = c(2015, 2021), ylab = ('Total Energy'), ylim = c(2000, 3900), main = ('Energy Production'))

#Take 20% data as Validation set and 80% data as Training set; Since there are 67 records, nValid= (0.2*6=13.4)
nValid <- 13
nTrain <- length(electricity.ts) - nValid

#Partitioning the data into training and validation sets
train.ts <- window (electricity.ts, start = c(2015,1), end = c(2015, nTrain))
train.ts
valid.ts <- window (electricity.ts, start = c(2015, nTrain+1), end = c(2015, nTrain+nValid))
valid.ts

#Plotting the training and validation sets in one graph; Appending validation set with training set
plot(train.ts, xlab = ('Year'), xlim = c(2015, 2021), ylab = ('Total Energy'), ylim = c(2000, 3800), main = ('Training & Validation Set: Energy Production'))
lines(valid.ts, col='red')   

library(forecast)

#ACF Plot
install.packages("ggfortify")
install.packages("fpp2")
library(ggfortify)
library(fpp2)

ggAcf(electricity.ts)

###### Checking for 1 year and 4 years; h = 24 for 1 year forecast and h = 60 for 4 year forecast

#1st Model: Seasonal Naive model
test.pred <- snaive(train.ts, h=60)
test.pred

#Prediction
ypred_snaive <- forecast(test.pred, h=60)
ypred_snaive

#Accuracy
accuracy(ypred_snaive, valid.ts)

#2nd Model: Linear Rgression model
train.lm. <- tslm(train.ts ~ trend + season)
train.lm.

#Prediction
ypred_lm <- forecast(train.lm., h=60)
ypred_lm

#Accuracy
accuracy(ypred_lm, valid.ts)

#3rd Model: Holt- Winters - ZZZ
hw.lm <- ets(train.ts, model = 'ZZZ')
hw.lm

#Prediction
ypred_HW_ZZZ <- forecast(hw.lm, h=60)
ypred_HW_ZZZ

#Accuracy
accuracy(ypred_HW_ZZZ, valid.ts)

#4th Model: Holt- Winters - ZAA
hw.lm <- ets(train.ts, model = 'ZAA')
hw.lm

#Prediction
ypred_HW_ZAA <- forecast(hw.lm, h=60)
ypred_HW_ZAA

#Accuracy
accuracy(ypred_HW_ZAA, valid.ts)

#5th Model: ARIMA
library(tseries)
#autoarima function
productionnmodel = auto.arima(train.ts, ic ="aic", trace = TRUE)
productionnmodel
arimaforecast = forecast(productionnmodel, level = c(95), h = 60)
arimaforecast
plot(arimaforecast)
accuracy(arimaforecast, valid.ts)

#For quick comparison
accuracy(ypred_snaive, valid.ts)
accuracy(ypred_lm, valid.ts)
accuracy(ypred_HW_ZZZ, valid.ts)
accuracy(ypred_HW_ZAA, valid.ts)
accuracy(arimaforecast, valid.ts)

# Since Holt-Winters: ZAA is the best among all, this model will be used for forecasting
forecasting <- forecast(ypred_HW_ZAA, h = 60)   

ypred_HW_ZAA <- forecast(train.lm.,h = 60)
ypred_HW_ZAA

plot(forecasting, xlab = ('Year'), ylab = ('Total Energy'))
lines(valid.ts, col='orange')
lines(c(2019.45,2019.45), c(0, 3100), col='green')
lines(c(2020.5, 2020.5), c(0, 3100), col='green')
text(2017, 3000, "Training")
text(2020, 3000, "Validation")
text(2022.5, 3000, "Test")

predict(forecasting)

#Decomposing the plot
decompose_Add = decompose(train.ts, "additive")
autoplot(decompose_Add)
