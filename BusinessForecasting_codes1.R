#CLear the environment
rm(list=ls(all=TRUE))

#Required packages for the functioning of the codes
install.packages("readxl")
install.packages("dplyr")
install.packages("ggfortify")
install.packages("fpp2")
library(fpp2)
library(ggfortify)
library(readxl)
library(dplyr)
library(forecast)
library(ggfortify)
library(zoo)
library(tseries)

#Data Loading
consumption.df <-read_excel('IEloadconsumptiondataset.xlsx', sheet = "Final - Monthly Average")

#Viewing the data and it's statistical information
View(consumption.df)
summary(consumption.df)

#Time Series
Consumption.ts<-ts(consumption.df$`Average of load`,start = c(2017,1),end=c(2022,7),frequency=12)
Consumption.ts
plot(Consumption.ts, xlab = ('Year'), xlim = c(2017, 2023), ylab = ('Total Energy'), ylim = c(2000, 3900), main = ('Energy Consumption'))

#Additinal Styled Graphs
autoplot(Consumption.ts, facets = F)
ggseasonplot(Consumption.ts,polar = T)
ggsubseriesplot(Consumption.ts)
ggAcf(Consumption.ts)


###Dividing data into training and validation sets##
nvalid <- 13
ntrain <-length(Consumption.ts) -nvalid
train.ts <- window(Consumption.ts, start = c(2017,1), end = c(2017, ntrain))
valid.ts <- window(Consumption.ts, start =c(2017, ntrain +1), 
                   end = c(2017, ntrain+nvalid))
ntrain
train.ts
valid.ts
plot(train.ts, xlab = ('Year'), xlim = c(2017, 2023), ylab = ('Total Energy'), ylim = c(2000, 3900), main = ('Training and Validation Set:Energy Consumption'))
lines(valid.ts, col='red')
lines(c(2021.4,2021.5), c(0, 3800), col='green')
text(2019, 3900, "Training")
text(2022, 3900, "Validation")
#Moving Average Line
lines(ma(train.ts, order = 12, centre =TRUE),col = "blue")
#DEcompose Function
decompose_Add = decompose(train.ts, "additive")
autoplot(decompose_Add)

#Tests For Stationarity Of Time-Series
adf.test(train.ts, k= 7)
####autocorrelation###
acf(train.ts)#high correlation in itself;it's not stationary
##partial stationay test
pacf(train.ts)##blue lines crossed, so minimal effect
adf.test(train.ts)

#########seasoanality, trend, level, noise all are present in this dataset###
#We can't use Exponential Series as the graph has seasonality####

#Seasonal Naive###to be seen later
snaive.pred <- snaive(train.ts, h = 24)
autoplot(snaive.pred)
accuracy(snaive.pred,valid.ts)

#Linear regresion model###
train.lm. <- tslm(train.ts ~ trend + season)
train.lm.
ypred <-  forecast(train.lm., h = 24)
ypred
accuracy(ypred, valid.ts)

#holtz wintee model-ZAA##
hw.lm<- ets(train.ts,model = 'ZAA')
hw.lm
ypred_holtz_zaa<-forecast(hw.lm,h = 24)
ypred_holtz_zaa
accuracy(ypred_holtz_zaa, valid.ts)
#holtz wintee model-ZAA#

hw.lm1<- ets(train.ts,model = 'ZZZ')
hw.lm1
ypred_holtz_default<-forecast(hw.lm1,h = 24)
ypred_holtz_default
accuracy(ypred_holtz_default, valid.ts)

#ARIMA model
consumptionmodel = auto.arima(train.ts, ic ="aic", trace = TRUE)
#Best Model:ARIMA(1,0,0)(1,1,1)[12] 
consumptionmodel
acf(ts(consumptionmodel$residuals))
pacf(ts(consumptionmodel$residuals))
arimaforecast = forecast(consumptionmodel, level = c(95), h = 24)
arimaforecast
plot(arimaforecast)
accuracy(arimaforecast, valid.ts)
##test##
Box.test(consumptionmodel$residuals, lag =5, type = "Ljung-Box")

#As the best fit model is Holtz-Winter Model, Forecast using that###
forecasting<-forecast(ypred_holtz_default, h = 24)
ypred_holtz_default<-forecast(ypred_holtz_default,h = 24)
ypred_holtz_default
plot(forecasting, xlab = ('Year'),ylab = ('Total Energy'))
lines(valid.ts, col='orange')
lines(c(2021.4,2021.5), c(0, 3800), col='green')
lines(c(2022.5, 2022.5), c(0, 3800), col='green')
text(2019, 3900, "Training")
text(2022, 3900, "Validation")
text(2023, 3900, "Test")
predict(forecasting)
###############



