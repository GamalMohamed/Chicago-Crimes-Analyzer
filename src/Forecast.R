rm(list=ls())

library(dplyr)
library(highcharter)
library(prophet)
library(Metrics)

##Reading dataset
crimes2012_2017 = read.csv("Dataset/2012_to_2017.csv",stringsAsFactors=F)

crimes2012_2015 = crimes2012_2017[crimes2012_2017$Year %in% c('2012', '2013', '2014', '2015'), 
                                  c('Date', 'ID')] # Picking 2012 to 2015 crimes only!

crimes =  crimes2012_2015[,]

crimes$Date = as.Date(crimes$Date, "%m/%d/%Y %I:%M:%S %p")

# Prepare model data frame required by Prophet
mdf = na.omit(crimes) %>% group_by(Date) %>% summarise(y = n()) %>% mutate(y = log(y))
names(mdf) = c("ds", "y")
mdf$ds = factor(mdf$ds)

# Model fitting
model = prophet(mdf)

# Creating historical and fture data frame
future = make_future_dataframe(model, periods = 365 * 4)

## Predict future values
forecast = predict(model, future)

## insights on some predicted values
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

## Plot model and forecasts
plot(model, forecast)

## Plot time series components 
prophet_plot_components(model, forecast)


## Evaluating model accuracy
crimes2016 = crimes2012_2017[crimes2012_2017$Year =='2016', c('Date', 'ID')]
crimes2016$Date = as.Date(crimes2016$Date, "%m/%d/%Y %I:%M:%S %p")
crimes2016 = crimes2016 %>% group_by(Date) %>% summarise(y = n())
crimes2016$y_log = log(crimes2016$y)

ccf=forecast[,]
ccf$Year = factor(year(as.POSIXlt(ccf$ds, format="%Y/%m/%d")))
ccf$ya= round(exp(ccf$yhat))
crimes2016_predicted = ccf[ccf$Year == '2016', c('ds', 'yhat', 'ya')]

all= crimes2016[,]
all$yp_log=crimes2016_predicted$yhat
all$yp=crimes2016_predicted$ya


## Calculating MSE & MAPE
rmse(actual =  crimes2016$y,predicted = crimes2016_predicted$ya) / mean(crimes2016$y)
mae(actual =  crimes2016$y,predicted = crimes2016_predicted$ya) / mean(crimes2016$y)
mape(actual = crimes2016$y, predicted = crimes2016_predicted$ya)
