##################################
#Time Series Analysis in R
#This file....

#Created by: Nicholas Davidson 10/1/2020
########################################

#Clear all variables in workspace
rm(list=ls())

#Load the forecasting package
#install.packages('fpp2', dependencies = TRUE)
library(fpp2)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('forecast')
library(forecast)
#Load the data
data <- read.csv(file.choose())

#Declare data as time series data
Y <- ts(data[,2], start = c(1992, 1), frequency = 12)

#################################################
#Preliminary Analysis
#################################################

#Time PLot 
autoplot(Y) +
  ggtitle("Time PLot: ") +
  ylab("Millions of dollars adjusted for infaltion in 2017")

#Data has a strong trend 
#Take the first difference
DY <- diff(Y)

#Time plot of difference data
autoplot(Y) +
  ggtitle("TIme plot of difference data") +
  ylab("Millions of dollars adjusted for inflation in 2017")

# Series appears trend-staionary, investigate seasonality now
ggseasonplot(DY) +
  ggtitle("Seasonal Plot: Change in daily retail sales") +
  ylab("Millions of dollars adjusted for inflation in 2017")

#Another seasonal plot: Subseries Plot
ggsubseriesplot(DY) +
  ggtitle("Subseasonal Plot: Change in daily retail sales") +
  ylab("Millions of dollars adjusted for inflation in 2017")

###############################################################
# Our series Y, has trend and seasonality
# To remove the trend we take the first difference
# The first differenced series still has seasonality
#
# Forecast with various methods and pick the best fit
###############################################################

#######################################
# Benchmark forecast
# Using Seasonal naive model as benchmark
# y_t = y_(t-s) + e_t
#######################################
fit <- snaive(DY) #Residual SD = 8.55
print(summary(fit))
checkresiduals(fit)

#######################################
# Fit ETS method
#######################################
fit_ets <- ets(DY) #Residual SD = 
print(summary(fit_ets))
checkresiduals(fit_ets)

#######################################
# Fit on ARIMA model
#######################################
fit_arima <- auto.arima(Y, d=1, D=1, stepwise = FALSE, trace = TRUE) #Residual SD = x
print(summary(fit_arima))
checkresiduals(fit_arima)

#######################################
#Forecast with ARIMA model
#######################################
fcst <- forecast(fit_arima, h = 24)
autoplot(fcst)
print(summary(fcst))

