#!/usr/bin/env Rscript
# name: Lab4.Rscript
# author: Ryan Jennings
# date: 2020-11-07

library(TSA)
library(forecast)
library(tseries)
library(urca)
library(fGarch)

exercise1 <- function() {
  # a
  print(data(prescrip))
  tsdisplay(prescrip)
  print(adf.test(prescrip, alternative="stationary", k=0))

  # b
  n=length(prescrip)
  tt=2:n # convenience vector of time indices
  y=diff(prescrip) # first difference of the series
  fit=lm(y~tt+prescrip[-n]) # estimate alpha, omega x[t-1], beta
  yhat=fitted(fit)
  print(summary(fit))

  ssm <- sum((yhat - mean(y))^2)
  sse <- sum((y - yhat)^2)
  dofm = 2
  dofe = 64
  phi3 <- (ssm/dofm)/(sse/dofe)
  print('Phi_3 value: -> ')
  print(phi3)

  # c
  ur_res <- ur.df(prescrip, type='trend', lags=0)
  print(summary(ur_res))

}

exercise2 <- function() {
  print(data(beersales))

  # a
  tsdisplay(beersales)

  # b
  TC = ma(beersales, 12)
  tsdisplay(beersales - TC)
  tsdisplay(TC)

  # c
  decomp = decompose(beersales, type="additive")
  decomp_trend = decomp$trend
  decomp_seasonal = decomp$seasonal
  decomp_random = decomp$random
  plot(decomp)
}

exercise3 <- function() {
  print(data(google))
  google = google - mean(google)

  # a
  tsdisplay(google)

  # b
  adf.test(google)

  m1.google = arima(google, order=c(0, 0, 0))
  res.google = residuals(m1.google)
  McLeod.Li.test(y=res.google)

  tsdisplay(res.google^2)

  # c
  fit1 = garchFit(~arma(0, 0) + garch(1, 0), google, include.mean=F)
  summary(fit1)
  #Box.test((fit1@residuals/fit1@sigma.t)^2, lag=10, t='Ljung')
  print("-----------------------------------------------------------")
  fit2 = garchFit(~arma(0, 0) + garch(1, 1), google, include.mean=F)
  summary(fit2)
  print("-----------------------------------------------------------")
  fit3 = garchFit(~arma(0, 0) + garch(2, 1), google, include.mean=F)
  summary(fit3)
  print("-----------------------------------------------------------")
  fit4 = garchFit(~arma(0, 0) + garch(2, 2), google, include.mean=F)
  summary(fit4)

  # d
  #par(mfrow=c(2, 1))
  #plot(fit2@sigma.t^2, type='l', ylab='Conditional Variances', xlab='Time', main='Google Conditional Variances')
  #plot(google, main='Google Time Series')

  #par(mfrow=c(2, 1))
  #plot(fit2@residuals/fit2@sigma.t, type='h', ylab='Standard Residuals', xlab='Time', main='Google ACF Plot for Standard Residuals')
  #acf((fit2@residuals/fit2@sigma.t)^2, main='ACF Plot for Squared Residuals')
}

exercise3()
