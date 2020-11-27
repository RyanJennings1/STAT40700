#!/usr/bin/env Rscript
# name: Lab4.Rscript
# author: Ryan Jennings
# date: 2020-11-07

library(TSA)
library(forecast)
library(tseries)
library(urca)

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


  # c
}

exercise2()
