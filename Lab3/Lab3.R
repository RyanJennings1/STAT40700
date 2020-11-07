#!/usr/bin/env Rscript
# name: Lab3.Rscript
# author: Ryan Jennings
# date: 2020-11-07

library(forecast)
library(lmtest)

exercise1 <- function() {
  Z = scan("series_for_A3.txt")
  model_Z = arima(Z, include.mean=FALSE, order=c(1,0,1))
  residuals_Z = residuals(model_Z)
  fitted_Z = fitted(model_Z)

  png('exercise1_residuals.png')
  plot(residuals_Z)
  dev.off()

  png('exercise1_qqline_residuals.png')
  qqnorm(residuals_Z)
  qqline(residuals_Z)
  dev.off()

  print(shapiro.test(residuals_Z))

  png('exercise1_fitted_residuals.png')
  plot(fitted_Z, residuals_Z)
  dev.off()

  png('exercise1_acf_residuals.png')
  acf(residuals_Z)
  dev.off()

  print(Box.test(residuals_Z, type='Ljung-Box', lag=12, fitdf=2))

  png('exercise1_pacf_residuals.png')
  pacf(residuals_Z)
  dev.off()

  print('end of method')
}

exercise2 <- function() {
  Z = scan("series_for_A3.txt")
  model_Z = arima(Z, include.mean=FALSE, order=c(10,0,0))
  residuals_Z = residuals(model_Z)
  fitted_Z = fitted(model_Z)

  png('exercise2_residuals.png')
  plot(residuals_Z)
  dev.off()

  png('exercise2_qqline_residuals.png')
  qqnorm(residuals_Z)
  qqline(residuals_Z)
  dev.off()

  print(shapiro.test(residuals_Z))

  png('exercise2_fitted_residuals.png')
  plot(fitted_Z, residuals_Z)
  dev.off()

  png('exercise2_acf_residuals.png')
  acf(residuals_Z)
  dev.off()

  print(Box.test(residuals_Z, type='Ljung-Box', lag=20, fitdf=10))

  png('exercise2_pacf_residuals.png')
  pacf(residuals_Z)
  dev.off()

  print(coeftest(model_Z))

  print('end of method')
}

exercise3 <- function() {
  print('exercise 3')
}

exercise2()
