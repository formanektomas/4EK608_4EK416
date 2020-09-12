#### Spurious regression - example #### 
#
# 
# 
# 
#
# Data: Two random walks without a drift,
#       Both given as a cumulative sum of
#       random elements with N(0,1) distribution,
#       200 obs. each
rm(list=ls())
set.seed(12)
rwalk1 = c(cumsum(rnorm(200)))
rwalk1.ts = ts(rwalk1)
set.seed(1)
rwalk2 = c(cumsum(rnorm(200)))
rwalk2.ts = ts(rwalk2)
#
# Plot the data
plot(rwalk1.ts, type="l", ylim = c(-8,15))
lines(rwalk2.ts, col="red")
#
# Setup a linear model and examine it
LM.1 <- lm(rwalk1.ts ~ rwalk2.ts)
summary(LM.1)
plot(resid(LM.1))
acf(resid(LM.1))
# pacf(resid(LM.1))
# Residuals seem highly autocorrelated
library(lmtest) # install.packages("lmtest")
bgtest(LM.1, order = 1)
bgtest(LM.1, order = 3)
dwtest(LM.1, alternative = "two.sided") # Durbin-Watson
#
#
# Methods for Testing for spurious regression - for nonstationary series
#
# (1) Newbold - Granger rule of thumb 
# .. NOT an actual test!
# If R^2 > DW-d, look out for potential spurious regression
summary(LM.1)$r.squared
dwtest(LM.1, alternative = "two.sided")
# Does it suggest spurious regression in this particular example?
# 
#
#
# (2) CRDW statistics (cointegration Regression DW stats.)
# The usuall DW statistics is used, however,
# we test for H0: series not cointegrated, 
#                 regression is spurious
#                 DW-d statistics = 0
#             H1: cointegration, DW-d != 0
# 
# Here, we use the approximate equivalency: DW-d ~= 2(1-rho)
#
# For alpha = 5%, we compare DW-d to its critical value of 0.386
dwtest(LM.1, alternative = "two.sided")
# Here, we do not reject H0 of no-cointegration.
#
#
# (3) Engel - Granger test for cointegration
# We test residuals from the regression for a unit root,
# usually through an ADF test.
# H0: residuals have a unit root <--> series are not cointegrated
library(tseries)  # install.packages("forecast")
adf.test(LM.1$residuals)
# alternatively, we may use ndiffs() from {forecast}
library(forecast) # install.packages("forecast")
ndiffs(LM.1$residuals, test = "adf")
# Here, we would conclude that the residuals are integrated of order 1,
# i.e. that the regression is spurious - series are not cointegrated.
# 
# Using the tseries::adf.test() function on residual series 
# (as opposed to observed series) is problematic as "wrong" (ADF instead of E-G)
# dfs and critical values are applied here
# 
# (4) Phillips - Ouliaris test for cointegration
library(urca)
?ca.po
po <- ca.po(cbind(rwalk1.ts,rwalk2.ts), demean = "constant")
po
summary(po)
# for test statistic description, see e.g.
# http://www.eviews.com/help/helpintro.html#page/content/nsreg-Testing_for_Cointegration.html
#
#
#
## Re-estimate the model using first differences
# i.e. on stationarized data.
#
fd.mod <- lm(diff(rwalk1.ts) ~ diff(rwalk2.ts))
summary(fd.mod)
plot(resid(fd.mod))
acf(resid(fd.mod))
bgtest(fd.mod, order = 1)
bgtest(fd.mod, order = 3)
#
# Compare the residuals from spurious and fd.sp
par(mfrow = c(2,1))
plot(resid(LM.1))
plot(resid(fd.mod))
par(mfrow = c(1,1))
#
#
#
#
## Excercise 1
## Setup: Make two random walks, similar to lines 12 & 15.
rho <- 1
set.seed(333)
e1 <- rnorm(500)
e2 <- rnorm(500)
y  <- c(e1[1], rep(NA,499))
x  <- c(e2[1], rep(NA,499))
#
for (ii in 2:500){
  y[ii] <- rho * y[ii-1] + e1[ii]
  x[ii] <- rho * x[ii-1] + e2[ii]
}
#
## 1) Follow lines 19&20 to plot series y and x, use "ylim = c(-15,50)"

## 2) Regress y on x, print out the summary & Durbin Watson test

## 3) Repeat steps 1 and 2 for for rho = 0.99 and rho = 0.5.
#
#
#