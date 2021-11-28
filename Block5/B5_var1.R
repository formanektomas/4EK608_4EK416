#### VAR models & IRFs - using non-stationary vs. stationary series for estimation ####
#    
#
#
#
# Model data:
# Monthly TS, 1959M01 - 1995M04
#
# IP     - Industrial production (total) index; 1987 = 100; seas. adj.
# M1     - M1 Money stock; billions USD; s.a.
# FF     - Federal funds effective interest rate; % p.a.; s.a.
# TB3    - U.S. Treasury bill interest rate; % p.a.; not s.a.
# PPI    - Producer price index; 1982 = 100; not s.a.
#
#
#
# Basic data handling
rm(list=ls())
US.data <- read.csv("var1.csv")
US.ts <- ts(US.data, start=c(1959,1), frequency = 12)
# zoo objects provide simpler dataset manipulation as compared to ts
library(zoo) #install.packages("zoo")
US.zoo <- zoo(US.ts) 
US.zoo$dIP <- diff(US.zoo$IP)
US.zoo$dM1 <- diff(US.zoo$M1)
US.zoo$dTB3 <- diff(US.zoo$TB3)
US.zoo$dFF <- diff(US.zoo$FF)
US.zoo$dPPI <- diff(US.zoo$PPI)
head(US.zoo) # All series have observations in levels and in first differences
# 
#
#
#
### VAR model & IRFs, based on IP, M1 and TB3 series:
#   
#
#
# STEP 1: Stationariry of the series
#
library(tseries) #install.packages("tseries")
# ADF tests for level values
adf.test(US.zoo$IP, alternative ="stationary", k=6)
adf.test(US.zoo$M1, alternative ="stationary", k=6)
adf.test(US.zoo$TB3, alternative ="stationary", k=6)
#
# Test for stationariry of the differenced series
# NOTE:
# The p-values are interpolated from Table 4.2, p. 103 of Banerjee et al. (1993).
# If the computed statistic is outside the table of critical values, 
# then a warning message is generated.
# .. In our example, warning messages may be ignored. 
# .. They only signal that that p-values are even lower than 0.01.
adf.test(US.zoo$dIP[2:372], alternative ="stationary")
adf.test(US.zoo$dM1[2:372], alternative ="stationary")
adf.test(US.zoo$dTB3[2:372], alternative ="stationary")
# 
#
## Assignment 1
## Are the series stationary? What is their order of integration?
#
#
#
# Cointegration test (Johansen cointegration test)
# .. Granger causality may not be tested among non-stationary series
# .. For G-C tests, data need to be stationarized by differencing or by detrending.
library(urca) #install.packages("urca")
var.data.ts <- US.ts[, c("IP","M1","TB3")]
?ca.jo
summary(ca.jo(var.data.ts, type = "eigen", ecdet = "const", spec="longrun"))
# We do not reject the H0 that number of cointegration relations r <= 2.
#
# Maximum lag selection for the VAR model
library(vars) #install.packages("vars")
VARselect(var.data.ts, lag.max=12, type="const")
# 
#
#
# STEP 2 - VAR model estimation: series in levels are used
#
varmodel <- VAR(var.data.ts, p=4, type="const")
summary(varmodel)
coefficients(varmodel)
plot(varmodel) # please note the y-o-y high ACF and PACF values..
# Autocorrelation of residuals
serial.test(varmodel, lags.pt=10, type="BG")
# Stability
plot(stability(varmodel))
roots(varmodel)
# Impulse response functions
IRFs <- irf(varmodel, n.ahead = 24) # ortho=TRUE is the default setting
# Cumulative IRFs
CIRFs <- irf(varmodel, n.ahead=24, cumulative=T)
# 
#
#
#
# STEP 3 - VAR model estimation using stationarized data
#
# First, we need to prepare a new dataset, containing only the
# differenced endogenous variables to be used in the VAR. 
# Also, we have to leave out row 1 of the dataset, as VAR() cannot handle NAs.
var.data.ts.diff <- US.zoo[2:372, c("dIP", "dM1", "dTB3")]
varmodel.diff <- VAR(var.data.ts.diff, p=4, type="const")
summary(varmodel.diff)
coefficients(varmodel.diff)
plot(varmodel.diff) 
serial.test(varmodel.diff, lags.pt=10, type="BG")
plot(stability(varmodel.diff))
roots(varmodel.diff)
# IRFs from VARs estimated using stationary series
IRFs.d <- irf(varmodel.diff, n.ahead = 24) 
# Cumulative IRFs
CIRFs.d <- irf(varmodel.diff, n.ahead=24, cumulative=T)
# 
#
#
#
# STEP 5: Comparison of IRFs from a VAR model estimated using non-stationary 
#         vs. stationary data series
#
# For stationary series, IRFs from properly specified VARS
# should converge to zero.
# .. They do so under most circumstances, as contemporaneous effects 
# .. from any one-off shock die out over time
plot(IRFs.d) 
#
# In contrast, IRFs based on non-stationary data 
# may show persistent responses in some cases
plot(IRFs)
# 
#
# Cumulative IRFs:
#  
# The accumulated responses to any one-off shock (i.e. impulse)
# should converge to some (nonzero) constant.
plot(CIRFs.d)
#
# For non-stationary series, cumulative IRFs sometimes do not converge
plot(CIRFs)
#
# 
#
## Assignment 2
## Compare & discuss the R^2 coefficients for individual
## VAR equations: model estimated using non-stationary data
## against model estimated using first differences.
## Hint: use summary() function, or
##       R^2 for the IP variable may be obtained as follows:
summary(varmodel)$varresult$IP$r.squared
summary(varmodel.diff)$varresult$dIP$r.squared
#
#
#
#
#
#