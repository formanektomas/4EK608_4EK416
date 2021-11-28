#### VAR models: Construction, Testing, IRFs, Forecasts ####
#    
#
#
#
# Model data:
# Monthly TS, 1917M08 - 2005M04
#
# G      - growth of currency in the hands of general public
# GV     - growth of vault cash
# 
#
#
#
#
rm(list=ls())
cv.data <- read.csv("currencyvolume.csv")
# Series "G" is observed from 1917:M09. 
# Series GV is observed from 1959:M12.
# The series GV contains many irregular observations and outliers 
# during years 1959 - 1962. Hence, for this example, we only use
# G and GV observations starting from 1963:M01.
cv.ts <- ts(cv.data[546:1053, ], start=c(1963,1), frequency = 12)
cv.ts[,"OBS"] <- 1:508 # Observations are numbered starting on 1963:M01
library(zoo)
cv.zoo <- zoo(cv.ts)
# 
#
#
#
#
# Basic data plots
# Whole period 1963:M01 - 2005:M04
par(mfrow = c(2,1))
plot.zoo(cv.zoo[,"G"])
plot.zoo(cv.zoo[,"GV"])
# plot 1990:M01 - 2005:M04 only
plot.ts(cv.zoo[325:508, "G"])
plot.ts(cv.zoo[325:508, "GV"])
par(mfrow = c(1,1))
#
plot(cv.zoo[,"G"] ~ cv.zoo[,"GV"], pch=20, cex=0.5)
abline(lm(G ~ GV, data=cv.zoo), col = "red", lwd=2)
# 
#
#
#
### STEP 1 - VAR Model construction & preliminary tests
#
#
#
#
# Stationariry of the series
library(forecast) #install.packages("forecast")
?ndiffs
ndiffs(cv.zoo[,"G"], alpha=0.05)
ndiffs(cv.zoo[,"GV"], alpha=0.05)
#
?nsdiffs
nsdiffs(cv.ts[,"G"], alpha=0.05)
nsdiffs(cv.ts[,"GV"], alpha=0.05)
#
cv.zoo$dGV <- diff(cv.zoo[,"GV"])
cv.zoo$dG <- diff(cv.zoo[,"G"])
# .. Full ADF-test results may be obtained using {urca} or {tseries} packages
#
# Granger causality tests
library(lmtest) #install.packages("lmtest")
# max. lag for the test is set ad-hoc:

grangertest(cv.zoo[,"dGV"], cv.zoo[,"dG"], order=12) 
grangertest(cv.zoo[,"dG"], cv.zoo[,"dGV"], order=12)  
# Also, we try max. lag = 4:
grangertest(cv.zoo[,"dGV"], cv.zoo[,"dG"], order=4)
grangertest(cv.zoo[,"dG"], cv.zoo[,"dGV"], order=4) 
# 
#
# Maximum lag selection for the VAR model
library(vars) #install.packages("vars")
?VARselect
# We need to specify a dataset containing endogenous variables only
colnames(cv.zoo)
cv.zoo.2 <- cv.zoo[2:nrow(cv.zoo), c("dG","dGV")] # NAs in 1st row due to 1st diffs
VARselect(cv.zoo.2, lag.max=12, type="const")
# 
#
#
#
#
# STEP 2 - VAR model estimation & diagnostic tests
#
?VAR # VAR() function cannot handle NA values!
#
# NOTE:
# For the sake of simplicity, we start with a VAR(2) model 
# .. not consistent with the VARselect() results,
# .. this VAR(2) follows the text example in Chapter 14.
# .. More complex specifications (seasonality, etc.) are provided
# .. at the end of this example & in subsequent VAR-model examples.
#
varmodel <- VAR(cv.zoo.2, p=2, type="const")
summary(varmodel)
str(varmodel) # Please note the complexity of the estimated varmodel() object
coefficients(varmodel)
dev.off() # resets the plot settings
# NOTE: Enlarge the Plot window to maximum (full) size before plotting!
plot(varmodel) # please note the y-o-y high ACF and PACF values..
#
# Tests Autocorrelation in residuals
?serial.test # H0: Residuals up to lag are NOT autocorrelated
serial.test(varmodel, lags.pt=10, type="BG")
# Residuals are autocorrelated, seasonality and VAR order should be considered...
# .. one possible solution/estimation approach is provided below..
#
# Test for normality of residuals
normality.test(varmodel)
# H0 of normality is rejected
#
# CUSUM test/plot for stability of the VAR model
# .. see e.g. 
# http://www.eviews.com/help/helpintro.html#page/content%2Ftesting-Stability_Diagnostics.html%23ww183720
stability.test <- stability(varmodel, type="Rec-CUSUM")
plot(stability.test)
# Roots
# .. VAR(p)-process is stable, if all eigenvalues (characteristic numbers) 
# .. of the companion matrix A from an equivalent VAR(1) representation 
# .. have modulus (absolute value) less than one.
# .. see Lütkepohl, H., New Introduction to Multiple Time Series Analysis
roots(varmodel)
#
#
#
#
#
# STEP 3 - Impulse response functions
#
?irf
IRFs <- irf(varmodel, n.ahead = 24) # ortho=TRUE is the default setting
IRFs # table output of the IRF function
str(IRFs)
plot(IRFs)
# .. "Orthogonal Impulse Response from G" 
# .. i.e. the plot shows responses of G and GV to an impulse from G.
# 
# Cumulative IRFs
CIRFs <- irf(varmodel, n.ahead=24, cumulative=T)
CIRFs
plot(CIRFs)
# 
#
#
#
#
# STEP 4 - Predictions from an estimated VAR model
#
# Predictions from an estimated model
?vars:::predict
var.pred <- predict(varmodel, n.ahead = 36)
var.pred # table-form output
str(var.pred) # object structure 

# fitted values - obs. period
var.pred$model$varresult$G$fitted.values
var.pred$model$varresult$dGV$fitted.values
# forecasts - ex ante forecasts
var.pred$fcst$G
var.pred$fcst$dGV
# Forecast plots
?fanchart
dev.off()
fanchart(var.pred)
fanchart(var.pred, xlim=c(400,541))
dev.off() # resetting plot settings
#
# Forecast error variance decomposition
var.dec <- fevd(varmodel, n.ahead=36)
var.dec
plot(var.dec)
# 
#
#
#
#
#
# VAR model for G & GV re-estimated, adjusted for seasonality
#
# We have data with month-based seasonality, hence: season = 12
VARselect(cv.zoo.2, lag.max=24, type="const", season =12)
varmodel.sa <- VAR(cv.zoo.2, p=13, season = 12, type="const")
summary(varmodel.sa)
coefficients(varmodel.sa)
dev.off() # reset plot settings
plot(varmodel.sa)
# Test for autocorrelation in residuals
serial.test(varmodel.sa, lags.pt=10, type="BG")
# Test for normality
normality.test(varmodel.sa)
# Stability of the model
plot(stability(varmodel.sa))
roots(varmodel.sa)
# IRFs
plot(irf(varmodel.sa, n.ahead = 24))
# Cumulative IRFs
plot(irf(varmodel.sa, n.ahead = 24, cumulative = T))
# Forecasts
var.pred.sa <- predict(varmodel.sa, n.ahead=36)
dev.off() #
fanchart(var.pred.sa, xlim=c(400,541))
# Forecast error variance decomposition
plot(fevd(varmodel.sa, n.ahead=36))
#
#
#
#
#
#
## Assignment 1
## (1) Use the variables G and dGV to create a VAR(4) model, save the object as "Var.4"
##     Use the "season = 12" argument to handle seasonality in the data.
## (2) Plot cumulative IRFs for a period of 24 months ahead, based on Var.4
## (3) Plot a 2-year forecast for both series, based on Var.4. Include 3 years of
##     pre-forecast (actual, observed) data in the plot.
## 
#
#
#
#
#
#
#