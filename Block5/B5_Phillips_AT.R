#### VAR models: Construction, Testing, IRFs, Forecasts ####
#    
#
#
#
#
#
# The use of "gap" variables
# 
# 1) Comes from theoretically justified unobservable variables 
#    (output gap, potential output, NAIRU, ...)
# 2) Gap variables are mostly stationary (trend-stationary) - unless major shocks happen
# 3) Based on the assumption that economies operate close to their
#    potentials, but are affected by many types of shocks, causing
#    differences between potential (optimal) and observed values.
# 4) Potential values may be approximated using moving averages or
#    using Hodrick-Presscott trend.
#
# 5) Example:   GDP_gap = (GDP - GDP*) / GDP* 
# 
#              .. GDP_gap is the output gap measured as 
#                 relative deviations from the optimum
#              .. GDP* is the potential product, e.g. from HP-filter approximation
#              .. GDP  is the observed products, usually seasonally adj.
#
#
#
#
#
# Model data:
# Quarterly TS, 2004Q1 - 2012Q4
#
# PPI_GAP_AT  - PPI inflation gap, Austria 
#               (HP Filter and seas. adj. obs. used for calculation)
# U_GAP_AT    - unemployment gap, Austria.
#               (HP Filter and seas. adj. obs. used for calculation)
#
#
#
#
require(zoo)
rm(list=ls())
Austria.data <- read.csv("Phillips_AT.csv")
Austria.ts <- ts(Austria.data, start=c(2004,1), frequency = 4)
AUT.zoo <- zoo(Austria.ts)
#
#
# Basic data plots
par(mfrow = c(2,1))
plot.ts(AUT.zoo[,"PPI_GAP_AT"])
plot.ts(AUT.zoo[,"U_GAP_AT"])
par(mfrow = c(1,1))
#
#
#
#
#
#
# STEP 1: Stationariry evaluation
#
require(tseries) #install.packages("tseries")
# ADF tests for level values
adf.test(AUT.zoo[,"PPI_GAP_AT"], alternative ="stationary", k=1)
adf.test(AUT.zoo[,"U_GAP_AT"], alternative ="stationary", k=1)
# ADF tests for 1st differences 
adf.test(diff(AUT.zoo[,"PPI_GAP_AT"]), alternative ="stationary", k=1)
adf.test(diff(AUT.zoo[,"U_GAP_AT"]), alternative ="stationary", k=1)
# 
#
# For this example, we shall assume that the DGPs for PPI and unemployment gaps
# are stationary - however, our observations are heavily influenced / biased
# by the 2008 crisis.
#
# We create a suitable dummy variable matrix, related to the 2008 crisis, 
# for use in the VAR model specification:
AUT.zoo$dummy <- 0
AUT.zoo$dummy[16:19] <- 1
exog.m <- matrix(AUT.zoo$dummy, nrow = 36, ncol = 1)
colnames(exog.m) <- "exo1"
# The dummy equals 1 for 2007Q4 - 2008Q3. This is an ad-hoc specification.
# .. other specifications are possible
#
#
#
#
#
#
# STEP 2: VAR model estimation and testing:
#
# Granger causality tests
require(lmtest) #install.packages("lmtest")
# max. lag for the test is set ad-hoc:
grangertest(Austria.ts[,"PPI_GAP_AT"], Austria.ts[,"U_GAP_AT"], order=3) 
grangertest(Austria.ts[,"U_GAP_AT"], Austria.ts[,"PPI_GAP_AT"], order=3)  
#
# Maximum lag selection for the VAR model
require(vars) #install.packages("vars")
# We need to specify a dataset containing endogenous variables only
Austria.ts.2 <- Austria.ts[, c("PPI_GAP_AT","U_GAP_AT")]
VARselect(Austria.ts.2, lag.max=6, type="const")
# For better maximum lag selection,
# we may include our dummy variable into the VAR specification as follows:
VARselect(Austria.ts.2, lag.max=6, type="const", exog = exog.m)
# 
# Given the lenght of observed series (36 obs), we use vAR(2) model
varmodel <- VAR(Austria.ts.2, p=2, type="const", exogen = exog.m)
summary(varmodel)
coefficients(varmodel)
dev.off() # resets the plot settings
# Enlarge "Plots" window to full size before running the next plot command!
plot(varmodel)
#
# Tests Autocorrelation in residuals
# H0: Residuals up to lag are NOT autocorrelated
serial.test(varmodel, lags.pt=10, type="BG")
#
# Test for normality of residuals
# H0: Normality of residuals
normality.test(varmodel)
#
# Stability
plot(stability(varmodel, type = "OLS-CUSUM"))
roots(varmodel)
#
#
#
#
#
# STEP 3 - Impulse response functions
#
IRFs <- irf(varmodel, n.ahead = 24) # ortho=TRUE is the default setting
plot(IRFs)
# Cumulative IRFs
CIRFs <- irf(varmodel, n.ahead=24, cumulative=T)
plot(CIRFs)
#
#
#
#
#
#
# STEP 4 - Forecasting from an estimated VAR model
#
?predict
# We need to provide "observations" for the dummy variable,
# for all the "n.ahead" forecast periods.
# Let's say, we want to predict 3 years ahead (12 quarters)
dummy.pred <- matrix(rep(0, 12), nrow = 12)
var.pred <- predict(varmodel, n.ahead = 12, dumvar = dummy.pred)
var.pred # table-form output
plot(var.pred)
fanchart(var.pred)
#
# Forecast error variance decomposition
var.dec<- fevd(varmodel, n.ahead=12)
var.dec
plot(var.dec)
#
#
#
#
#
#
#