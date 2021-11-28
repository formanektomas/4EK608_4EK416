#### VECM model estimation and IRFs ####
#
#
#
#
# Model data:
# Quarterly TS, 2001Q1 - 2010Q3
#
# GDP_EU      - EU real GDP, base index, 2005 = 100, seasonaly adjsuted.
# GDP_DEF_EU  - EU inflation measured by GDP deflator, base index, 2005 = 100, seas. adj.
# .. measurements are consistent with respect to new EU members
#
#
#
# Basic data handling
rm(list=ls())
EU.df <- read.csv("EUmacrodata.csv")
EU.ts <- ts(EU.df, start=c(2001,1), frequency = 4)
#
plot.ts(EU.ts[,"GDP_EU"], ylim=c(85,110), lwd=2)
lines(EU.ts[,"GDP_DEF_EU"], col="green", lwd=2)
legend("topleft", legend=c("EU GDP", "EU GDP Deflator"), 
       pch=16, col=c("black", "green"))
#
#
#
#
#
### VECM estimation
#
# STEP 1: (non)stationariry & cointegration of the series
#
library(tseries) #install.packages("tseries")
# ADF tests for level values
adf.test(EU.ts[,"GDP_EU"], alternative ="stationary", k=1)
adf.test(EU.ts[,"GDP_DEF_EU"], alternative ="stationary", k=1)
# Test for stationariry of the differenced series /1st diffs/
adf.test(diff(EU.ts[,"GDP_EU"], lag=1), alternative ="stationary", k=1)
adf.test(diff(EU.ts[,"GDP_DEF_EU"], lag=1), alternative ="stationary", k=1) 
# 1st diff of GDP_DEF_EU may be considered stationary at alpha = 10%
#  
#
library(urca) #install.packages("urca")
# Make a matrix with endogenous series only
# .. for use in both Johansen cointegration test and for VECM estimation
endog.ts <- EU.ts[, c("GDP_EU", "GDP_DEF_EU")]
# Johansen cointegration test
EU_Jo <- ca.jo(endog.ts, type = "eigen", ecdet = "const", spec="longrun", K = 4)
summary(EU_Jo) 
# .. we reject H0 of " r = 0 "
# .. we do not reject H0 of " r <= 1 "
# 
?ca.jo
# Given the "spike" in GDP series, related to the 2008 crisis onset,
# it may be useful/convenient to extend our "model" i.e. the tested
# cointegration specification using a dummy variable.
# Note:
# .. When looking for the "best" dummy variable specification,
# .. there is no single "correct" answer.
# .. In the following example, we use a dummy that equals 1 on 2007Q4
# .. and zero otherwise. Students are welcome to suggest alternatives.
exog.m <- matrix(EU.ts[,"D2007Q4"], nrow = 39, ncol = 1)
colnames(exog.m) <- "exo1"
# Johansen cointegration test, one-off shock controlled using "dumvar" argument
EU_Jo.dummy <- ca.jo(endog.ts, type = "eigen", ecdet = "const", spec="longrun", 
                     K = 4, dumvar = exog.m)
summary(EU_Jo.dummy)
#
#
#
# STEP 2: VECM estimation
#
# First, we estimate a VECM model for the specification WITHOUT the D2007Q4 dummy.
?cajorls # provides a VECM estimation
VECM_EU <- cajorls(EU_Jo, r=1)
# Cointegrating equation
VECM_EU$beta # Cointegrating (long term) relation
# VECM coefficients
summary(VECM_EU$rlm) # coefficients of the VECM equations
#
# VECM model, with D2007Q4 dummy (exogenous variable) included 
VECM_EU.dummy <- cajorls(EU_Jo.dummy, r=1)
VECM_EU.dummy$beta
summary(VECM_EU.dummy$rlm)
#
#
#
# STEP 3: IRFs from the VECM
#
# Before IRF estimation, we need to transform the estimated object
# into a "VAR" specification, suitable for IRF calculation
library(vars) # install.packages("vars")
vec2var.obj <- vec2var(EU_Jo.dummy)
# Plot IRFs without saving them to the Global environment
plot(irf(vec2var.obj))
# Cumulative IRFs: save object to the Global environment & show the plot
c.irfs <- irf(vec2var.obj, cumulative = T)
plot(c.irfs)
#
#
#
# STEP 4: Predictions from the VECM
#
# The prediction is based on the "vec2var" object, as described in Step 3
# observations for exogenous variables for the prediction period must be provided!
exog.pred <- matrix(rep(0,12), nrow = 12)
colnames(exog.pred) <- "exo1" # column names need to match the VECM 
prediction.VECM <- predict(vec2var.obj, n.ahead = 12, dumvar = exog.pred)
plot(prediction.VECM)
fanchart(prediction.VECM)
#
#
#
#
#
#
#### A maximum likelihood VECM estimation may also be performed using {tsDyn}
#
library(tsDyn) # install.packages("tsDyn")
?VECM
# both "endog.ts" and "exog" have been declared above
VECM.mod <- VECM(endog.ts, lag = 3, r = 1, include = "both", estim="2OLS")
summary(VECM.mod)
# Prediction from the VECM model; 5 years ahead
pred.VECM <- predict(VECM.mod, n.ahead=20)
pred.VECM
# IRFs from the VECM model
irfs.VECM <- irf(VECM.mod, n.ahead = 20)
plot(irfs.VECM)
# Cumulative IRFs from the VECM model
c.irfs.VECM <- irf(VECM.mod, n.ahead = 20, cumulative = T)
plot(c.irfs.VECM)
#
#
#
#
#
#
