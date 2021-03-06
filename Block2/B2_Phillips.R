#### Repetition: repetition of expectations augmented Phillips curve, 
#### adaptive expectations, testing of serial correlation, 
#### FGLS in the case of serial correlation, 
#### Cochrane-Orcutt and Prais-Winsten transformation #### 
#
# 
# 
# 
#
#
### Example 10.1: Static Phillips Curve 
#    
#   
#   
#
# Data
rm(list=ls())
phillips <- read.csv('phillips.csv') # Read the data

#
# Model data:
# Annual TS, 1948 - 2003
#
# inf      - percentage change in CPI
# unem     - civilian unemployment rate, %
# 
#
#
#
# Basic data plots
phillips$time <- 1:56 # time trend
plot(phillips$inf ~ phillips$obs, type = "l" )
plot(phillips$inf ~ phillips$unem)
#
#
class(phillips)
# 
phillips.ts <- ts(phillips, start = 1948, frequency = 1)
library(zoo) # install.packages(zoo)
phillips.zoo <- zoo(phillips.ts) 
head(phillips.zoo)
str(phillips.zoo)
#
# Estimation of the static model 
lm.10.14 <- lm( inf ~ unem, data=phillips.zoo)
summary(lm.10.14) # Results suggest no tradeoff between inflation and unemployment...
#
library(lmtest) # install.packages("lmtest")
dwtest(lm.10.14, alternative = "two.sided") # Durbin Watson test 
bgtest(lm.10.14, order=1) # Breusch-Godfrey test 
#
#
# Neglected nonstationarity in series results in autocorrelation,
# st.errs, t-ratios, F-test and R^2 estimates are unreliable
# .. one simple approach is to use first differences for the estimation:
lm.10.14.dif <- lm( diff(inf) ~ diff(unem), data=phillips.zoo)
summary(lm.10.14.dif)
bgtest(lm.10.14.dif, order=1)
#
#
#
### Example 11.5: Expectations augmented Phillips Curve 
# Simplified adaptive expectations: Expected(inf[t])  = Actual(inf[t-1])
#
# Estimation of model as per equation 11.19
phillips.zoo$d_inf <- diff(phillips.zoo$inf)
head(phillips.zoo)
# We use data up to 1996 only fo the estimation:
lm.11.19 <-lm( d_inf ~ unem, data = phillips.zoo, subset=1:49 )
summary(lm.11.19) # note the deletion due to missigness for row 1...
#
# The natural rate of unemployment may be calculated as
# mu = beta0 / (- beta1)
lm.11.19$coefficients[1]/(-1*lm.11.19$coefficients[2])
#
# Reliability of the estimated natural rate of unemployment
library(RcmdrMisc)
DeltaMethod(lm.11.19,"b0/-b1")
#
#
#
### Example 12.1: ar(1) autocorrelation tests and rho estimations 
#
#
# ar(1) test and rho estimation for the Static Phillips Curve
summary(lm.10.14) # Estimation of the Static Phillips Curve
bgtest(lm.10.14, order=1) # Breusch Godfrey test
# rho estimation
phillips.zoo$resids <- lm.10.14$residuals 
phillips.zoo$resids_1 <- lag(phillips.zoo$resids, -1) # Note the "-1" syntax for t-1...
summary(lm(resids ~ resids_1 -1 , data = phillips.zoo))
#
#
## Assignment 1
## Estimate rho in Expectations Augmented Phillips Curve
## .. i.e. use residuals from model lm.11.19, 
## .. note the syntax amendment for shorter series used in lm.11.19:
phillips.zoo$resids2 <- c(NA,lm.11.19$residuals, rep(NA,7) )
head(phillips.zoo,5)
tail(phillips.zoo,10)
## Provide the code for "manual" rho estimation 
#
#
#
#
#
#
#
### Example 12.5: Prais-Winsten estimation (transformation) of the Static Phillips Curve
#
# We use data through 1996 only (as per Wooldridge)
#
# Static model output repeated
summary(lm.10.14) 
#
library(prais) # install.packages("prais")
prais_winsten(lm.10.14, data=phillips.zoo[2:49,] )
summary(prais_winsten(lm.10.14, data=phillips.zoo[2:49,] ))
#
#
# The same FGLS estimation, using Cochrane Orcutt transformation
library(orcutt) # install.packages("orcutt")
lm.10.14.2 <- lm( inf ~ unem, data=phillips.zoo, subset = 2:49)
summary(cochrane.orcutt(lm.10.14.2))
#
## Assigment 2
## Compare the coefficients on "unem", rho and the degrees of freedom for the F-tests
## in PW and CO transformations/estimations of the model lm.10.14.
#
#
#
#