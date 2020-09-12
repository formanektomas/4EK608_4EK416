#### Chow test (Chow 1) and Forecasting #### 
#
# 
# 
# 
#
#
### Examples 18.8, 18.9 & 18.10: 
#
# Forecasting the U.S. unemployment rate
#    
#   
#   
#
# Data & basic data manipulation
rm(list=ls())
phillips <- read.csv('phillips.csv') # Read the data
phillips <- phillips[complete.cases(phillips),] # removes the first obs with missing data
trend <- 1:55 # create a  time trend series
phillips <- cbind(phillips, trend)
phillips.ts <- ts(phillips, start = 1949, frequency = 1)
library(zoo)
phillips.zoo <- zoo(phillips.ts)
#
# Model data:
# Annual TS, 1948 - 2003
#
# inf      - percentage change in CPI
# unem     - unemployment rate (in %)
#
#
#
# Basic data plots
plot.ts(phillips.zoo$inf, type = "l" )
lines(phillips.zoo$unem, type = "l", col="red")
legend("topright", legend=c("inf", "unem"), 
       pch=16, col=c("black", "red"))
plot(phillips.zoo$unem ~ phillips.zoo$inf)
plot(phillips.zoo$unem ~ phillips.zoo$unem_1)
#
library(urca)
PO1.test <- ca.po(phillips.zoo[,c("unem","inf_1")], lag="long", type="Pz", demean="constant")
summary(PO1.test) 
#
#
#
# Estimation of equations 18.48 and 18.49,
# training sample: 1949 - 1996
#
#
eq1849 <- lm(unem ~ unem_1 + inf_1, data=phillips.zoo, subset=1:48)
summary(eq1849)
#
### Chow test for structural change in eq1848:
#
library(strucchange) #install.packages("strucchange")
?sctest
# test for observation / line 35, i.e. y 1982
which(phillips.ts[,"obs"]==1982)
phillips.ts[34,]
# 
sctest(unem ~ unem_1 + inf_1, data=phillips.ts,
       type = "Chow", point=34)
# the same is performed for "point = c(1982,1)" ... where ",1" is for frequency=1 (annual data)
sctest(unem ~ unem_1 + inf_1, data=phillips.ts,
       type = "Chow", point=c(1982,1))
#
# test for observation / line 49, i.e. y 1996
sctest(unem ~ unem_1 + inf_1, data=phillips.ts,
       type = "Chow", point=c(1996,1))
#
?Fstats
FS <- Fstats(unem ~ unem_1 + inf_1, data=phillips.ts, from=c(1960,1))
# F-statistics calculated for different breakpoints
FS$Fstats
cbind(FS$Fstats,1960:1991)
# Plots
# https://www.jstatsoft.org/article/view/v007i02
?plot.Fstats
plot(FS) # F-statistics plotted, red "boundary" is not crossed
plot(FS, pval = T) # p-val plotted
#
sctest(unem ~ unem_1 + inf_1, data=phillips.ts,
       type = "Chow", point=c(1981,1))
#
#
#
### Example 18.8 Forecasting the U.S. Unemployment rate
#
# training sample: 1949 - 1996
eq1848 <- lm(unem ~ unem_1, data=phillips.zoo, subset=1:48)
summary(eq1848)
#
# Dynamic forecast are done using loops:
# (1) keep copy of the original matrix
# (2) Create slots (variables) for upper and lower pred. limits
# (3) NA out observations of the dependent variable to be predicted 
# (4) predict 1 step ahead in each iteration
#

Data2.zoo <- phillips.zoo # (1)
Data2.zoo$unem_obs <- Data2.zoo$unem # predictions from 1848
Data2.zoo$lwr_48 <- NA # (2)
Data2.zoo$upr_48 <- NA # (2)
Data2.zoo$unem[49:55] <- NA # (3)
Data2.zoo$unem_1[49:55] <- NA # (3)
for(i in 49:55) {  # (4)
  Data2.zoo$unem_1[i] <- Data2.zoo$unem[i-1]
  Data2.zoo[i, c("unem","lwr_48","upr_48")] <- tail(predict(eq1848, Data2.zoo[48:i,], 
                                                                    interval = "prediction"), 1)
}
Data2.zoo$unem_fcst_48 <- Data2.zoo$unem
#
#
# Repeat the same dynamic prediction (loop based) for equation eq1849
Data2.zoo$lwr_49 <- NA # (2)
Data2.zoo$upr_49 <- NA # (2)
Data2.zoo$unem[49:55] <- NA # (3)
Data2.zoo$unem_1[49:55] <- NA # (3)
for(i in 49:55) {  # (4)
  Data2.zoo$unem_1[i] <- Data2.zoo$unem[i-1]
  # get predictions for the 1849 equation
  Data2.zoo[i, c("unem","lwr_49","upr_49")] <- tail(predict(eq1849, Data2.zoo[48:i,], 
                                                            interval = "prediction"), 1)
}
# Store the forecast for subsequent comparison
Data2.zoo$unem_fcst_49 <- Data2.zoo$unem
#
#
#
#
#
#
### Example 18.9 Out-of-sample comparison of forecasts
# (please note that our results differ from Wooldridge - due to the removed time period)
#
# The two dynamic predictions may be compared using the RMSE:
RMSE_48 <- sqrt(mean((Data2.zoo[49:55,"unem_obs"] - Data2.zoo[49:55,"unem_fcst_48"])^2))
RMSE_49 <- sqrt(mean((Data2.zoo[49:55,"unem_obs"] - Data2.zoo[49:55,"unem_fcst_49"])^2))
RMSE_48
RMSE_49
#
# Also, MAE (mean absolute error) may be used for comparison:
MAE_48 <- mean(abs(Data2.zoo[49:55,"unem_obs"] - Data2.zoo[49:55,"unem_fcst_48"]))
MAE_49 <- mean(abs(Data2.zoo[49:55,"unem_obs"] - Data2.zoo[49:55,"unem_fcst_49"]))
MAE_48
MAE_49
#
#
#
# Also, we may plot the actual & forecasted data, along with forecasts' confidence limits:
plot.ts(Data2.zoo[47:55,"unem_obs"], ylab="unemployment forecasts and observations", ylim=c(2,8), lwd=2)
lines(Data2.zoo[47:55,"unem_fcst_48"],  col = 2, lwd = 1, lty = 1) # lower limit of prediction
lines(Data2.zoo[47:55,"lwr_48"],  col = 2, lwd = 1, lty = 3) # lower limit of prediction
lines(Data2.zoo[47:55,"upr_48"],  col = 2, lwd = 1, lty = 3) # lower limit of prediction
lines(Data2.zoo[47:55,"unem_fcst_49"],  col = "green", lwd = 1, lty = 1) # lower limit of prediction
lines(Data2.zoo[47:55,"lwr_49"],  col = "green", lwd = 1, lty = 3) # lower limit of prediction
lines(Data2.zoo[47:55,"upr_49"],  col = "green", lwd = 1, lty = 3) # lower limit of prediction
lines(Data2.zoo[47:55,"unem_obs"], lwd=2)
abline(v=1996)
legend("topright", legend=c("Observed", "eq1848 FCST", "eq1849 FCST"), 
       pch=16, col=c("black", "red", "green"))
#
#
#
#
#
#
### Example 18.10 Two-year-ahead forecast
#
# Within the above predictions from equation 1849, we used actual observations of inf(t-1) 
# for the prediction period (dynamic nature of the prediction: we used past predictions of
# the endogenous variable as lagged "observations" for prediction in the next period).
#
# Now, we assume that no information on both "unem" and "inf" is available past 1996.
# Therefore, as a first step in predicting unem for 1997-2003, we need to make 1997-2002 
# predictions for inf, based on the 1948-1996 data available.
# Hence, in this example, we shall simulate an ex-ante prediction.
#
# A simple AR(1) type regression for inf may be used as follows:
inf.eq <- lm(inf ~ inf_1, data=phillips.zoo, subset=1:48) 
summary(inf.eq)
#
# Now, we make dynamic forecasts of "inf" for the period 1997-2003,
# subsequently we use this predicted "inf" to predict "unemp" from equation 1849:
Data3.zoo <- phillips.zoo # (1)
Data3.zoo$unem_obs <- Data3.zoo$unem # for subsequent predictions from 1848
Data3.zoo$inf_obs <- Data3.zoo$inf
Data3.zoo$unem[49:55] <- NA # (3)
Data3.zoo$unem_1[49:55] <- NA # (3)
Data3.zoo$inf[49:55] <- NA # (3)
Data3.zoo$inf_1[49:55] <- NA # (3)
for(i in 49:55) {  # (4)
  Data3.zoo$inf_1[i] <- Data3.zoo$inf[i-1]
  Data3.zoo[i, "inf"] <- tail(predict(inf.eq, Data3.zoo[49:i,]), 1)
}
#
# Once we have the regressors, necessary for the prediction period, we may 
# use prediction function for unem (eq1849)
for(i in 49:55) {
  # get predictions for the 1849 equation, using predicted inf values.
  Data3.zoo$unem_1[i] <- Data3.zoo$unem[i-1]
  Data3.zoo[i, "unem"] <- tail(predict(eq1849, newdata=Data3.zoo[49:i,]), 1)
}
#
# The forecasts for 1997 and 1998 (as per Wooldridge):
round(Data3.zoo$unem[49:50], 2)
#
#
#
#
#
#
#
#