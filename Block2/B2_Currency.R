#### Forecasting example
#    
#
#
#
# Model data:
# Monthly TS, 1917M09 - 2005M04
#
# g      - growth rate of currency in the hands of general public
# 
#
#
#
#
# Data & dataset preparation
rm(list=ls())
library(zoo) # install.packages("zoo")
load("currency.Rdata")
currency[,"Trend"] <- 1:nrow(currency)
#
summary(currency)
head(currency, 14)
#
#### Basic data plots
# plot.ts(currency$G)
# plot(currency$G ~ currency$G_1)
library(ggplot2) #install.packages("ggplot2)
autoplot.zoo(currency$G) # ggplot2 has zoo-specific tools
#
ggplot(currency, aes(x=G_1,y=G) ) + 
  geom_point(colour="orange") +
  geom_smooth(method= "lm") +
  ggtitle("G_t ~ G_t-1") 
#
#
#
library(forecast) #install.packages("forecast")
?ndiffs
ndiffs(currency$G, test = "adf")
#
# Potentially seasonal data, the following seasonal test does not work with NA values
which(is.na(currency$G) == T)
# 
nsdiffs(currency$G[197:995], m=frequency(currency$G[197:995]), test = "ocsb")
# the above test is outside the scope of our course, see:
#  http://users.metu.edu.tr/ceylan/STAT%20497_LN6.ppt
#
#
# Hence, we may conclude that our series is stationary..
#
#
#
######## STEP 1 - Fit a simple OLS model,
#
# A simple autorregression:
summary(lm(G ~ G_1, data = currency))
#
# using the following setup: G(t) <- f{ G(t-1), trend, monthly dummies }
G.fit <- lm(G ~ G_1 + Trend +M1+M2+M3+M4+M5+M6
            +M7+M8+M9+M10+M11+M12 -1, data = currency)
summary(G.fit)
#
#
#
#
#
# STEP 2 - Static forecasts
# 1-step ahead "static" forecasts 
#     observed lagged dependent values are always used, past predictions are ignored
#     in subsequent prediction periods.
#
#     Suitable for ex-post predictions only.
#
#
#  We estimate the above equation "G.fit" for the period up to 2000:M12, 
#  then we forecast 2001:M1 onwards
#
# First, we have to find out, which rows in the dataset correspond to 1956:M01 and 2000:M12 
# .. to be used in the subset argument for lm()...
est.start <- which(currency$Year==1956 & currency$Month == 1)
est.end <- which(currency$Year==2000 & currency$Month==12)
# 
G.fit.r <- lm(G ~ G_1 + Trend +M1+M2+M3+M4+M5+M6+M7+M8+M9+M10+M11+M12 -1,
              data = currency, subset = est.start:est.end)
summary(G.fit.r)
# 
static.prediction <- predict(G.fit.r, newdata = currency,  interval = "confidence")
# interval = "confidence"
# Reflects the uncertainty (confidence interval) related to
# "average" predictions. Here, we ignore the variance of
# residuals - Variance of estimated regression coefficients is taken into account only
# "confidence" intervals are usually narrower than "prediction" intervals.
static.prediction[c(1009:1065), ]
# Static forecasts are only produced if G_1 is observed...
#
#
# We prepare a plot, comparing observed values vs. static predictions
pred.plot.data <- cbind(currency[,1:4], static.prediction)
colnames(pred.plot.data) <- c("G", "Trend", "Year", "Month", "Fitted", "lwr", "upr")
pred.plot.data <- pred.plot.data[1009:1068,] # 2000:M1 - 2005:M12
#
plot.ts(pred.plot.data[,"upr"],  col = 2, lwd = 1, lty = 3,
        ylab="G forecasts and observations") #upper confidence limit of prediction
lines(pred.plot.data[,"lwr"],  col = 2, lwd = 1, lty = 3) # lower limit of prediction
lines(pred.plot.data[,"Fitted"],  col = 1, lwd = 2, lty = 1) # prediction
lines(pred.plot.data[,"G"],  col = "green", lwd = 1, lty = 1) # observed values
legend("topright", legend=c("Observed G", "Fitted"), pch=16, col=c("green", "black"))
# Fitted values "overshoot" the seasonal dip in Month 1
#
# Manual prediction evaluation, RMSE
MSE <- mean((pred.plot.data$G - pred.plot.data$Fitted)^2, na.rm=T)
MSE
RMSE <- sqrt(MSE) # Root mean squared error
RMSE
#
#
#
# STEP 3 - Dynamic forecasts from the equation "G.fit.r"
#          .. estimation period: 1917:M09 - 2000:M12
#          .. prediction period: 2001:M01 - 2007:M12
#
#
#   Dynamic forecasts:
#      observed lagged dependent values are ignored, past predictions are used
#      as regressors in predictions for subsequent periods..
#
#      Suitable for ex-post & ex-ante predictions
#   
# 
## Note:
# Currently (02/2017), Dynamic forecast in R may be done using loops:
# (1) keep copy of the original matrix
# (2) Create slots (variables) for upper and lower pred. limits
# (3) NA out observations of the dependent variable to be predicted 
# (4) predict 1 step ahead in each iteration
#
Data2.zoo <- currency # (1)
Data2.zoo$G_obs <- Data2.zoo$G # original obs., for reference 
Data2.zoo$lwr <- NA # (2) 
Data2.zoo$upr <- NA # (2) 
Data2.zoo$Trend[1061:1092] <- 1053:1084 # extend time trend to 2007:M12
Data2.zoo$G[1009:1092] <- NA # (3)
Data2.zoo$G_1[1009:1092] <- NA # (3)
#
for(i in 1009:1092) {   # (4)
  Data2.zoo$G_1[i] <- Data2.zoo$G[i-1] # update the G(t-1) observation
  Data2.zoo[i, c("G","lwr","upr")] <- predict(G.fit.r, newdata = Data2.zoo[i,], 
                                                   interval = "confidence")
}
tail(Data2.zoo[,c(1:4,18:20)], 100)
#
plot(Data2.zoo[1009:1092,"upr"],  col = 2, lwd = 1, lty = 3,
        ylab="G forecasts and observations") #upper confidence limit of prediction
lines(Data2.zoo[1009:1092,"lwr"],  col = 2, lwd = 1, lty = 3) # lower limit of prediction
lines(Data2.zoo[1009:1092,"G"],  col = 1, lwd = 2, lty = 1) # prediction
lines(Data2.zoo[1009:1092,"G_obs"],  col = "green", lwd = 1, lty = 1) # observed values
legend("topright", legend=c("Observed G", "Fitted"), pch=16, col=c("green", "black"))
#
# Manual prediction evaluation, RMSE
MSE.dyn <- mean((Data2.zoo[1009:1068,"G_obs"] - Data2.zoo[1009:1068,"G"])^2, na.rm=T)
MSE.dyn
RMSE.dyn <- sqrt(MSE.dyn) # Root mean squared error
RMSE.dyn
#
RMSE # Static forecasts are usually closer to the actual observations of the dependent variable
#
#
#
### Dynamic Forecasts may also be performed using {forecast} package
### and Arima() command. 
### Pleas not that the "ML" estimation method will be discussed later in the course.
#
library(forecast)
library(lmtest) # install.packages("lmtest")
?Arima
# ARIMA (1,0,0); I=0, 
# we want trend and seasonal dummies included explicitly as a regressors
# exogenous variables have to be declared separately
xreg.df <- currency[est.start:est.end,c(2,5:15)]
head(xreg.df)
Arima.obj <- Arima(currency$G[est.start:est.end], order=c(1,0,0), xreg = xreg.df,
      method = "ML")
summary(Arima.obj)
coeftest(Arima.obj)
#
# xreg for the forecasting period (exogenous regressors need to be provided)
fxreg.df <- xreg.df <- currency[1009:1092,c(2,5:15)]
?forecast.ar
# Forecast with confidence intervals
plot(forecast(Arima.obj, level=0.95, xreg=fxreg.df), xlim=c(2001, 2007))
lines(currency[1009:1092,"G"],  col = "red", lwd = 2, lty = 1) # observed values
# 
# Simulated paths 
?simulate.Arima
set.seed(112)
for(i in 1:10) {
  lines(simulate(Arima.obj, nsim=20, xreg=fxreg.df, bootstrap = T), col="darkgreen")
}
#
#
#