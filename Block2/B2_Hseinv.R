#### Distributed lag (FDL and IDL) and Rational DL models #### 
#
# 
# 
# 
#
#
### Example 18.1: Housing investment and residential price inflation 
#    
#   
#   
#
# Data
library(zoo)
rm(list=ls())
hseinv <- read.csv('hseinv.csv') # Read the data
hseinv <- zoo(hseinv, order.by = hseinv$obs) # one-step zoo conversion
# hseinv.ts <- ts(hseinv, start = 1947, frequency = 1)
# hseinv <- zoo(hseinv.ts)
# 
#
#
# Model data:
# Annual TS, 1947 - 1988
#
# linvpc    - log of per capita invest, 
#             (based on real housing investment in millions USD / population)
# gprice    - log( price(t) / price(t-1) )
#             (based on a housing prices index)
# time     - time trend
#
#
#
# Basic data plots
plot(hseinv$linvpc ~ hseinv$gprice)
#
# Step 1: We need to de-trend linvpc, log(invpc)
# i.e. we want the residuals from a regression of linvpc on a time trend:
de.trend <- lm(log(invpc) ~ time, data = hseinv)
hseinv$dlinvpc <- de.trend$residuals 
#
# Estimation of a finite distributed lags model, max. lag at 3 years
hseinv$gprice_1 <- lag(hseinv$gprice, -1)
hseinv$gprice_2 <- lag(hseinv$gprice, -2)
hseinv$gprice_3 <- lag(hseinv$gprice, -3)
#
#
FDL.model <- lm(dlinvpc ~ gprice + gprice_1 + gprice_2 + gprice_3, data=hseinv)
summary(FDL.model)
library(lmtest) # install.packages("lmtest")
bgtest(FDL.model, order=1)
library(car) # install.packages("car") # Companion to Applied Regression
vif(FDL.model)
#
#
#
# Geometrically distributed lags (Koyck transformation)
hseinv$dlinvpc_1 <- lag(hseinv$dlinvpc, -1)
#
Koyck <- lm( dlinvpc ~ gprice + dlinvpc_1, data=hseinv)
summary(Koyck)
bgtest(Koyck, order=1) # H0: residuals are not autocorrelated
#
#
# Long Run Propensity
# .. i.e. the effect of a permanent 1-unit increase in a regressor
LRP <- Koyck$coefficients[2] / (1 - Koyck$coefficients[3])
LRP # this result seems economically implausible.
#
#
# Lag distribution plot
# .. the effect of a transitory (one-off) unit change in a regressor
# .. impact propensity (immediate effect) to time t+5
#
#
plot.lags <- function() {
  prop.df <- data.frame(0:5) # lag 0 to 5, a data frame with 6 rows is created
  colnames(prop.df) <- "lag"
  prop.df$coeff <- 0 
  for (ii in 0:5) {          # lag 0 to 5
    prop.df[ii+1,"coeff"] <- Koyck$coefficients[2] * Koyck$coefficients[3]^ii
  }
  plot(prop.df$coeff ~ prop.df$lag, type = "b", xlab = "lag", ylab = "coefficient", col="red")
  print(prop.df)
}
#
plot.lags() # Calls the function
#
#
## Assignment 1
## Change the code in plot.lags, as follows:
##   1) Increase the maximum lag to 10.
##
##      Bonus task: Change the function so that it takes one argument: maxlag, 
##                  which sets the desired maximum lag order to be plotted,
##                  set default maxlag value to 6
##               
#
#
#
#
#
#
#
#
# Rational distributed lags model 
RDL.model <- lm(dlinvpc ~ gprice + gprice_1 + dlinvpc_1, data=hseinv)
summary(RDL.model)
bgtest(RDL.model, order=1)
#
# Long Run Propensity in the RDL model
LRP.RDL <- (RDL.model$coefficients[2]+RDL.model$coefficients[3])/(1-RDL.model$coefficients[4])
LRP.RDL
#
# 
# Lag distribution plot
plot.lag.rdl <- function() {
  prop.df <- data.frame(0:6) # lag 0 to 6
  colnames(prop.df) <- "lag"
  prop.df$coeff <- 0 
  prop.df[1, "coeff"] <- RDL.model$coefficients[2] # lag 0
  for (ii in 1:6) {          # lags 1 to 5...
    prop.df[ii+1,"coeff"] <- RDL.model$coefficients[4]^(ii-1)*(RDL.model$coefficients[4]*RDL.model$coefficients[2] + RDL.model$coefficients[3])
  }
  plot(prop.df$coeff ~ prop.df$lag, type = "b", xlab = "lag", ylab = "coefficient", col = "red")
  print(prop.df)
  abline(h=0) # Adds horizontal line at 0 (y-axis) for better interpretation
}
plot.lag.rdl() # Calls the function
#
#
## Assignment 2
## What is the expected effect at lag 10?
#
#
#
# Statistical significance of the LRP in RDL model
library(RcmdrMisc)
DeltaMethod(RDL.model,"(b1+b2)/(1-b3)")
#
#