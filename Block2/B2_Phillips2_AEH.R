#### Adaptive expectations hypothesis (AEH) #### 
#
# 
# 
# 
#
#
### Example 11.5: Expectations augmented Phillips Curve (extended)
#    
#   
#   
#
# Data
#
# Model data:
# Annual TS, 1948 - 2003
#
# inf      - percentage change in CPI
# unem     - civilian unemployment rate, %
# 
#
rm(list=ls())
phillips <- read.csv('phillips.csv') # Read the data
phillips.ts <- ts(phillips, start = 1948, frequency = 1)
#
#
#
# Basic data plots
plot.ts(phillips.ts[,"inf"], type = "l" , ylab = "y-o-y percentage changes")
lines(phillips.ts[,"unem"], type = "l", col = "blue" )
legend("topright", c("inf", "unem"), 
       col = c("black", "blue"), lty = 1, inset = .02)
#
#
#
# AEH-based  estimation setup is analogous to Koyck transformation:
#
library(dyn) # install.packages("dyn")
AEH.mod <- dyn$lm(inf ~ unem + lag(inf, -1), data = phillips.ts)
summary(AEH.mod)
library(lmtest) # install.packages("lmtest")
bgtest(AEH.mod, order = 1)
bgtest(AEH.mod, order = 2)
#
# \phi coefficient of adaptation
phi <- 1-AEH.mod$coefficients[3]
phi
# Alpha
Alpha <- AEH.mod$coefficients[1]/phi
Alpha
# Beta
Beta <- AEH.mod$coefficients[2]/phi
Beta
# 
#
# Fitted values from the AEH model
#
# STEP 1 Start with the Xstar (expected unem values generated using AEH)
#
# .. we need to include one additional assumption: , for Xstar(t=0)
library(zoo) # install.packages("zoo") 
phillips.zoo <- zoo(phillips.ts) # easier ts data manipulation...
#
Xstar <- rep(NA, 56)
Xstar[1] <- 3.5 # The ad-hoc chosen expectation at t=0 (1948)
# Calculate Xstar values (i.e. AEH-expected levels of unemployment)
for (ii in 2:56) {
  Xstar[ii] <- phi*phillips.zoo[ii, "unem"] + (1-phi)*Xstar[ii-1]
}
phillips.zoo$Xstar <- Xstar
plot.ts(phillips.zoo$unem)
lines(phillips.zoo$Xstar, col = "blue",lwd=4)
#
## Assignment 1
## Before running the code below, try to answer the following questions:
## How much do our Xstar results depend on the initial setting of Xstar on line 97 (year 1948)?
## What happens, if we set the initial Xstar to, say, 8?
#
Xstar2 <- rep(NA, 56)
Xstar2[1] <- 8 # The ad-hoc chosen expectation at t=0 (1948)
# Calculate Xstar values (i.e. AEH-expected levels of unemployment)
for (ii in 2:56) {
  Xstar2[ii] <- phi*phillips.zoo[ii, "unem"] + (1-phi)*Xstar2[ii-1]
}
phillips.zoo$Xstar2 <- Xstar2
plot.ts(phillips.zoo$unem) # observed unemployment data
lines(phillips.zoo$Xstar, col = "blue", lwd=4)
lines(phillips.zoo$Xstar2, col = "red", lwd=2)
#
# Differences between expected unem values  
# caused by different t=0 values.
phillips.zoo$Xstar2-phillips.zoo$Xstar 
plot.ts(phillips.zoo$Xstar2-phillips.zoo$Xstar)
#
#
# STEP 2 "Fitted" values - expected values of the endogenous variable
# under the AEH hypothesis
#
phillips.zoo$Fit.AEH <- Alpha + Beta*phillips.zoo$Xstar
#
plot.ts(phillips.zoo$inf) # observed inflation data
lines(phillips.zoo$Fit.AEH, col = "red")
#
#
## Given the plots of actual unem & inf observations vs AEH-based expectations,
## how would you evaluate the AEH validity for our dataset?
#
#
#
#
#