#### SEMs - Klein I #### 
#
# 
# 
# 
#
#
### Estimation of the Klein (1950) model "Model I"
#    
#   
#   
#
# Data
rm(list=ls())
library(systemfit) # install.packages("systemfit")
# Load sample data for the Klein I model from package {systemfit}
data("KleinI")
View(KleinI)
# Data description as provided in package {systemfit}
?KleinI
# 
#
#
#
# Basic data plots
plot(KleinI)
# 
#
#
#
# Model specification - basic (no identity restrictions)
#
# (1) Define equations
eqConsump <- consump ~ corpProf + corpProfLag + wages
eqInvest <- invest ~ corpProf + corpProfLag + capitalLag
eqPrivWage <- privWage ~ gnp + gnpLag + trend
# (2) Define instruments (all exogenous variables serve as instruments)
instr <- ~ govExp + taxes + govWage + trend + capitalLag + corpProfLag + gnpLag
# (3) Combine equations into a system of equations
system <- list(Consumption = eqConsump, Investment = eqInvest, 
               PrivateWages = eqPrivWage)
# 
#
#
#
# Model estimation
#
# (a) The model is estimated using OLS (interdependencies are ignored)
#
kleinOls <- systemfit(system, method="OLS", data = KleinI)
summary(kleinOls)
# Print a compact model output
round(coef(summary(kleinOls)), digits = 3)
#
# (b) 2SLS estimation:
klein2sls <- systemfit(system, method = "2SLS", inst = instr, data = KleinI)
summary(klein2sls)
# Print a compact model output
round(coef(summary(klein2sls)), digits = 3)
#
# (b) 3SLS estimation:
klein3sls <- systemfit(system, method = "3SLS", inst = instr, data = KleinI,
                       method3sls = "GMM")
summary(klein3sls)
# Print a compact model output
round(coef(summary(klein3sls)), digits = 3)
# Compare OLS and 2SLS estimated models using BIC
BIC(kleinOls)
BIC(klein2sls)
BIC(klein3sls)
#
#
#
#
# Fitted values and limits for the 2SLS-estimated model
?predict.systemfit
predict( klein2sls , interval = "confidence" ) # predicting average values (not indiv. obs.)
#
# Fitted values of the first equation
predict( klein2sls$eq[[1]], interval = "confidence" )
#
# Fitted  values of the second equation
predict( klein2sls$eq[[2]], interval = "confidence" )
#
# Fitted values of the third equation
predict( klein2sls$eq[[3]], interval = "confidence" )
#
# Look at the structure of the object generated on line 86:
str(predict( klein2sls$eq[[3]], interval = "confidence" ))
# 
#
# Plot of observed vs fitted consumpiton 
# (along with 95% confidence intervals for fitted values)
plot(KleinI$consump ~ KleinI$year, type="l", ylim=c(35,70), 
     main = "EQ1 - Consumption", ylab="Consumption", xlab="time", lwd =2)
lines(predict( klein2sls$eq[[1]], interval = "confidence" )$fit ~ KleinI$year, 
      col = "red")
lines(predict( klein2sls$eq[[1]], interval = "confidence" )$lwr ~ KleinI$year, 
      col = "red", lty=2)
lines(predict( klein2sls$eq[[1]], interval = "confidence" )$upr ~ KleinI$year, 
      col = "red", lty=2)
#
#
#
## Assignment 1
## Prepare a plot of the "invest" variable and compare it to its
## fitted values generated from our model (2sls estimation).
## Hint: Invest is the dependent variable from the second equation of our system,
##       .. copy+paste lines 94-101 & make amendments where necessary.
##
## Bonus assignment: For comparison, include in the same plot the interval predictions
##                   of "invest", as generated from the 3sls estimation of our model
##                   (use green color for the 3sls-based predictions)
#
#
#
#