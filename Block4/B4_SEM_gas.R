#### Identification in SEMs ####
#
# install.packages("AER") 
# install.packages("systemfit")
library("AER") # Applied Econometrics with R, for estimating IV models
library("systemfit")
?systemfit # Read the documentation before using this command.
#
#
rm(list=ls())
gas <- read.csv("gas.csv")
gas <- gas[complete.cases(gas) == T, ] # complete cases analysis
#
# Model data:
#
# Annual data, 1960 - 1995
#
# G        - total US gasoline consumption
# d_G      - first differences of G
# d_G_1    - lag of d_G
# Pg       - price index of gasoline
# d_Pg     - 1st differences of Pg
# d_Y      - per capita disposable income (first differences)
#
plot(gas$Pg ~ gas$obs, type="l")  # price index of gasoline
plot(gas$d_Pg ~ gas$obs, type="l") # 1st differences of Pg
#
plot(gas$G ~ gas$obs, type="l") # gasoline consumption
plot(gas$d_G ~ gas$obs, type="l") # first difference of G
#
#
### Unidentified equations - illustration 1
#
# Estimation through the {systemfit} package
Consump <- d_G ~ d_Pg # define equation 1
Price <- d_Pg ~ d_G # define equation 2
system <- list(GasCons = Consump, GasPrice = Price) # make a SEM
#
# First, the model is estimated using OLS 
# (interdependencies are ignored)
#
model1 <- systemfit(system, method="OLS", data = gas)
summary(model1)
# A condensed view of the coefficients:
round(coef(summary(model1)), digits = 3)
#
# Now, lets try 2SLS
model1 <- systemfit(system, method="2SLS", data = gas)
# 
#
#
#
### Unidentified equations - illustration 2
#
Consump2 <- d_G ~ d_Pg + d_G_1 # define equation 1
Price2 <- d_Pg ~ d_G # define equation 2
instr <- ~ d_G_1 # list of instruments
system2 <- list(GasCons = Consump2, GasPrice = Price2) # make a SEM
#
# OLS 
model2 <- systemfit(system2, method="OLS", data = gas)
round(coef(summary(model2)), digits = 3)
#
# 2SLS
model2 <- systemfit(system2, method="2SLS",  inst = instr, data = gas)
summary(model2)
round(coef(summary(model2)), digits = 3)
# The first equation is not identified.
# This may be illustrated using the ivreg function from {AER}:
#
IVR.e1 <- ivreg( d_G ~ d_Pg + d_G_1 | d_G_1, data=gas)
# whereas, the second equation is identified:
IVR.e2 <- ivreg( d_Pg ~ d_G | d_G_1, data=gas)
summary(IVR.e2, vcov = sandwich, diagnostics=T)
# compare with
round(coef(summary(model2)), digits = 4)
# 
#
#
#
### Just identified equations - illustration 3
#
Consump3 <- d_G ~ d_Pg + d_G_1 # define equation 1
Price3 <- d_Pg ~ d_G + d_Y # define equation 2
instr3 <- ~ d_G_1 + d_Y # list of instruments
system3 <- list(GasCons = Consump3, GasPrice = Price3) # make a SEM
#
# OLS 
model3 <- systemfit(system3, method="OLS", data = gas)
round(coef(summary(model3)), digits = 3)
#
# 2SLS
model3 <- systemfit(system3, method="2SLS",  inst = instr3, data = gas)
summary(model3)
round(coef(summary(model3)), digits = 3)
# Results may be replicated using the ivreg function from {AER}:
#
IVR2.e1 <- ivreg( d_G ~ d_Pg + d_G_1 | d_G_1 + d_Y, data=gas)
coef(summary(IVR2.e1, vcov = sandwich, diagnostics=T))
# and
IVR2.e2 <- ivreg( d_Pg ~ d_G + d_Y | d_G_1 + d_Y, data=gas)
coef(summary(IVR2.e2, diagnostics=T))
# compare with
round(coef(summary(model3)), digits = 6)
# 
#
#
#
### Over-identified equations - illustration 4
#
Consump4 <- d_G ~ d_Pg + d_G_1  # define equation 1
Price4 <- d_Pg ~ d_G + d_Y + d_Pop # define equation 2
instr4 <- ~ d_G_1 + d_Y + d_Pop # list of instruments
system4 <- list(GasCons = Consump4, GasPrice = Price4) # make a SEM
#
# OLS 
model4 <- systemfit(system4, method="OLS", data = gas)
round(coef(summary(model4)), digits = 6)
#
# 2SLS
model4 <- systemfit(system4, method="2SLS",  inst = instr4, data = gas)
summary(model4)
round(coef(summary(model4)), digits = 6)
# Results may be replicated using the ivreg function from {AER}:
#
IVR4.e1 <- ivreg( d_G ~ d_Pg + d_G_1 | d_G_1 + d_Y + d_Pop, 
                  data=gas)
coef(summary(IVR4.e1, diagnostics=T))
# and
IVR4.e2 <- ivreg( d_Pg ~ d_G + d_Y + d_Pop | d_G_1 + d_Y + d_Pop, data=gas)
coef(summary(IVR4.e2, diagnostics=T))
# compare with
round(coef(summary(model4)), digits = 6)
#
#
#