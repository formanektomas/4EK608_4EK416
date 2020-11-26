#### SEMs:  Estimation and identification ####
#
# 
# 
# 
#
#
### Example 16.4 & 16.6 & C16.3, Based on Wooldridge: Introductory econometrics 
#    
#   Inflation and openness of the economy
#   
#
# Data
rm(list=ls())
openness <- read.csv('openness.csv')
# 
#
# Model data: 114 economies, 1973
#
# inf        - average annual inflation, 1973
# open       - imports as % of GDP
# lpcinc     - log(per capita income in USD) 
# lland      - log(land area, square miles) 
# oil        - =1 if country is a major oil producer
#
#
# Basic data plots
# Scatterplot matrix of the data used in our model
plot(openness[, c(1,2,5,7,8)])
#
#
#
library(systemfit) # install.packages("systemfit")
#
# Specify a system of equations and instruments:
eqInf <- inf ~ open + lpcinc
eqOpen  <- open ~ inf + lpcinc + lland
instr <- ~ lpcinc + lland
# If no labels are provided, equations are named automatically
Openness.model <- list(Inflation = eqInf, Open = eqOpen)
#
# (1) OLS estimation
# We start by estimating the model using OLS (interdependencies ignored)
fitOls <- systemfit(Openness.model, data = openness)
summary(fitOls)
round(coef(summary(fitOls)), digits = 4)
#
#
# (2) 2SLS
# Next, we estimate the model using 2SLS method
fit2sls <- systemfit(Openness.model, method = "2SLS", inst = instr, data = openness)
summary(fit2sls)
# The second equation is not identified.
# We can 
#   A) Leave out equation 2 (eqOpen) and use IV for estimation of eqOpen
#   B) Change specification of equation 1 (add exogenous regressor)
#      to identify equation 2.
# 
#
# Approach A:
library('AER') # install.packages('AER')
iv.eq.1 <- ivreg(inf ~ open +lpcinc | lpcinc + lland,
                  data=openness)
summary(iv.eq.1, vcov = sandwich, diagnostics = T)
# 
#
# Approach B:
# We add "oil" (being a major oil producer) as a new exogenous
# regressor to the equation 1.
#
eqInf2 <- inf ~ open + lpcinc + oil
eqOpen2  <- open ~ inf + lpcinc + lland
instr2 <- ~ lpcinc + lland + oil
#
Openness.model2 <- list(Inflation = eqInf2, Open = eqOpen2)
SEM.2sls <- systemfit(Openness.model2, method = "2SLS", inst = instr2, data = openness)
summary(SEM.2sls)
round(coef(summary(SEM.2sls)), digits = 4)
#
#
#
#
#
#
# lpcinc is not significant in any of the equations:
round(coef(summary(SEM.2sls)), digits = 4)
# therefore, we shall drop this variable from our model:
eqInf3 <- inf ~ open + oil
eqOpen3  <- open ~ inf + lland
instr3 <- ~  lland + oil
#
Openness.model3 <- list(Inflation = eqInf3, Open = eqOpen3)
SEM.alt.2sls <- systemfit(Openness.model3, method = "2SLS", inst = instr3, data = openness)
summary(SEM.alt.2sls)
round(coef(summary(SEM.alt.2sls)), digits = 4)
#
#
#
#
#
#