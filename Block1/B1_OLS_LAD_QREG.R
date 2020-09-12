#### Infant Mortality rates:   OLS and Quantile regression / LAD estimation ####
#    
#
# Example based on
# Wooldridge, Introductory econometrics, 5th ed., Example 9.10
#
# Model data:
# 
# 
#
#
#
#
# Data + basic plot
rm(list=ls())
load('infmrt.RData')
# Data description
desc
# We shall only use 1990 data
data <- data[data$year==1990,]
plot(data[,c(2,4,5,6)])
#
#
#
#
# Model specification
#
# log(infmort) = beta0 + beta1*log(pcinc) + beta2*log(physic)
#
#
# STEP 1 - Fit an OLS model
OLS.1 <- lm(infmort ~ log(pcinc) + log(physic) + log(popul), data)
summary(OLS.1)
plot(OLS.1, which = 4)
plot(OLS.1, which = 5)
#
# It seems that the observation "48" /D.C./ is an outlier & high leverage point...
#
# If we drop the outlier from regression, the model is estimated (using OLS) as follows
OLS.2 <- lm(infmort ~ log(pcinc) + log(physic) + log(popul), data = data[data$DC==0,])
summary(OLS.2)
#
# We can see that the coefficients differ significantly
library(sandwich) # install.packages("sandwich")
library(lmtest) # install.packages("lmtest")
coeftest(OLS.1, vcov = vcovHC)
coeftest(OLS.2, vcov = vcovHC)
#
#
#
# STEP 2 - Use LAD / Quantile regression
#
library(quantreg) # install.packages("quantreg")
?rq
# LAD
LAD.1 <- rq(infmort ~ log(pcinc) + log(physic) + log(popul), 
            data = data, tau = 0.5)
summary(LAD.1)
summary(LAD.1, se="boot")
#
# The LAD also changes when DC is dropped from the dataset:
# + the change is lower when compared with OLS
# - LAD performs well on large datasets (asymptotic properties)
LAD.2 <- rq(infmort ~ log(pcinc) + log(physic) + log(popul), 
            data = data[data$DC==0,], tau = 0.5)
summary(LAD.2)
summary(LAD.2, se="boot")
# Compare to LAD estimation with DC indluded:
summary(LAD.1, se="boot")
#
#
#
#
# Quantile regression, use tau = 0.25, 0.5, 0.75
#
# Note that the following example is mostly for illustration purposes,
# as we only have 51 observations.
QR.1 <- rq(infmort ~ log(pcinc) + log(physic) + log(popul), data = data, tau = c(0.25, 0.5, 0.75))
summary(QR.1)
summary(QR.1, se="boot")
# summary plot
plot(summary(QR.1, se="boot"), ols=F)
#
#
# ANOVA test for equal coefficients at tau 0.25 and 0.75
q25 <- rq(infmort ~ log(pcinc) + log(physic) + log(popul), tau = 0.25, data = data)
q75 <- rq(infmort ~ log(pcinc) + log(physic) + log(popul), tau = 0.75, data = data)
anova(q25, q75)
#
#
#
#
# Example 2 - OLS - LAD - Quantile regression
# Example 7.10 (Greene): Income Elasticity of Credit Cards Expenditure
#
# Repetition of the results as in Lecture 5
#
# Data
rm(list=ls())
data.2 <- read.csv("TableF7-3.csv")
# 
# Cardhldr = Dummy variable, 1 if application for credit card accepted, 0 if not
# Default = 1 if defaulted 0 if not (observed when Cardhldr = 1, 10,499 observations),
# Age = Age in years plus twelfths of a year,
# Adepcnt = 1 + number of dependents,
# Acadmos = months living at current address,
# Majordrg = Number of major derogatory reports,
# Minordrg = Number of minor derogatory reports,
# Ownrent = 1 if owns their home, 0 if rent
# Income = Monthly income (divided by 10,000),
# Selfempl = 1 if self employed, 0 if not,
# Inc_per = Income divided by number of dependents,
# Exp_Inc = Ratio of monthly credit card expenditure to yearly income,
# Spending = Average monthly credit card expenditure (for Cardhldr = 1),
# Logspend = Log of spending.
#
# 
# OLS
OLS.est <- lm(LOGSPEND ~ log(INCOME)+AGE+ADEPCNT, data=data.2)
summary(OLS.est)
# LAD .... different optimization algorithm leads to slightly different results compared to EViews
LAD.2 <- rq(LOGSPEND ~ log(INCOME)+AGE+ADEPCNT, data=data.2, tau = 0.5)
summary(LAD.2)
summary(LAD.2, se="boot")
# compare to OLS
coeftest(OLS.est)
#
# QREG for deciles
QR.2 <- rq(LOGSPEND ~ log(INCOME)+AGE+ADEPCNT, data=data.2, tau = 1:9/10)
summary(QR.2)
# summary plot
plot(summary(QR.2), ols=T)
#
# Coefficient comparison at different deciles
q10 <- rq(LOGSPEND ~ log(INCOME)+AGE+ADEPCNT, data=data.2, tau = 0.1)
q80 <- rq(LOGSPEND ~ log(INCOME)+AGE+ADEPCNT, data=data.2, tau = 0.8)
anova(q10, q80)
#
#
#
#
#
#
#