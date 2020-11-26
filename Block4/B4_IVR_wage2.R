#### Comparison of OLS and IV estimator,  testing for endogeneity ####
#
# 
# 
# 
#
#
### Example 15.2: Based on Wooldridge: Introductory econometrics, 
#    
#   Estimating the return to education men 
#   
#
# Data
rm(list=ls())
wage2 <- read.csv('wage2.csv')
#
# Model data:
#
# lwage     - dependent variable, log of wage (monthly wages)
# educ      - years of schooling
# feduc    - father's years of schooling
# meduc     - mother's years of schooling
# sibs      - number of siblings
#
plot(wage2[, c(10, 4, 6, 12, 13)])
#
#
#
# OLS model 
#
ols.1 <- lm(lwage ~ educ, data=wage2)
summary(ols.1)
#
# IV estimation 
#
# sibs IV for educ
iv.1 <- lm(educ ~ sibs, data=wage2)
summary(iv.1)
#
# IV estimation
library('AER') # install.packages('AER')
iv.est <- ivreg(lwage ~ educ | sibs, data=wage2)
summary(iv.est, vcov = sandwich, diagnostics = T)
#
# "Manual" Durbin-Wu-Hausman test of endogeneity of educ
#   
# Structural equation with residuals from the reduced form included:
summary(lm(lwage ~ educ + iv.1$resid, data=wage2))
# 
#
#
## Supervised work:
## Repeat the example 15.2, but use "meduc" and "feduc" as IVs for educ 
## instead of "sibs". Compare the results of IV estimation on line 42.
## 