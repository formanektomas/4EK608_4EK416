#### Comparison of OLS and IV estimator,  testing for weak instruments ####
#
# 
# 
# 
#
#
### Example 15.3: Based on Wooldridge: Introductory econometrics, 
#    
#   Estimating the effect of smoking on birth weight 
#   
#
# Data
rm(list=ls())
bwght <- read.csv('bwght.csv')
bwght$lbwght <- log(bwght$bwght)
#
# Model data:
#
# lbwght     - log of birth weight
# packs      - packs smoked per day while pregnant
# cigprice   - cigarette price (1988)
# motheduc   - mother's years of education
# fatheduc   - father's years of education
#
plot(bwght[, c(13, 12, 3, 5, 6)])
#
#
# OLS model 
#
ols.1 <- lm(lbwght ~ packs, data=bwght)
summary(ols.1)
#
# IV estimation 
#
# use cigprice as IV for packs
iv.1 <- lm(packs ~ cigprice, data=bwght)
summary(iv.1)
# "Rule of thumb" for weak instruments:
# If the F-statistics of the reduced form equation is below 10, instruments are probably weak.
summary(iv.1)$fstatistic
#
# IV estimation
library('AER') # install.packages('AER')
iv.est <- ivreg(lbwght ~ packs | cigprice, data=bwght)
summary(iv.est, vcov = sandwich, diagnostics = T)
#
## Supervised work:
## Repeat the example 15.3, as follows:
## 1) Choose one of "motheduc" or "fatheduc" as IV for "packs" instead of "cigprice"
##    and test it for weak instrument; test for for endogeneity of packs
## 2) Use both "motheduc" and "fatheduc" instead of "cigprice" and repeat the
##    tests for endogeneity and weak instrument.
## 3) Compare the results.
## 