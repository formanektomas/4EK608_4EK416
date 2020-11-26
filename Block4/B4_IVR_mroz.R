#### Comparison of OLS and IV estimator, 2SLS estimator, testing for endogeneity ####
#
# 
# 
# 
#
#
### Example 15.1: Based on Wooldridge: Introductory econometrics, 
#    
#   Estimating the return to education for married women 
#   
#
# Data
rm(list=ls())
mroz <- read.csv('mroz.csv')
# at this point, we only use data for working married women 
mroz <- mroz[mroz$inlf!=0,]
#
# Model data:
#
# lwage     - dependent variable, log of wage (hourly wages)
# educ      - years of schooling
# fatheduc  - father's years of schooling
# motheduc  - mother's years of schooling
# exper     - actual labor market experience
#
plot(mroz[, c(21, 15, 16, 6, 19, 22)])
#
#
#
# OLS model
#
ols.1 <- lm(lwage ~ educ, data=mroz)
summary(ols.1)
#
# IV estimation 
#
# fatheduc as IV for educ
iv.1 <- lm(educ ~ fatheduc, data=mroz)
summary(iv.1)
#
# IV estimation
library('AER') # install.packages('AER')
?ivreg
iv.est <- ivreg(lwage ~ educ | fatheduc, data=mroz)
summary(iv.est, vcov = sandwich, diagnostics = T)
#
#
### Example 15.5: Based on Wooldridge: Introductory econometrics, 
#    
#   Estimating the return to education for married women 
#   
#
# OLS 
# as compared to ols.1, we add more exogenous variables: exper, expersq
s.lm.1 <- lm(lwage ~ educ + exper + expersq, data=mroz)
summary(s.lm.1) 
# what is the effect on estimated coeff. for educ in comparison with ols.1?
#
#
# Say, we want to use fatheduc and motheduc as IVs for educ:
# 
iv.2a <- lm(educ ~ exper + expersq + motheduc + fatheduc, data = mroz)
summary(iv.2a)
# What are the identification conditions for s.lm.1 when estimated by IV?
#
iv.2b <- lm(educ ~ exper + expersq, data = mroz)
summary(iv.2b)
#
anova(iv.2a,iv.2b)
#
# IV estimation
iv.est.15.5 <- ivreg(lwage ~ educ + exper + expersq | exper + expersq 
                + motheduc + fatheduc, data=mroz)
summary(iv.est.15.5, vcov = sandwich, diagnostics = T)
#
#
### Example 15.7: Based on Wooldridge: Introductory econometrics, 
#    
#   Durbin-Wu-Hausman test of endogeneity 
#   
#
# Structural equation
eq.15.7.a <- lm(lwage ~ educ + exper + expersq, data=mroz)
summary(eq.15.7.a)
# Reduced form for educ
eq.15.7.b <- lm(educ ~ exper + expersq + motheduc + fatheduc, data = mroz)
summary(eq.15.7.b)
# Structural equation with residuals from the reduced form included:
summary(lm(lwage ~ educ + exper + expersq + eq.15.7.b$resid, data=mroz))
# 
#