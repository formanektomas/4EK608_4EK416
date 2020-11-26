#### IV estimators in multiple regression models ####
#
# 
# 
# 
#
#
### Example 15.4: Based on Wooldridge: Introductory econometrics, 
#    
#   Wage and education: using proximity to college as an IV for educ
#   
#
# Data
rm(list=ls())
card <- read.csv('card.csv')
card$lwage <- log(card$wage)
card$expersq <- card$exper^2
#
# Model data:
#
# wage       - hourly wage in cents
# nearc4     - binary, = 1 if near 4-year college
# educ       - years of schooling
# black      - binary, = 1 if black
# smsa       - binary, = 1 if standard metropolitan statistical area
# south      - binary, = 1 if in south
# exper      - age - educ - 6
# expersq    - exper^2
#
#
plot(card[, -1])
#
#
#
# OLS model 
#
ols.1 <- lm(lwage ~ educ + exper + expersq + black
            + smsa +south, data = card)
summary(ols.1)
#
# IVR
#
# use nearc4 as IV for educ
#
## Supervised work:
## How do we evaluate identification of the structural equation
## from the following reduced form equation?
# Reduced form for educ:
red.form <- lm(educ ~ nearc4 + exper + expersq + black
           + smsa + south, data = card)
summary(red.form)
#
# IVR
library('AER') # install.packages('AER')
iv.est <- ivreg(lwage ~ educ + exper + expersq + black
                + smsa + south | nearc4 + exper + expersq + black
                + smsa + south, data = card)
summary(iv.est, vcov = sandwich, diagnostics = T)
fitted.ivreg <- fitted.values(iv.est)
#
# 2SLS performed "manually"
M.2SLS <- lm(lwage ~ red.form$fitted.values + exper + expersq + black
             + smsa +south, data = card)
summary(M.2SLS)
fitted.2sls <- fitted(M.2SLS)
#
##    The fitted values from the ivreg object 
##    "iv.est" are correct, because X %*% b is used for calculation, 
##    where X is the matrix of regressors
##    and b is the IV-estimated vector of beta coefficients.
head(fitted.2sls, 8)
head(fitted.ivreg, 8)
##
##