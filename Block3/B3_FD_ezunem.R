#### FD estimator #### 
#
# 
# 
# 
#
#
### Example 13.8: Based on Wooldridge: Introductory econometrics, 
#    
#   Effect of Enterprise Zones on Unemployment Claims
#   
#
# Data
rm(list=ls())
ez.data <- read.csv('ezunem.csv')
#
# Model data:
#
# ez       - binary, = 1 if city has EZ at given year
# d81      - binary, = 1 if year == 1981
# .....
# d88      - binary, = 1 if year == 1988
# uclms    - unemployment claims
# guclms   - log-difference: log(uclms(t)) - log(uclms(t-1))
# cez      - delta(ez) = ez(t) - ez(t-1)
# city     - city identifier, 1 to 22
#
boxplot(ez.data$uclms ~ ez.data$year)
boxplot(ez.data$uclms ~ ez.data$ez)
#
#
# First, we ignore the unobserved (individual and time) effects: 
#
lm.1 <- lm(log(uclms) ~ ez, data = ez.data)
summary(lm.1)
#
# Second, we may include the time effects:
lm.2 <- lm(log(uclms) ~ ez +d81 +d82 +d83 +d84 +d85 +d86 +d87 +d88, data = ez.data)
summary(lm.2)
#
# For panel data, we use the {plm} package
#
require(plm) # install.packages("plm")
#
?plm
# Pooled regression
pool.fit <- plm(log(uclms) ~ ez +d81 +d82 +d83 +d84 +d85 +d86 +d87 +d88, data = ez.data, 
            index=c('city','year'),
            model='pooling')
summary(pool.fit)
#
# This may be compared to the lm.2 model
summary(lm.2)
#
# FD estimator - we take into account the individual effects
# ... individual effects are assumed constant over time,
# ... individual effect get eliminated by first differences (FD estimator)
#
FD.fit <- plm(log(uclms) ~ ez +d82 +d83 +d84 +d85 +d86 +d87 +d88, data = ez.data, 
                index=c('city','year'),
                model='fd',
                effect='individual')
summary(FD.fit)
# The effect of establishing an Economic zone may be calculated as
100*(exp(coefficients(FD.fit)[1])-1) # results is in percentage points
#
# Test for ar(1) in residuals
#
?pwfdtest
# Estimation follows textbook, but
# should we actually use FE or FD approach?
pwfdtest(FD.fit, h0="fe") # serial correlation in FE-errors tested
# H0 rejected, turn attention back to FD:
#
# Observations may be clustered by "group" ("time") 
# to account for serial (cross-sectional) correlation.
pwfdtest(FD.fit, h0="fd", cluster="group") # serial correlation tested
pwfdtest(FD.fit, h0="fd", cluster="time")  # CS correlation tested
#
library(lmtest)
library(car)
# robust significance test, cluster by group
coeftest(FD.fit, vcov.=function(x) vcovHC(x, method="arellano", type="HC1"))