#### SEMs - specification, identification and estimation ####
#
# 
# 
# 
#
#
### Example 16.3 & 16.5: Based on Wooldridge: Introductory econometrics, 
#    
#   Labor supply of married, working women
#   
#
# Data
rm(list=ls())
mroz <- read.csv('mroz.csv')
# We limit our data to working women only
mroz <- mroz[mroz$inlf == 1, ]
#
# Model data:
#
# hours      - hours worked, 1975
# wage       - hourly wage
# educ       - years of schooling
# age        - woman's age in years
# kidslt6    - number of kids < 6 years old
# nwifeinc   - faminy income with "wage" variable excluded
# exper      - actual labor market experience
# expersq    - exper^2
#
# Basic data plots
# Scatterplot matrix of the data used in our model
plot(mroz[, c(2,3,4,5,6,7,19,20,22)])
#
#
library(systemfit) # install.packages("systemfit")
#
# Specify a system of equations and instruments:
eqHours <- hours ~ log(wage) + educ + age + kidslt6 + nwifeinc
eqWage  <- log(wage) ~ hours + educ + exper + expersq
instr <- ~ educ + age + kidslt6 + nwifeinc + exper + expersq
# If no labels are provided, equations are named automatically
Wage.model <- list(Hours = eqHours, Wages = eqWage)
#
#
# We start by estimating the model using OLS (interdependencies ignored)
fitOls <- systemfit(Wage.model, data = mroz)
summary(fitOls)
round(coef(summary(fitOls)), digits = 4)
#
#
#
#
# Before we try 2SLS estimation of the SEM, we want to make sure that both equations
# are identified:
#
# Step 1:
# Estimate the reduced forms for both dependent variables:
# Reduced form for hours:
red.hours <- lm(hours ~ educ + age + kidslt6 + nwifeinc + exper + expersq, data=mroz)
# Reduced form for log(wage)
red.wage <- lm(log(wage) ~ educ + age + kidslt6 + nwifeinc + exper + expersq, data=mroz)
#
# Step 2: Verify identification of equation 1 (eqHours):
summary(red.wage)
# eqHours is identified if either exper or expersq coefficients are not zero
summary(red.hours)
#
## Assignment 1
## What is the identification condition for equation eqWage?
## Is the equation eqWage identified?
#
#
#
# Next, we estimate the model using 2SLS method
fit2sls <- systemfit(Wage.model, method = "2SLS", inst = instr, data = mroz)
summary(fit2sls)
round(coef(summary(fit2sls)), digits = 4)
#
#
# We can also estimate the model using the 3SLS method
fit3sls <- systemfit(Wage.model, method = "3SLS", inst = instr, data = mroz)
summary(fit3sls)
round(coef(summary(fit3sls)), digits = 4)
#
# The estimated models may be compared using BIC
BIC(fitOls)
BIC(fit2sls)
BIC(fit3sls)
#
#
#
#
# To assess the quality of instruments and endogeneity of regressors, we need to
# use the ivreg command (2SLS method) from the {AER} package:
library('AER') # install.packages('AER')
#
# equation eqHours
eqHours.iv <- ivreg(hours ~ log(wage) + educ + age + kidslt6 + nwifeinc
                    | educ + age + kidslt6 + nwifeinc + exper + expersq, 
                    data = mroz)
summary(eqHours.iv, vcov = sandwich, diagnostics = T)
#
#
## Assignment 2
## Comment on the results of Weak instruments test,
## Wu-Hausmann test and Sargan test
#
#
## Assignment 3
## By analogy to lines 98 - 101, evaluate the instruments for equation eqWage.
#
#
#
#
#
# 
# 
# 
#
#
### Computer exercise C16.2: Based on Wooldridge: Introductory econometrics, 
#    
#   Labor supply of married, working women
#
#
# (i) We re-estimate the SEM with log(hours) used instead of "hours"
#
# Specify a system of equations and instruments:
eqHours2 <- log(hours) ~ log(wage) + educ + age + kidslt6 + nwifeinc
eqWage2  <- log(wage) ~ log(hours) + educ + exper + expersq
instr2 <- ~ educ + age + kidslt6 + nwifeinc + exper + expersq
# If no labels are provided, equations are named automatically
Wage.model2 <- list(Hours = eqHours2, Wages = eqWage2)
#
fit2s2s.c16.2 <- systemfit(Wage.model2, method = "2SLS", inst = instr2, data = mroz)
#
summary(fit2s2s.c16.2)
round(coef(summary(fit2s2s.c16.2)), digits = 4)
#
#
# (ii) We allow educ to be endogenous because of omitted ability.
#      We use motheduc and fatheduc as IVs for educ.
#
eqHours3 <- log(hours) ~ log(wage) + educ + age + kidslt6 + nwifeinc
eqWage3  <- log(wage) ~ log(hours) + educ + exper + expersq
instr3 <- ~ age + kidslt6 + nwifeinc + exper + expersq + motheduc + fatheduc
# 
# Note that we go beyond the standard SEM definition and identification paradigm,
# as motheduc and fatheduc are not present in the set of SEM regressors...
#
Wage.model3 <- list(Hours = eqHours3, Wages = eqWage3)
#
fit2s2s.c16.2.ii <- systemfit(Wage.model2, method = "2SLS", inst = instr3, data = mroz)
#
summary(fit2s2s.c16.2.ii)
round(coef(summary(fit2s2s.c16.2.ii)), digits = 4)
#
#
# (iii) We use the ivreg command (2SLS method) from the {AER} package
#       to test the IVs-setup introduced in (ii):
# 
eqHours.iv3 <- ivreg(log(hours) ~ log(wage) + educ + age + kidslt6 + nwifeinc
                    | age + kidslt6 + nwifeinc + exper + expersq + motheduc + fatheduc, 
                    data = mroz)
summary(eqHours.iv3, vcov = sandwich, diagnostics = T)
#
eqWages.iv3 <- ivreg(log(wage) ~ log(hours) + educ + exper + expersq
                     | age + kidslt6 + nwifeinc + exper + expersq + motheduc + fatheduc, 
                     data = mroz)
summary(eqWages.iv3, vcov = sandwich, diagnostics = T)
#
#
#
#
#