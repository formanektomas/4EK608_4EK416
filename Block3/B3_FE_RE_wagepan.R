#### FE with time invariant regressors, Pooled vs. FE vs. RE & tests #### 
#
# 
# 
# 
#
#
### Example 14.2: Based on Wooldridge: Introductory econometrics, 
#    
#   Does the Return to education change over time?
#   
#
# Data
rm(list=ls())
wagepan <- read.csv('wagepan.csv') # Read the data

#
# Model data:
#
# lwage    - log(wage)
# d81      - binary, = 1 if year == 1981
# ....
# d87      - binary, = 1 if year == 1987
# married  - binary, = 1 if married
# union    - binary, = 1 if in union
# educ     - years of schooling
# nr       - person identifier (545 individuals)
# year     - 1980 to 1987
#
#
#
#
library(plm) # install.packages("plm")
#
#
# 
# OLS / Pooled estimation of the model
#
OLS.fit <- lm(lwage ~ educ + black + hisp + married 
              + union + d81 + d82 + d83 + d84 
              + d85 + d86 + d87, data=wagepan)
summary(OLS.fit)
#
?plm
# We shall replicate the above results using the tools from {plm} 
#
Pooled.fit <- plm(lwage ~ educ + black + hisp + married 
                  + union + d81 + d82 + d83 + d84 
                  + d85 + d86 + d87, data=wagepan, 
                  model='pooling', index=c('nr','year'))
#
summary(Pooled.fit)
#
#
#
# FE Estimation (all time-invariant regressors are dropped)
model1 <- plm(lwage ~ married + union +
               d81 + d82 + d83 +d84 + d85 + d86 + d87,
             data=wagepan,
             model='within', #FE
             effect='individual',
             index=c('nr','year'))
summary(model1)
# F test for redundancy of individual effects
pFtest(model1,Pooled.fit)
# Honda (default) test for redundancy of individual effects
plmtest(model1, effect = "individual")
#
#
# Check FE (FD vs FD) estimator:
# .. estimate model by FD first, then use pwfdtest()
model.FD <- plm(lwage ~ married + union +
                d81 + d82 + d83 +d84 + d85 + d86 + d87,
              data=wagepan,
              model='fd', #FD
              effect='individual',
              index=c('nr','year'))
pwfdtest(model.FD,h0="fe") # reject H0 of no ar in FE errors
pwfdtest(model.FD,h0="fd") # reject H0 of no ar in FD errors
# .. use robust errors:
library(lmtest)
library(car)
coeftest(model1, vcov.=function(x) vcovHC(x, method="arellano", type="HC1"))
#
# We may want to extract the individual effects (first 10 individuals)
head(fixef(model1), 10)
?fixef # may be used for extraction of time effects as well.
#
#
#
#
# With FE, we cannot include variables that remain constant for individuals,
# such as race, gender and -in this example- education 
# (education many change for individuals over time, but it doesn't in our dataset)
# However, we can include interaction terms of education with other variables, such as year dummies:
#
model2 <- plm(lwage ~ married + union +
               d81 + d82 + d83 +d84 + d85 + d86 + d87 +
               d81*educ + d82*educ + d83*educ + d84*educ + d85*educ +
               d86*educ + d87*educ,
             data=wagepan,
             model='within',
             effect='individual',
             index=c('nr','year'))
summary(model2)
coeftest(model2, vcov.=function(x) vcovHC(x, method="arellano", type="HC1"))
# F test for linear restrictions of coefficients for panel data models:
pFtest(model2, model1) # H0: all coefficients not included in "model1" are insignificant, 
#                      # i.e. H0: all interaction terms are equal to zero.
#
#
#
# RE method: model1 estimated using RE
#
model3 <- plm(lwage ~ married + union +
                d81 + d82 + d83 +d84 + d85 + d86 + d87,
              data=wagepan,
              model='random',
              effect='individual',
              index=c('nr','year'))
summary(model3)
# test for redundancy of individual effects
plmtest(model3, effect = "individual")
#
# Hausmann test for Panel models
phtest(model1, model3) # HO: RE-estimated model is consistent, RE estimation is appropriate
#                        # H1: RE is not an appropriate estimator for our data
# 
# 
#
#
### Example 14.4: Based on Wooldridge: Introductory econometrics, 
#    
#   A Wage Equation Using Panel Data: 
#   a simple comparison of OLS (pooled) vs. FE vs. RE estimates
#   
#
# OLS
lm.1 <- lm(lwage ~ educ + black + hisp + exper + expersq + married + union +
             d81 + d82 + d83 + d84 + d85 + d86 + d87,
           data=wagepan)
#
# Fixed effects
plm.fe <- plm(lwage ~ expersq + married + union +
                d81 + d82 + d83 + d84 + d85 + d86 + d87,
              data=wagepan,
              effect='individual',
              model='within',
              index=c('nr','year'))
#
# Random effects
plm.re <- plm(lwage ~ educ + black + hisp + exper + expersq + married + union +
                d81 + d82 + d83 + d84 + d85 + d86 + d87,
              data=wagepan,
              model='random',
              index=c('nr','year'))
#
# Table of results (rounded to 4 digits) as in Wooldridge
results <- round(data.frame("Pooled OLS"=lm.1$coefficients[2:8],
                            "Random Effects"=plm.re$coeff[2:8],
                            "Fixed Effects"=c(NA,NA,NA,NA,plm.fe$coeff[1:3])),3)
results
#
#
#
#