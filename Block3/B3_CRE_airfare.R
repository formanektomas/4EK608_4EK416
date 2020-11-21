#### Correlated random effects #### 
#
# 
# 
# 
#
#
### Based on Chapter 14, Computer exercises 10 and 14, Wooldridge: Introductory econometrics, 
#
#
#
## Read the data
rm(list=ls())
load("airfare.RData")
library(dplyr)
# data description
desc
str(data)
# calculate "concenbar",
# time-averages for concen: fraction market, biggest carrier
data$ID <- as.factor(data$id)
summary(data$ID)
data <- data %>% 
  group_by(ID) %>% 
  mutate(concenbar=mean(concen)) %>% 
  ungroup()
#
library(plm) # install.packages("plm")
#
# Pooled regression
#
Pooled.fit <- plm(lfare ~ y98 + y99 + y00 + concen 
                  + ldist + ldistsq, data=data, 
                  model='pooling', index=c('ID','year'))
summary(Pooled.fit)
#
# FE estimation
#
FE.fit <- plm(lfare ~ y98 + y99 + y00 + concen 
                  + ldist + ldistsq, data=data, 
                  model='within', index=c('ID','year'))
summary(FE.fit) # ldist and ldistsq dropped as time-invariant...
#
# RE estimation
#
RE.fit <- plm(lfare ~ y98 + y99 + y00 + concen 
              + ldist + ldistsq, data=data, 
              model='random', index=c('ID','year'))
summary(RE.fit)
#
# CRE estimation
#
CRE.fit <- plm(lfare ~ y98 + y99 + y00 + concen + concenbar
              + ldist + ldistsq, data=data, 
              model='random', index=c('ID','year'))
summary(CRE.fit)
#
#
# Given the concen/concenbar transformation, the coefficient
# on "concen" is identical for FE and CRE:
#

results <- round(data.frame("Pooled"=c(Pooled.fit$coefficients[1:5],NA,Pooled.fit$coefficients[6:7]),
                            "RE"=c(RE.fit$coefficients[1:5],NA,RE.fit$coefficients[6:7]),
                            "FE"=c(NA,FE.fit$coeff[1:4],NA,NA,NA),
                            "CRE"=CRE.fit$coefficients[1:8]),5)
rownames(results) <- names(CRE.fit$coefficients)
#
results # Compare the CRE and FE coefficients for "concen"
#
#
#
# Also, CRE allows for convenient testing of FE vs. RE:
# [RE assumes beta[concenbar] = 0 and so if we reject H0, we also reject RE in favor of FE]
# ... HCE standard errors should be used ...
require(lmtest) # install.packages("lmtest")
coeftest(CRE.fit, vcov.=function(x) vcovHC(x, method="arellano", type="HC1"))
#
#
#
#
#
#

