#### Tobit ####
#
# 
# 
# 
#
#
#
# Data
rm(list=ls())
mroz<-read.csv('mroz.csv')
#
#
#
### Example 17.2: Based on Wooldridge: Introductory econometrics, 
#    
#   Tobit model (Corner solution responses)
#   
#
library(AER) #install.packages('AER')
?tobit # convenient interface to "survreg", may be used for
# censored and survival analysis
# default left censoring: left = 0 /may be changed/
#
Tobit.Fit <- tobit(hours ~ nwifeinc + educ + exper + expersq + age +
                     kidslt6 + kidsge6, data=mroz)
summary(Tobit.Fit)
# Tobit may be compared to OLS results for the same specification
OLS.fit <- lm(hours ~ nwifeinc + educ + exper + expersq + age + 
             kidslt6 + kidsge6, data=mroz)
summary(OLS.fit)
#
# Comparison table - coefficients of Tobit and OLS
Table1 <- cbind(Tobit.Fit$coefficients, OLS.fit$coefficients)
colnames(Table1) <- c("Tobit", "OLS")
round(Table1, 4)
#
#
# Predictions from a Tobit model
fitted <- predict(Tobit.Fit, type="response")
cor(mroz$hours,fitted)^2
head(cbind(mroz$hours,fitted), 10)
tail(cbind(mroz$hours,fitted), 10)
#
#
# APEs for the Tobit model
ape.factor <- mean(pnorm(Tobit.Fit$linear.predictors/Tobit.Fit$scale))
# for "ape.factor, see equation 17.25 in Wooldridge, Introductory econometrics
APEs <- ape.factor*Tobit.Fit$coefficients[2:8]
APEs 
# 
# 
#
#