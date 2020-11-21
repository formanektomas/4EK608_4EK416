#### Dynamic panels ####
#
#
# 
rm(list=ls())
library(plm) # install.packages("plm")
data("EmplUK", package = "plm")
# Basic data information
?EmplUK
summary(EmplUK)
length(unique(EmplUK$firm))
head(EmplUK)
#
library(ggplot2) # install.packages("ggplot2")
ggplot(EmplUK, aes(x=as.factor(year), y=log(emp), fill=as.factor(year))) + 
  geom_boxplot() +
  theme_minimal() 
#
# Estimation of the dynamic model using GMM - Arellano and Bond estimation
?pgmm # note the default "transformation" argument
#
D.P.1 <- pgmm( log(emp) ~ lag(log(emp), 1) + log(wage) + log(capital) 
               + log(output) | lag(log(emp), 2:3), 
               data = EmplUK, effect = "twoways", model = "twosteps",
               robust = T)
summary(D.P.1)
#
# Sargan Test:
# H0:  the model is "valid"
# H1:  model is "invalid"; the data does not come close to meeting the restrictions
#   .. Strictly speaking this is a test of the validity of all instruments
#   .. see https://en.wikipedia.org/wiki/Generalized_method_of_moments#J-test
#
# Arellano-Bond test of Serial Correlation
?mtest
mtest(D.P.1, 1)
mtest(D.P.1, 2)
#
#
#
# Model 2, lags on exogenous regressors as well:
D.P.2 <- pgmm(log(emp) ~ lag(log(emp), 1) + log(wage) + lag(log(wage), 1)
              + log(capital) + log(output) | lag(log(emp), 3:4),
              data = EmplUK, effect = "twoways", model = "twosteps",
              robust = T)
summary(D.P.2)
#
?mtest
mtest(D.P.2, 1, vcovHC)
mtest(D.P.2, 2, vcovHC)
#
#
#