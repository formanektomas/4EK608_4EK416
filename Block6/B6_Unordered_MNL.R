#### Multinomial Logit ####
#
#
#
# Data
rm(list=ls())
occ <- read.csv("occupation.csv")
# sample of young men & their occupation/status
# "at home", "in school", "working", where "at home" means not in school and not working.
# Occupation/status is modelled using: age, exper, exper^2, 
# .. usuall interpretation of explanatory variables.
#
#
#
# Model estimation
require(nnet) # install.packages("nnet")
?multinom
#
summary(multinom(status~educ+exper+expersq, data=occ))
# Say, we want "school" to be the base outcome
occ$status <- relevel(occ$status, ref="school")
multi.FIT <- multinom(status~educ+exper+expersq, data=occ)
summary(multi.FIT)
require("lmtest") # instal.packages("lmtest")
require("AER") # install.packages("AER")
coeftest(multi.FIT) # interpret coefficients on educ
#
#
#
#
# Predictions from the model
#
class.fit <- predict(multi.FIT, type='class')
prob.fit <- predict(multi.FIT, type='probs')
#
head(class.fit)
head(prob.fit)
# The outcome with the highest estimated probability is the predicted outcome.
head(cbind(prob.fit, as.character(class.fit)), 15)
#
#
## Interpretation of the individual beta-coefficients 
# 
coeftest(multi.FIT)
# home:educ        -0.72
# work:educ        -0.339
#
# relative change in [p(home)/p(school)] from a one unit increase in educ, cet. par.
(exp(-0.72)-1)*100
# relative change in [p(work)/p(school)] given a one unit increase in educ, cet. par.
(exp(-0.339)-1)*100
#
# 
#
#
## Model evaluation: 
coeftest(multi.FIT)
logLik(multi.FIT)
require("lmtest") # instal.packages("lmtest")
lrtest(multi.FIT)
#  McFadden's pseudo-R-squared = 1 - (deviance(UR)/deviance(null.model))
null.m <- multinom(status~1, data=occ)
summary(null.m)
cat("McFadden's pseudo-R-squared =", 1- (deviance(multi.FIT)/deviance(null.m)) )
#
#
#
#
### Confusion matrix
require(caret) # install.packages("caret")
confusionMatrix(data = class.fit, reference = occ$status)
#
#