#### Ordered Multinomial Response Models ####
#
#
#
#
#
#
require("MASS") # install.packages("MASS")
#
#
### Asset allocation in Pension Plans
#              .. amended from Wooldridge, J: (2002)
#              .. Econometric Analysis of Cross Section and Panel Data
#              .. Example 15.5
#
# Data
rm(list=ls())
pension <- read.csv("pension.csv")
#
# Variables
# 
# Ordered dependent variable: 
# pctstck = {0, 50, 100}
# codes the response "mostly bonds", "mixed", "mostly stock"
#
# Explanatory variables:
# age, educ female, black, married .. standard interpretation
# choice = 1  if a person has a choice in how pension fund is invested
# wealth89    net worth, 1989, $1000
# prftshr =1  if profit sharing plan
# income is described by a set of dummy variables:
#
# finc25=1  ....   $15,000  < faminc92  <= $25,000
# finc35=1  ....   $25,000  < faminc92  <= $35,000
# finc50=1  ....   $35,000  < faminc92  <= $50,000
# finc75=1  ....   $50,000  < faminc92  <= $75,000
# finc100=1 ....   $50,000  < faminc92  <= $75,000
# finc101=1 ....   $100,000 < faminc92
#
#
#
# Model estimation
#
#
# Ordered response 
require(MASS) # # instal.packages("MASS")
?polr
head(pension)
summary(pension)
pension$pctsk <- as.factor(pension$pctstck) 
pension$female <- as.factor(pension$female)
pension$choice <- as.factor(pension$choice)
pension$black <- as.factor(pension$black)
# dep. var must be a factor when polr() is used.
Ordered.logit <- polr(pctsk~age+educ+female+black+choice+wealth89
                      +prftshr+finc35+finc50+finc75+finc100+finc101, 
                      data=pension, Hess = T, method="logistic")
#
summary(Ordered.logit)
#
require("lmtest") # instal.packages("lmtest")
require("AER") # install.packages("AER")
coeftest(Ordered.logit)
#
#
# Simple predictions
head(predict(Ordered.logit), 10)
head(predict(Ordered.logit, type="p"), 10)
#
#
#
#
#
# Model evaluation 
logLik(Ordered.logit)
lrtest(Ordered.logit)
#  Pseudo-R-squared = 1 - (logLik(UR)/logLik(null.model))
null.m2 <- polr(pctsk~1, data=pension, Hess = T, method="logistic")
summary(null.m2)
cat("Pseudo-R-squared =", 1- (logLik(Ordered.logit)/logLik(null.m2)) )
#
#
#
#
# Confusion matrix
#
logit.fitted <- predict(Ordered.logit)
require(caret) # install.packages("caret")
#
confusionMatrix(data = logit.fitted, reference = pension$pctsk)
#
#
#
#
# 
## The estimated effect of change in the explanatory variable "choice" 
#  on the expected probabilities of dependent variable outcomes.
#
#  .. Beta coefficient for "choice" > 0
#  .. therefore: P(y="0", i.e. mostly bonds) decreases as "choice" changes from 0 to 1
#                P(y="100", i.e. mostly stocks) increases as "choice" changes from 0 to 1
#                effect on P(y="50") is not determined by sign of the beta coefficient!
#
#  .. To calculate expected probabilities, we need to choose values
#  .. for the other regressors:
#     Illustrative, approx. average person: 
#         age=60, educ=13.5, married=0,
#         black=0, female=0, wealth89 = 200 {$200.000}
#         finc75=1 {income between $50.000 and $75.000}
# new data
pens2 <- rbind(c(NA, 60, 13.5, 0, 0, 0, 200, 1, 0, 0, 1, 0, 0), 
               c(NA, 60, 13.5, 0, 0, 1, 200, 1, 0, 0, 1, 0, 0))
pens2 <- data.frame(pens2)
colnames(pens2) <- c("pctsk","age","educ","female","black","choice","wealth89","prftshr",
                    "finc35","finc50","finc75","finc100","finc101")
pens2$female <- as.factor(pens2$female)
pens2$choice <- as.factor(pens2$choice)
pens2$black <-  as.factor(pens2$black)
pens2 # Our "new data" table: reference individual, "choice" changes
# prediction of dependent variable probability outcomes
y.P.logit <- predict(Ordered.logit, type='p', newdata=pens2)
#
y.P.logit # 
diff(y.P.logit) # The effect of "choice" (changing 0 --> 1) on the reference individual
#
# As the outcomes are ordered, it makes sense to calculate
# cumulative probabilities: e.g.: P(y <= 50 | x) = P(y = 0 | x) + P(y = 50 | x)
# and the changes in estimated cumulative probabilities given a change in regressor.
#
# P(y <= 50), choice=0
1-y.P.logit[1,3]
# P(y <= 50), choice=1, cet. par.
1-y.P.logit[2,3]
# Change in P(y <= 50) as "choice" moves from 0 to 1 for the reference individual:
(1-y.P.logit[2,3]) - (1-y.P.logit[1,3])
#
#
#
#
library(effects)
plot(effect("age:choice",Ordered.logit))
#