#### Selected methods for estimating models with count data dependent variable ####
#
# 
# 
# 
#
#
### Example 17.3: Based on Wooldridge: Introductory econometrics
#
#   Determinants of Number of Arrests for Young Men
#
#
require(lmtest) # install.packages("lmtest")
rm(list=ls())
crime1 <- read.csv("crime1.csv")
#
# Model: Number of arrests in 1986 <- (Intercept)
#                                     + pcnv "proportion of prior convictions to arrests"
#                                     + avgsen "average sentence length, months"
#                                     + tottime "time in prison since age 18 (in months)"
#                                     + ptime86 "months in prison during 1986"
#                                     + qemp86 "number of quarters employed, 1986"
#                                     + inc86 "legal income, 1986, $100s"
#                                     + black + hispan "0-1 dummies"
#                                     + born60 "=1 if born in 1960"
#
#
# Basic data information & plots
summary(crime1$narr86)
boxplot(crime1$narr86~crime1$born60)
boxplot(crime1$narr86~crime1$black)
#
#

# We start by estimating the model by OLS:
#
OLS.FIT<-lm(narr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86 + inc86 
            + black + hispan + born60, data = crime1)
summary(OLS.FIT)
logLik(OLS.FIT)
bptest(OLS.FIT)
#
#
# Poisson regression
# 
#
Poiss.FIT<-glm(narr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86 + inc86 
               + black + hispan + born60, data=crime1, family=poisson)
summary(Poiss.FIT)
logLik(Poiss.FIT)
cor(Poiss.FIT$fitted.values, crime1$narr86)^2
lrtest(Poiss.FIT)
#
#
#
#
#
# Interpretation of coefficients: approximation
#
# %delta(E(y|x)) =approx (100*beta(j))*delta(x(j))
coeftest(Poiss.FIT) # print estimated coefficients
# Lets evaluate the effect of pcnv: "proportion of prior convictions to arrests"
Poiss.FIT$coefficients[2] # pcnv coefficient
# Say, we want to know the expected effect of a 0.1 (10%) improvement in pcnv
100*Poiss.FIT$coefficients[2]*0.1
# .. i.e. by improving (increasing) pcnv by 0.1, 
# we expect to reduce narr86 by approx. 4%
#
# Interpretation of coefficients: exact calculation
#
# %delta(E(y|x)) = (100*(exp(beta(j))-1))*delta(x(j))
# pcnv variable
(100*(exp(Poiss.FIT$coefficients[2])-1))*0.1
# This is the expected effect of a 0.1 change in pcnv...
#
# The expected effect of a change in the dummy variable black
coefficients(Poiss.FIT)[8] # "black" coefficient
#
100*(exp(coefficients(Poiss.FIT)[8])-1)
# 93.6 % more arrests for black young men as compared to whites (cet. par.), 1986
#
#
#
#
#
# Predictions (fitted values) from an estimated Poisson model:
#
predPoiss <- predict(Poiss.FIT, type="response") 
# for prediction on the scale of "y" - dependent variable
head(cbind(crime1$narr86, predPoiss), 30)
cor(Poiss.FIT$fitted.values, crime1$narr86)^2
#
#
#
#
# Dispersion test & Poisson distribution restrictions
# .. Equidispersion test
#
require(AER) # install.packages("AER")        # AER = Applied Econometrics with R
?dispersiontest
dispersiontest(Poiss.FIT)
# We reject H0 of equidispersion, i.e. E(x) = VAR(X)
#
# Summary of the Poisson regression may be amended by 
# choosing proper dispersion 
# .. coefficients are not affected (dispersion is only relevant for inference)
summary(Poiss.FIT, dispersion = 1.5)
# As compared to
summary(Poiss.FIT) # dispersion = 1 is the default value
#
#
#
#
## Negative Binomial Distribution & Model
# 
require(MASS) # install.packages("MASS")
#  MASS = 'Modern Applied Statistics with S'
NB.FIT<-glm.nb(narr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86 + inc86 
               + black + hispan + born60, data=crime1)
summary(NB.FIT)
logLik(NB.FIT)
head(predict(NB.FIT)) # scale of linear predictors
head(predict(NB.FIT, type="response")) # scale of dep. variable
head(cbind(NB.FIT$fitted.values, crime1$narr86), 30)
cor(NB.FIT$fitted.values, crime1$narr86)^2
lrtest(NB.FIT)
#
# IRR
# We might be interested in looking at incident rate ratios 
# rather than coefficients. To do this, we can exponentiate 
# our model coefficients. The same applies to the confidence intervals.
# 
# The dispersion parameter in negative binomial regression does not affect 
# the expected counts, but it does affect the estimated variance of the expected counts. 
# For other examples, see: 
# http://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
#
est <- cbind(Estimate = coef(NB.FIT), confint(NB.FIT))
IRR <- exp(est)
#
est
IRR
# 
# Note: IRR works the same for Poisson and for NB - based models
#
#
#
## Zero inflated models 
require(pscl)  # install.packages("pscl")
?zeroinfl
#
# y ~ x1 + x2 | z1 + z2 + z3 
# .. is the count data model y ~ x1 + x2  /Poisson, NB/,
# .. conditional on (|) the zero-inflation model y ~ z1 + z2 + z3 /logit/
#
#
# Say, we keep the ZI count part of the model unchanged:
# narr86 <- pcnv+avgsen+tottime+ptime86+qemp86+inc86+black+hispan+born60
#
# We construct an additional, "link", model (to be estimated by logit), describing 
# the participation / non-participation in activities that are
# of criminal nature or with significant arrest potential
# .. we may be over-simplifying here by ignoring accidental & unprovoked arrests.
# "link":
# narr86 <- qemp86+born60+hispan+black+inc86
#
#
#
## Zero-inflated Poisson model  
ZI.Poiss.FIT <- zeroinfl(narr86 ~ pcnv + avgsen + tottime + ptime86 + qemp86 + inc86 
                         + black + hispan + born60 | qemp86+born60+hispan+black+inc86,
                         link = "logit", dist = "poisson", data = crime1)
summary(ZI.Poiss.FIT)
head(predict(ZI.Poiss.FIT)) 
head(predict(ZI.Poiss.FIT, type="response")) 
cor(ZI.Poiss.FIT$fitted.values, crime1$narr86)^2
lrtest(ZI.Poiss.FIT)
#
