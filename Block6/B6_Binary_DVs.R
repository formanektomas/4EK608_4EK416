#### Binary dependent variables: LPM, logit, probit ####
#
#
#
#  
# Based on Wooldridge - Introductory econometrics, Example 8.8
# Labor force participation for married women
# 
#
#
#
### Binary dependent variables & OLS
#
rm(list=ls())
mroz<-read.csv('mroz.csv')
ols.fit<-lm(inlf ~ nwifeinc + educ + exper  + age + kidslt6 + kidsge6,
           data=mroz)
summary(ols.fit)
pred.lf.partic <- predict(ols.fit, type="response") 
summary(pred.lf.partic) # not within <0; 1> 
# 
# Heteroskedasticity & Robust errors 
#
require(lmtest)
require(sandwich)
summary(ols.fit)
bptest(ols.fit)
# Heteroskedasticity corrected standard errors:
coeftest(ols.fit, vcov=vcovHC(ols.fit,type='HC0'))
#
#
#
#
#
### Logistic regression
#
?glm
#
logit.fit<-glm(inlf ~ nwifeinc + educ + exper  + age + kidslt6 + kidsge6,
               family=binomial, data=mroz)
summary(logit.fit)
#
# Evaluation of the estimated model
logLik(logit.fit) # The maximized log-likelihood 
#
# Mc Fadden R^2 - manual calculation... 
# Mc Fadden R^2 = 1 - (logLik(Unrestricted model)/logLik(Trivial model))
logit.zero <- glm(inlf ~ 1, family=binomial, data=mroz) # Trivial model
LL.UR <- as.numeric(logLik(logit.fit))
LL.zero <- as.numeric(logLik(logit.zero))
cat("Mc Fadden R^2 = ", 1 - LL.UR/LL.zero)
#
# LR: Likelihood ratio statistics
# LR = 2*(LL(UR)-LL(zero)) ~ Chisq(k) 
# .. under H0: model(UR) is not statistically significant 
# .. where k is the number of variable regressors in UR model
require(lmtest)
lrtest(logit.fit, logit.zero) # Reject H0
#
## Predictions from a model estimated using logistic regression
#
?predict.glm
# Predict on the scale of the linear predictors:
# .. predict the log-odds for observations 100 and 120
p.l.odds <- predict(logit.fit, newdata=mroz[c(100,120), ])
p.l.odds
# 'Manually', we can predict odds (odds ratio):
p.odds <- exp(p.l.odds)
p.odds
# 'Manual' prediction of the success probability:
# p(y) = odds/(1+odds)
p.prob <- p.odds/(1+p.odds)
p.prob
# Success probability, using the "response" argument in predict()
p.prob.R <- predict(logit.fit, type="response", newdata=mroz[c(100,120), ])
p.prob.R
# The predictions may be summarized as follows: 
p.l.odds # log-odds
p.odds # odds ratio
p.prob # predicted probability
p.prob.R # predicted probability using the "response" argument
#
#
#
#
### Probit
#
?glm
#
probit.fit<-glm(inlf ~ nwifeinc + educ + exper  + age + kidslt6 + kidsge6,
               family=binomial(link = "probit"), data=mroz)
summary(probit.fit)
# Prediction from the probit-estimated model
predict(probit.fit, type="response", newdata=mroz[c(100,120), ])
#
#
#
#
#
#
### Marginal effects - Partial effects
# Wooldridge: APEs & PEAs
#  Coefficients from a probit and logit model are not directly comparable 
#  and easily interpretable. 
#  However, their marginal effects are.
require(mfx) # install.packages("mfx")
?logitmfx
?probitmfx
# Please note the "atmean" argument - APE vs. PEA
#
# APEs
logit.APEs <- logitmfx(inlf ~ nwifeinc + educ + exper  + age 
                       + kidslt6 + kidsge6, data=mroz, atmean = F)
logit.APEs
# PAEs
logit.PEAs <- logitmfx(inlf ~ nwifeinc + educ + exper  + age 
                       + kidslt6 + kidsge6, data=mroz, atmean = T)
logit.PEAs
#
#
#
#
#
### Confusion matrix
#
library(caret) # install.packages("caret")
library(e1071) # install.packages("e1071")
?confusionMatrix
# Success ratio / prevalence needs to be specified for the confusionMatrix()
preval <- sum(mroz$inlf)/nrow(mroz)
pred1 <- predict(logit.fit, type="response")
pred2 <- factor(round(pred1))
observed.inlf <- factor(mroz$inlf)
confusionMatrix(data=pred2, observed.inlf, positive="1", prevalence=preval)
# "no information rate," refers to predictions based on largest class 
# in the table...
# Mcnemar's Test
# https://en.wikipedia.org/wiki/McNemar%27s_test
# ..  to determine whether the row and column marginal frequencies are equal
#
#
#
# Accuracy & ROC curves (for Logit)
#
library(ROCR) # install.packages("ROCR")
# help(package=ROCR)
#
# Accuracy plot: (TP+TN)/(P+N)
?prediction # step 1 in every ROCR evaluation
ROC.pred <- prediction(pred1, labels=mroz$inlf) # step 1 in every ROCR evaluation
# Note that we are using pred1 (values are not rounded)
?performance
perf <- performance(ROC.pred, measure="acc") # Actual performance evaluation 
plot(perf)
#
# ROC Curve
# ROC curve (receiver operating characteristics)
plot(performance(ROC.pred,"tpr","fpr"), main="ROC Curve")
arrows(0,0,1,1,lty=2, code=0)
#
# Area under the ROC curve
performance(ROC.pred,"auc")@y.values # Ideally, very close to unity.
#
#
#
#
#