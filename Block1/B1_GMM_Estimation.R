#### Method of moments, GMM ####
#
# 
# Let -theta- be a m-vector of parameters that characterize
# the distribution of a random variable y.
# 
# With the method of moments, one estimates
# the components of -theta- by simply equating the
# first m population moments µ_k(-theta-) with the
# their corresponding sample moments µ-hat_k(-theta-), 
# and solving for the components in the parameter vector -theta-.
# ... assumptions apply: moments exist, random sampling, .....
#   
#
###   MM example 
rm(list=ls())
#
# We make a n=50 random draw from a normally distributed variable Y: Y ~ N(10,9), i.i.d.
set.seed(1110) # for repeatability
Y <- rnorm(50, mean=10, sd=3)
#
# The MM estimates are as follows
mean(Y) # MM estimate of the population's first moment
#
var(Y)  # MM estimate of the population's second moment... 
#
# Note the difference between population and sample moments...
#
# In our example, 
# mean(Y) and var(Y) should not be viewed as descriptive statistics for the sample,
# they are the sample-based MM estimators 
# of the vector of population parameters -theta- = (mean, var). 
# 
#
#
#
# Generalized method of moments GMM
# - we start by specifying functions that (for any DGP) depend boht on data and model parameters
# - OLS is a special case of GMM
# - the use of instrumental variables is an essential aspect of GMM 
# - HAC: heteroskedasticity & autocorrelation costistency in GMM may be achieved
#   by specifying a weighting matrix as a function of observed regressors/instruments...
#
#
#
### GMM estimation examples, based on Wooldridge, Ex. 10.9 and 12.2
#   Percentage employment rate in Puerto Rico as a function of minimum wages,
#   GNP of the USA and time trend.
#
#
# Data
rm(list=ls())
load("prminwge.RData")
desc
plot(data[,c(21,24,18,17,14)])
#
# Base OLS for comparison
OLS.PR <- lm(lprepop~lmincov+lusgnp+lprgnp+t, data = data)
summary(OLS.PR)
#
library(gmm) # install.packages("gmm")
?gmm
#
# Assuming all regressors are exogenous, 
# we only use the regressors as the "x" argument
#
# the GMM estimation may be specified as follows:
GMM.1 <- gmm(lprepop~lmincov+lusgnp+lprgnp+t, 
             x = ~lmincov+lusgnp+lprgnp+t,
             vcov="iid", # a somewhat strong assumption on u 
             data = data)
summary(GMM.1)
# compare with OLS
summary(OLS.PR)
#
# 
# If we want to control for AR and heteroskedasticity in errors,
# the GMM estimation may be specified as follows:
GMM.2 <- gmm(lprepop~lmincov+lusgnp+lprgnp+t, 
             x = ~lmincov+lusgnp+lprgnp+t,
             vcov="HAC", 
             data = data)
summary(GMM.2)
# compare to GMM.1 - estimation differs in s.e. (t-ratios and p-values)
summary(GMM.1)
#
#
# 
# ... Note: The next GMM specification is a type of instrumental variable regression (IVR),
#           discussed later in this course....
#
# If we assume lmincov is an endogenous regressor (correlated with u), plus we have 
# a variable lavgmin that is strongly correlated with lmincov and uncorrelated with u,
# the GMM estimation may be specified as follows:
# ... for illustrative purposes only, assumptions not tested here
#
GMM.3 <- gmm(lprepop~lmincov+lusgnp+lprgnp+t, 
             x = ~lusgnp+lprgnp+t+lavgmin, # note lmincov has been replaced by lavgmin
             vcov="HAC", 
             data = data)
summary(GMM.3)
# compare to the IVR estimator in {AER} package
library(AER) # install.packages("AER")
ivreg.1 <- ivreg(lprepop~lmincov+lusgnp+lprgnp+t |lusgnp+lprgnp+t+lavgmin,  data = data)
summary(ivreg.1) # (no "HAC" errors used in the IVR summary)
#
#
#
#
#
#