####  Bootstrapping  ####
#
#
#
## Bootstrap example 1:                                          
## Standard error, confidence interval and histogram for median 
#
#
# We shall use the Wage data from the ISLR package
# to calculate sample wage median and its bootstrapped 
# standard errors and 95% confidence interval
#
rm(list=ls())
library("ISLR") # install.packages("ISLR")
wageData <- Wage
#
# For simplicity, we shall treat our 3000 observations as a population
# and we shall shall select a sample of 100 workers at random:
wages.population <- wageData$wage
#
?sample
set.seed(450)
random.row.number <- sample(3000, 100, replace = FALSE)
wages <- wageData[random.row.number, "wage"]
#
# Now, we have two vectors containing wages:
# wages.population - contains wage data for all 3000 workers
# wages            - contains wage data for 100 workers selected at random
#
## Before we use medians, let's estimate mean and sd(mean)
cat("The population mean wage is", mean(wages.population), "(i.e. true mean)")
cat("The sample mean wage is", mean(wages))
# Population mean is fixed, hence its variance is zero.
# Sample mean is a random variable, hence its variance / s.e. is of interest.
# .. the variance/s.d. of sample mean may be obtained analytically:
# .. var(mean(x)) = var(x)/sample.size
cat("The estimated variance of sample mean is", var(wages)/100)
cat("The estimated s.e. of sample mean is", sd(wages)/10)
#
#
#
#
#
# Population and sample medians
cat("The population median wage is", median(wages.population))
cat("The sample median wage is", median(wages))
# Population median is fixed, hence its variance is zero.
# Sample median is a random variable, hence its s.e. is of interest.
# .. the variance/s.e. of sample median CANNOT be obtained analytically.
# .. Under some basic assumptions (representative & random draw sample)
# .. we may use Bootstrap to find out the s.d. of sample median as follows:
#
library("boot") # install.packages("boot")
?boot()
#
samplemedian <- function(x, d) {
  return(median(x[d]))
}
# The notation x[d] allows us to make a new vector 
# (the bootstrap sample), which is given to the median(). 
# This reflects sampling with replacement from the original data vector.
#
set.seed(400)
b <- boot(wages, samplemedian, R=500)  # 500 replications
# Let's view the bootstrap output
b
plot(b)
#
#
#
#
#
#
#### Bootstrap example 2: estimating standard errors of coefficients in a LRM
####                      with CLRM assumptions violated
#
#
rm(list=ls())
library("car") # install.packages("car")
library("ISLR")
Auto <- Auto # 392 observations
# Let's use only the first 50 observations from the "Auto"
# .. this way, we cannot rely on asymptotics for interpretation
# of the calculated s.e. of the OLS coefficients.
Auto <- Auto[1:50, ]
head(Auto)
# Now, we shall estimate a simple LRM:
fit.1 <- lm(mpg~horsepower, data = Auto)
summary(fit.1)
coef(lm(mpg~horsepower, data = Auto))
# Assumptions made for estimation of the 
# standard errors of coefficients:
# .. disturbances follow Normal distributions
# .. "sigma" estimate relies on the assumption of correct model specification
# .. regressors are assumed to be uncorrelated with the random error
#
# Let's test for normally distributed residuals, using the
# Shapiro–Wilk test, with a H0 hypothesis of normality.
shapiro.test(fit.1$residuals) # H0 is rejected..
#
# The asymptotic normality of coefficients' distributions
# librarys approx. n > 100 observations.
#
#
# We can use bootstrap to evaluate accuracy
# of the estimated standard errors
#
# 1 #
boot.fn = function(dataset, index) {
  return(coef(lm(mpg ~ horsepower, data=dataset, subset=index)))
}
# Do a 1000 fold bootstrap sampling
# and corresponding 1000 fold estimation of the model...
# This results in bootstrapped standard errors of coefficients:
set.seed(1)
boot.1 <- boot(Auto, boot.fn, 1000)
boot.1
# 
#
library(lmtest) #install.packages("sandwich")
library(sandwich) #install.packages("sandwich")
#
#
#
# Bootstrapped standard errors
boot.1
# Bootstrapped standard errors may be compared to:
# OLS-based standard errors
coeftest(fit.1)
# and to heteroscedasticity-corrected standard errors (White)
coeftest(fit.1, vcov=vcovHC(fit.1,type='HC0'))
#
#