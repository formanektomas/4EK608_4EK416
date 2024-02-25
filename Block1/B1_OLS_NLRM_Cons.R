#### OLS and Non-linear regression models ####
#    
#
# Example based on
# Greene: Econometric Analysis (5th ed.), 2002, pp 171-173 (Ch. 9.3)
# or
# Greene: Econometric Analysis (7th ed.), 2012, pp 231-233 (Ch. 7.2.5)
#
# Data description & source:
# Table F5.2: Macroeconomics Data Set, Quarterly, 1950I to 2000IV, 
# 204 Quarterly Observations 
# Source: Department of Commerce, BEA website and www.economagic.com
#
# Year = Date
# Qtr = Quarter
# Realcons = Real consumption expenditures
# Realdpi = Real disposable personal income
# Realgdp = Real GDP ($bil)
# Realinvs = Real investment by private sector
# Realgovt = Real government expenditures
# CPI_U = Consumer price index
# M1 = Nominal money stock
# Tbilrate = Quarterly average of month end 90 day t bill rate
# Unemp = Unemployment rate
# Pop = Population, mil. interpolate of year end figures using constant growth rate per quarter
# Infl = Rate of inflation (first observation is missing)
# Realint = Ex post real interest rate = Tbilrate - Infl. (First observation missing)
#
# 
#
#
#
#
# Data + basic plots
rm(list=ls())
cons.dat <- read.csv('TableF5-2.csv')
#
# OLS method
library(ggplot2) # install.packages("ggplot2")
ggplot(cons.dat, aes(x=REALDPI,y=REALCONS) ) + 
  geom_point() +
  geom_smooth(method= "lm") +
  ggtitle("REALCONS ~ REALDPI by OLS") 
# 
# Non-linear fitting
library(ggplot2) # install.packages("ggplot2")
ggplot(cons.dat, aes(x=REALDPI,y=REALCONS) ) + 
  geom_point() +
  geom_smooth() + # LOESS (locally estimated scatterplot smoothing)
  ggtitle("REALCONS ~ REALDPI: non-linear fitting") 
#
#
#
# Model specification
#
# REALCONS = alpha + beta*(REALDPI^gama)
# Note1 - the linear model is a special case of the NLM
# for: gama = 1
# Note2 - model setup follows Greene (2002), we focus
# on OLS vs NLRM, other TS-related topics are ignored at this point
#
#
#
# STEP 1 - Fit a simple OLS model
OLS.1 <- lm(REALCONS ~ REALDPI, data = cons.dat)
summary(OLS.1)
#
#
#
#
# STEP 2 - Fit a NLRM
NLS.1 <- nls(REALCONS ~ A+B*(REALDPI^C), 
             start = list(A=1, B = 1, C=1), 
             data = cons.dat )
summary(NLS.1)
#
#
# With more "realistic" starting estimates, 
# the number of iterations may be somewhat reduced:
NLS.2 <- nls(REALCONS ~ A+B*(REALDPI^C), 
             start = list(A=500, B = 0.2, C=1), 
             data = cons.dat )
summary(NLS.2)
# Pseudo R2
(cor(cons.dat$REALCONS, NLS.2$m$fitted()))^2
#
#
#
# MPC - marginal propensity to consume
#
# The first derivation of of our function:
# REALCONS = alpha + beta*(REALDPI^gama)
# along REALDPI 
#
# equals: beta*gama*(REALDPI^(gama-1))
#
# Say, we want to test the H0 of MPC = 1 for the last observation, 2000IV
# This may be done using the Delta method
# the following calculation replicates results of Greene(2002), p 173
#
# a) MPC depends on REALDPI: caluclate MPC(t) and s.e.(MPC(t))
B <- coefficients(NLS.2)[2]
C <- coefficients(NLS.2)[3]
MPC <- B*C*(cons.dat$REALDPI^(C-1))
MPC[204] # MPC for the obs 2000IV
#
# b) s.e. using the delta method for the obs 2000IV
VCV.M <- vcov(NLS.2)[2:3,2:3]
# [(der.MPC/der.B),(der.MPC/der.C)
vec.der <- c(C*(cons.dat$REALDPI[204]^(C-1)),(B*(cons.dat$REALDPI[204]^(C-1)))*(1+C*log(cons.dat$REALDPI[204])))
#
s.e. <- sqrt(vec.der %*% VCV.M %*%vec.der)
#
print(s.e.)
#
# c) calculate a z-score under H0:
(MPC[204]-1)/s.e.
# By comparing to the critical values of N(0,1) distribution,
# we reject H0.
#
#
# Alternatively, we get the same results using DeltaMethod()
# from the {RcmdrMisc} package
library(RcmdrMisc)
?DeltaMethod
obs2000IV <- cons.dat$REALDPI[204]
DeltaMethod(NLS.2,"B*C*(obs2000IV^(C-1))")
# or by deltaMethod() from {car}
# deltaMethod(NLS.2,"B*C*(obs2000IV^(C-1))")
#
#
#