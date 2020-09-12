#### Unit root test, Cointegration test, ECM #### 
#
# 
# 
# 
#
#
### Examples 18.2, 18.6 & 18.7 (amended)
#    
## Holding yield ECM construction and estimation   
#   
#
# Data
library(zoo)
rm(list=ls())
intqrt <- read.csv('intqrt.csv') # Read the data
intqrt$hy3_1 <- c(NA, intqrt$hy3[1:123]) # hy3 for (t-1)
intqrt <- intqrt[-1, ] # drop the first row, as it contains NA values
intqrt.ts <- ts(intqrt, start=c(1958,2), frequency = 4)
intqrt <- zoo(intqrt.ts)
#
# Model data:
# Quarterly TS, starting from 1958Q2 (possibly innacurate start date)
#
# 
# r3     - bond equivalent yield, 3-month T-bill
# p3     - price of 3-month T-bill (at quarter end)
# p6     - price of 6-month T-bill (at quarter end)
# hy3    - holding yield:  r3*(91/365)
#          % yield from buying a 3-month T-bill at (t-1), i.e. previous quarter
# hy6    - holding yield:  100*(p3 - p6[t-1])/p6[t-1])
#          % yield from buying a 6-month T-bill at (t-1) and 
#          selling it as a 3-month T-bill at time (t).
#
# 
# Basic data plots
plot(intqrt[,c("hy6","hy3_1")])
plot(intqrt[,c("hy6","hy3_1")],plot.type = "single", col = c("black","red"))
plot(intqrt$hy6 ~ intqrt$hy3_1, type = "p" )
abline(lm(hy6 ~ hy3_1, data=intqrt), col="red")
#
#
#
# Preliminary step 1 of ECM construction and estimation: 
# Are the series integrated and what is the integration order?
#
library(urca) # install.packages("urca")
#
# Unit root tests for hy6 and hy3 - LEVELS
summary(ur.df(intqrt$hy6, type="none", selectlags="BIC"))
summary(ur.df(intqrt$hy6, type="drift", selectlags="BIC"))
summary(ur.df(intqrt$hy6, type="trend", selectlags="BIC"))
#
summary(ur.df(intqrt$hy3, type="none", selectlags="BIC"))
summary(ur.df(intqrt$hy3, type="drift", selectlags="BIC"))
summary(ur.df(intqrt$hy3, type="trend", selectlags="BIC"))
#
# Unit root tests for for hy6 and hy3 - 1st DIFFERENCES
summary(ur.df(diff(intqrt$hy6), type="none", selectlags="BIC"))
summary(ur.df(diff(intqrt$hy6), type="drift", selectlags="BIC"))
summary(ur.df(diff(intqrt$hy6), type="trend", selectlags="BIC"))
#
summary(ur.df(diff(intqrt$hy3), type="none", selectlags="BIC"))
summary(ur.df(diff(intqrt$hy3), type="drift", selectlags="BIC"))
summary(ur.df(diff(intqrt$hy3), type="trend", selectlags="BIC"))
#
# 
#
#
#
# Preliminary step 2 of ECM construction and estimation: 
# Are the series co-integrated?
#
# Phillips & Ouliaris Cointegration Test
?ca.po # H0: series are not cointegrated
PO1.test <- ca.po(intqrt[,c("hy6","hy3_1")], lag="long", type="Pz", demean="none")
summary(PO1.test) # Reject H0
PO2.test <- ca.po(intqrt[,c("hy6","hy3_1")], lag="long", type="Pz", demean="constant")
summary(PO2.test) # Reject H0
PO3.test <- ca.po(intqrt[,c("hy6","hy3_1")], lag="long", type="Pz", demean="trend")
summary(PO3.test) # Reject H0
#
#
#
#
# Engel-Granger two-phase method for ECM estimation
# Phase 1: Estimation of the Cointegration Equation (CE: Long-term dependence)
#
CE.mod <- lm(hy6 ~ hy3_1, data=intqrt)
summary(CE.mod)
#
# Save the Error Correction element for use in the ECM equation:
intqrt$EC <- CE.mod$residuals
#
#
# Engel-Granger two-phase method for ECM estimation
# Phase 2: Estimation of the ECM (short term dynamics)
# 
library(dyn) # install.packages("dyn")
intqrt$d.hy6 <- diff(intqrt$hy6)  # Observed first diffs. of hy6
#
ECM1 <- dyn$lm(d.hy6 ~ diff(hy3_1) + lag(EC, -1), data = intqrt) 
summary(ECM1) 
#
# Save fitted values for subsequent analysis
intqrt$ECM.fitted <- c(NA,ECM1$fitted.values)  # Fitted values from the ECM model
#
#
#
#
# Task 1: Does the ECM approach improve prediction properties over a
#         basic Short-Term dynamic Model (fist differences)?
#
# Estimate the simpler/alternative model specification 
# in first differences 
STM <- dyn$lm(d.hy6 ~ diff(hy3_1), data = intqrt) 
summary(STM) 
#
# Compare the two models using anova:
anova(STM, ECM1)
# Use RMSE (Root mean square error) to evaluate predictions
intqrt$STM.fitted <- c(NA,STM$fitted.values)
test.df <- intqrt[3:123, c("d.hy6", "ECM.fitted", "STM.fitted")]
#
RMSE.ECM1 <- sqrt(mean((test.df[,1]-test.df[,2])^2))
RMSE.STM <- sqrt(mean((test.df[,1]-test.df[,3])^2))
# Compare RMSE for both models
RMSE.ECM1
RMSE.STM
#
#
#
#
#
#
# Task 2: In Example 18.7, we are libraryd to evaluate an expectations
#         hypothesis that in the Long-term model lm(hy6 ~ hy3_1, data=intqrt.ts)
#         the slope coefficient equals 1, and therefore, that the 
#         Error Correction element simplifies to: EC = hy6 - hy3_1.
#
# Calculate the EC element under H0
intqrt$EC2 <- intqrt$hy6 - intqrt$hy3_1
#
# Estimation of the model under H0:
ECM2 <- dyn$lm(d.hy6 ~ diff(hy3_1) + lag(EC2, -1), data = intqrt) 
summary(ECM2)
#
# The two ECM specifications can be evaluated in terms of RMSE of prediction
# (we can assess how restrictive is the assumption EC = hy6 - hy3_1 )
intqrt$ECM2.fitted <- c(NA,ECM2$fitted.values)
RMSE.ECM2 <- sqrt(mean((intqrt$d.hy6[3:123] - intqrt$ECM2.fitted[3:123])^2))
#
#
RMSE.ECM1
RMSE.ECM2
RMSE.STM
#
# Note that the above RMSE relate to Train sample predictions
# (out-of sample (Test sample) predictions will be discussed separately)
#
#