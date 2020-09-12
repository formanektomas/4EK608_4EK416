# EXAMPLE 17.10 from  Gujarati, Basic Econometrics, 4th ed.
# 
# 
rm(list=ls())
SriLanka <- read.csv("SriLanka.csv")
#
# PCON = private consumption expenditure.
# GDP = gross domestic product
# (mio rupees, constant prices, years 1967-1993)
# The data are obtained from the data disk in Chandan Mukherjee, Howard White, and
# Marc Wuyts, Econometrics and Data Analysis for Developing Countries, 
# Routledge, New York, 1998. The original data is from World Bank’s World Tables.
#
#
# In our model, PCON <- GDP
#
# lm model, ignores non-exogeneity of PCON_1 in some transformations
#           (Koyck, AEH)
summary(lm(PCON~GDP+PCON_1,data=SriLanka))
#
#
#
#
library(dLagM)
#
?koyckDlm
# note that "instrumental variables estimation is employed to fit the model"
# .. IVR will be discussed separately (Week 8)
Koyck <- koyckDlm(x=SriLanka[,"GDP"] , y=SriLanka[,"PCON"])
Koyck
summary(Koyck)
#
#
#
#
## Assignment: 
##
## Use the "Koyck" model /estimated by the koyckDlm()/ and:
##
## 1 Assume PAM & interpret the results (SR and LR propensities)
## 2 Assume AEH & interpret the results (SR and LR propensities)
## 3 Assume geometric decay in parameters of the IDL model & interpret the results
##   (SR and LR propensities)
##
## Bonus assignment
##
## 4 Use the SriLanka dataset to estimate a RDL (rational distributed lags) model
##   .. before estimation, you need to "produce" first lags of GDP: GDP_1,
##      various approaches are possible, e.g. ts() transformation.
##