#### Repetition of basic estimation using R #### 
#
# file    BARIUM
# [Wooldridge 5th Edition],  Example 10.5
# [Wooldridge 5th Edition], Example 10.11
# [Wooldridge 5th Edition], Example 12.3
# [Wooldridge 5th Edition], Example 12.4       
#
# Aim: repetition of event study, seasonal dummies, 
#      testing of serial correlation, 
#      FGLS in the case of serial correlation, 
#      Cochrane-Orcutt and Prais-Winsten transformation
#
#
### Data
rm(list=ls())
load('barium.RData') # Read the data
desc # metadata
str(data) 
# According to Wooldridge, the dataset is from 1978:02 to 1988:12 (monthly)
barium.ts <- ts(data, start = c(1978,2), frequency = 12)
library(zoo) # install.packages("zoo", dependencies =T)
barium <- zoo(barium.ts)
barium
barium$time <- index(barium)
# Plot China imports over time
library(ggplot2) # install.packages("ggplot2",, dependencies =T)
g1 <- ggplot(barium,aes(x=time,y=chnimp)) + 
       geom_line()  +
       geom_point()
g1
# Plot gasoline production over time
g2 <- ggplot(barium,aes(x=time,y=gas)) + 
      geom_line(color="red")  +
      geom_point()
g2
# Plot China imports as a function of gasoline production
g3 <- ggplot(barium,aes(x=gas,y=chnimp)) + 
      geom_point() +
      geom_smooth(method='lm',color="red",formula=y~x)
g3
#
## Assignment: Plot china imports against chemical production index (use log-log)
#
#
### [Wooldridge 5th Edition],  Example 10.5
lm.10.5<-lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6, 
            data=barium)
summary(lm.10.5)
library(lmtest) # install.packages("lmtest", dependencies =T)
bgtest(lm.10.5, order = 1)
#
#
### [Wooldridge 5th Edition], Example 10.11
# Examine the influence of monthly seasonality
lm.10.11<-lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 
             + affile6 + afdec6 + feb + mar + apr + may 
             + jun + jul + aug + sep + oct + nov + dec, 
             data=barium)
summary(lm.10.11)
bgtest(lm.10.11, order = 1)
# Compare the two models: with and without monthly dummies
anova(lm.10.11,lm.10.5)
#
# Examine the influence of quaterly seasonality
lm.10.11.Q<-lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 
             + affile6 + afdec6 + spr + sum +fall, 
             data=barium)
summary(lm.10.11.Q)
# Compare the two models: with and without quarterly dummies
anova(lm.10.11.Q,lm.10.5)
#
# Examine the influence of trend
lm.10.11.T<-lm(lchnimp ~ lchempi + lgas + lrtwex + befile6 
             + affile6 + afdec6 + t, data=barium)
summary(lm.10.11.T)
# Compare the two models: with and without time trend
anova(lm.10.11.T,lm.10.5)
#
bgtest(lm.10.11.T, order = 1)
#
#
### [Wooldridge 5th Edition], Example 12.3
# Test for AR(3)
bgtest(lm.10.5, order = 3)
bgtest(lm.10.11.T, order = 3)
#
#
### [Wooldridge 5th Edition], Example 12.4
# Prais-Winsten and Cochrane-Orcutt estimation
library(prais) # install.packages("prais", dependencies = T)
# P-W for lm.10.5
PW.10.5 <- prais_winsten(lchnimp ~ lchempi + lgas + lrtwex + befile6 + affile6 + afdec6, 
              data=barium)
summary(PW.10.5)
# P-W for lm.10.11.T
PW.10.11.T <- prais_winsten(lchnimp ~ lchempi + lgas + lrtwex + befile6 
                            + affile6 + afdec6 + t, data=barium)
summary(PW.10.11.T)
#
library(orcutt) # install.packages("orcutt", dependencies = T)
# C-O for lm.10.5
summary(cochrane.orcutt(lm.10.5))
#
# C-O for lm.10.11.T
summary(cochrane.orcutt(lm.10.11.T))
#
#
# Extension to the Barium model
#
library("forecast") #install.packages("forecast")
?ndiffs 
ndiffs(barium$lchnimp, alpha=0.05, test="adf")
ndiffs(barium$lchempi, alpha=0.05, test="adf")
ndiffs(barium$lgas, alpha=0.05, test="adf")
ndiffs(barium$lrtwex, alpha=0.05, test="adf")
#
barium$d.lchempi <- diff(barium$lchempi) # stationarize by f.diffs
barium$d.lrtwex <- diff(barium$lrtwex)
head(barium)
#
#
# Estimate the lm.10.5 model using amended regressors (1st diffs)
lm.10.5.A <- lm(lchnimp ~ d.lchempi + lgas + d.lrtwex + befile6 + affile6 + afdec6, 
                data=barium)
summary(lm.10.5.A) # mind the interpretation of differenced logs of regressors
bgtest(lm.10.5.A, order =1)
# Re-estimate model due to AR in residuals
summary(cochrane.orcutt(lm.10.5.A))
# rho estimate by the C-O method
cochrane.orcutt(lm.10.5.A)
#
#
# Estimate the lm.10.11.T model using amended regressors (1st diffs)
lm.10.11.TA <- lm(lchnimp ~ d.lchempi + lgas + d.lrtwex + befile6 + 
                    affile6 + afdec6 +t, data=barium)
summary(lm.10.11.TA)
bgtest(lm.10.11.TA, order = 1) # mind the interpretation of differenced logs of regressors
# Re-estimate model due to "mild" AR in residuals
summary(cochrane.orcutt(lm.10.11.TA))
summary(cochrane.orcutt(lm.10.5.A))