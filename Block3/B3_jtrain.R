#### FE estimator #### 
#
# 
# 
# 
#
#
### Example 14.1: Based on Wooldridge: Introductory econometrics, 
#    
#   Effect of Job Training on Firm Scrap rates
#   
#
# Data
rm(list=ls())
jtrain <- read.csv('jtrain.csv') # Read the data

#
# Model data:
#
# scrap    - scrap rate, per 100 items
# d88      - binary, = 1 if year == 1988
# d89      - binary, = 1 if year == 1989
# grant    - binary, = 1 if company received a grant
# grant_1  - lagged grant, assumed 0 in 1987
# fcode    - firm code number
# year     - 1987, 1988 or 1989
#
plot(jtrain[,c(1,2,6,10,11,12)])
boxplot(jtrain$scrap ~ jtrain$year)
boxplot(jtrain$scrap ~ jtrain$grant)
#
#
library(plm) # install.packages("plm")
#
model1 <- plm(log(scrap) ~ d88 + d89 + grant + grant_1,
            data=jtrain,
            index=c('fcode','year'),
            model='within', # FE
            effect='individual') # We assume there are no time-specific effects.
#
summary(model1)
#
# Test for significance of the individual and/or time effects for panel models
plmtest(model1, effect="individual") # H0: individual effect are not significant
#
#
#