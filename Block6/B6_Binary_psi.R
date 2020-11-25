#### Fitted values / predi Logit ####
#
# 
# 
# 
#
#
### Example: Limited dependent variables  
#    
#    
#   
#
# Data
rm(list=ls())
psi <- read.csv('psi.csv')
plot(psi)
#
#
# Model data:
#
# GRADE  - binary dependent variable, 1 if "grade in Economy" has improved, 0 otherwise
# TUCE   - points scored in Test of Understanding in College Economics
# GPA    - Grade point average <0,4>; 4 is the best average achievable
# PSI    - Personalized system of instruction; 1 if PSI used, 0 if PSI not used
#
#
# Logistic regression
#
logit.fit<-glm(GRADE ~ TUCE+GPA+PSI, family=binomial(link = "logit"), data=psi)
summary(logit.fit)
pred.Grade <- predict(logit.fit, type="response") 
#
# We want to assess (predict) the influence of PSI participation
# for different GPA levels (from 2 to 4). As we are not really interested 
# in TUCE values, we may fix TUCE at its mean (other fixed levels may be used as well)
#
# 
# First, we need to make regressor observations for plotting.
# We make two separate datasets, one for PSI = 0, other for PSI = 1.
#
# Make a new dataset including values of the regressors (PSI = 0):
# We start by creating an empty dataset.
# .. Ad-hoc, we choose to have 41 "artificial" observations
psi2 <- as.data.frame(matrix(rep(0, 123), ncol=3)) 
colnames(psi2) <- c("TUCE", "GPA", "PSI")
# Second, include values for TUCE and GPA
# .. PSI already set to 0 on line 39.
psi2$TUCE <- mean(psi$TUCE)
psi2$GPA <- seq(from = 2, to = 4, length.out = 41)
head(psi2)
#
# Now, we replicate the psi2 dataset for alternative predictions,
# but we set PSI = 1:
psi3 <- psi2
psi3$PSI <- 1
head(psi3)
#
# Lets make two alternative forecasts as specified above,
# .. the regressors only differ in the PSI: 0 or 1
psi.0.predict <- predict(logit.fit, type="response", newdata=psi2)
psi.1.predict <- predict(logit.fit, type="response", newdata=psi3)
#
plot(psi.1.predict~psi2$GPA, type="l", col = "red", xlim = c(2,4), ylim = c(0,1),
     xlab = "GPA", ylab = "GRADE prediction")
lines(psi.0.predict~psi2$GPA, type="l", col="darkgreen")
abline(v = 3.5, lty = 2)
legend("topleft", legend=c("PSI = 1", "PSI = 0"), 
       pch=16, col=c("red", "darkgreen"))
#
## Assignment 1
## What is the interpretation of the following "table"?
## How does it relate to the plot just produced?
#
GRADE_preds <- cbind(psi2$GPA, psi.1.predict - psi.0.predict)
colnames(GRADE_preds) <- c("GPA", "GRADE_hat difference")
head(GRADE_preds)
tail(GRADE_preds, 11)
#
#
## Assignment 2:
## Amend the above code (starting on line 45), to find the following:
## What is the expected effect (on GRADE_hat) of changing PSI from 0 to 1 at GPA = 3,
## if we fix TUCE at its observed maximum value (instead of using mean)?
## 
#
#
#
#