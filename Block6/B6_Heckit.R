#### LPM, Logit, Probit, Tobit, Sample selection correction (Heckit) ####
#
# 
# 
# 
#
#
#
# Data
rm(list=ls())
mroz<-read.csv('mroz.csv')
#
#
#
### Example 17.5: Based on Wooldridge: Introductory econometrics, 
#    
#   Sample selection correction (Heckit)
#   
#
library(sampleSelection) # install.packages("sampleSelection")
?heckit
#
# Two-step estimation
Heckit.2step <- heckit(inlf ~ educ + exper + expersq +nwifeinc + age + kidslt6 + kidsge6, 
                       log(wage) ~ educ + exper + I( exper^2 ), 
                       data = mroz, method = "2step" )
summary(Heckit.2step)
#
# ML estimation
Heckit.ML <- heckit(inlf ~ educ + exper + expersq +nwifeinc + age + kidslt6 + kidsge6,
                    log(wage) ~ educ + exper + I( exper^2 ), 
                    data = mroz, method = "ml" )
summary(Heckit.ML)
#
#
#
#