#### Chow test (Chow 1)
#
#
#
#
## Example 1
## based on artificial data
rm(list=ls())
x <- 1:100
y <- numeric(100)
y[1:50] <- 2*x[1:50]
y[51:100] <- 85+(0.3*x[51:100]) # y = constant here
plot(x,y, col="red")
set.seed(99123)
z <- rnorm(100,0,18)
y <- y+z # add noise to y observations
lines(x,y, type = "p")
# LM model using all observations
m1 <- lm(y~x)
abline(m1,lwd=2,col="blue")
#
#
library(strucchange)
?sctest
# Compare the 
sctest(y ~ x, type = "Chow", point = 51)
#
sctest(y ~ x, type = "Chow", point = 9)
#
# Repeat the test for multiple "breakpoints" and plot results:
?Fstats
FS <- Fstats(y ~ x, from = 9, to =91)
# F-statistics calculated for different breakpoints
FS$Fstats
# https://www.jstatsoft.org/article/view/v007i02
?plot.Fstats
plot(FS)
plot(FS, pval = T) # p-val plotted
#
#
# Alternative approach - use Chow 1 test (for a given potential break-point)
library(gap)
?chow.test
# split depvar observations into two time periods
y1 <- y[1:50]
y2 <- y[51:100]
# make regressor matrices for the two time periods
x1 <- matrix(x[1:50],ncol=1)
x2 <- matrix(x[51:100],ncol=1)
#
chow.test(y1,x1,y2,x2)
#
#
#
########################################################################
#
## Example 2
## Based on the Phillips dataset
rm(list=ls())
phillips <- read.csv('phillips.csv') # Read the data
phillips <- phillips[complete.cases(phillips),] # removes the first obs with missing data
phillips.ts <- ts(phillips, start = 1949, frequency = 1)
library(zoo)
phillips <- zoo(phillips.ts)
# prepare first differences and lags for estimation
phillips$d.inf <- diff(phillips$inf)
phillips$d.unem <- diff(phillips$unem)
phillips$d.inf_1 <- lag(diff(phillips$inf),-1)
head(phillips)
phillips <- phillips[complete.cases(phillips),]
phillips.ts <- ts(phillips, start = 1951, frequency = 1)
#
# Model data:
# Annual TS, 1951 - 2003
#
# inf      - percentage change in CPI
# unem     - unemployment rate (in %)
#
#
#
# Basic data plots
plot.ts(phillips.ts[,"d.inf"], type = "l" )
lines(phillips.ts[,"d.unem"], type = "l", col="red")
legend("topright", legend=c("d.inf", "d.unem"), 
       pch=16, col=c("black", "red"))
#
# Model estimation: (Expectations-Augmented Phillips Curve from Week 01)
#
summary(lm(d.inf ~ unem, data=phillips.ts))
#
#
## Assignment 1
## Perform the Chow1 test for breakpoint in 1981
## .. use "point=c(1981,1)" where the "1" denotes annual frequency ( we would use 4 for quarterly data, etc.)
## .. use "data=phillips.ts" as the sctest() command does not handle zoo objects



## Assignment 2
## Use the Fstats() command to 
## search for a break-point in the data
## .. use "from = 9, to = 44" 
## .. use "data=phillips.ts"



## Alternative approach - using the {gap} package
#
library(gap)
# ?chow.test
# split depvar observations into two time periods
y1 <- as.vector(phillips[phillips$obs<=1980,"d.inf"]) # str(y1)
y2 <- as.vector(phillips[phillips$obs>1980,"d.inf"])  # str(y2)
# make regressor matrices for the two time periods
x1 <- matrix(phillips[phillips$obs<=1980,"unem"],ncol=1)
x2 <- matrix(phillips[phillips$obs>1980,"unem"],ncol=1)
#
chow.test(y1,x1,y2,x2)


