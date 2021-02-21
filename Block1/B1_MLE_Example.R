### MLE estimation example ###
#
# 
# In this example, we estimate SLRM parameters 
# (Intercept, Slope, sigma)     .... sigma being the s.d.(u)
# using a maximum likelihood function
#
rm(list=ls())
#
## [1] Setup  the model: y = beta0 + beta1*X + u
#
set.seed(101) # For replicability
X <- cbind(1, runif(100))
theta.true <- c(Intercept=2,Slope=3,sigma=1) # true intercept, slope and var(u) for next line
y <- X %*% theta.true[1:2] + rnorm(100)      # sigma=1 is the default for rnorm()
head(y) # dependent variable
head(X) # matrix of regressors
plot(y~X[,2])
#
## [2] Standard OLS estimation & comparison with "actual" parameters
#
d <- summary(lm(y ~ X[,2]))
d
# estimated intercept, slope and var(u) as from OLS
theta.ols <- c(d$coefficients[,1], d$sigma)
#
# Comparison of the parameters: true vs. OLS
theta.true # True parameters of our system
theta.ols  # OLS-estimated parameters 
#
#
## [3b] Brief description of the dnorm function
#
plot(function(x) dnorm(x, log = F), -4, 4, main = "pdf of the N(0,1) distribution")
dnorm(0, mean=0, sd=1) # density (pdf)
dnorm(2, mean=0, sd=1) # density (pdf)
dnorm(-2, mean=0, sd=1) # density (pdf)
dnorm(10, mean=0, sd=1) # density (pdf)
#
## [3b] Log-Likelihood function of the linear model: y = beta0 + beta1*X + u
#
lm_lf <- function(theta, y, X) {
  sum(dnorm(y, mean = X %*% theta[-3], sd = theta[3], log = TRUE))
}
#
# [4] Experiments with the LF
#
cat("Evaluating LogL at very inacurate parameters theta : ", lm_lf(c(4,5,6), y, X), "\n")
cat("Evaluating LogL for a somewhat better theta : ", lm_lf(c(2.5,2,1.5), y, X), "\n")
cat("Evaluating LogL at true params  : ", lm_lf(theta.true, y, X), "\n")
cat("Evaluating LogL at OLS estimates: ", lm_lf(theta.ols, y, X), "\n")
# Discuss: why is the OLS-based logLik higher than logLik for the true theta?
#
#
## [5] Use MLE for parameter estimation / likelihood function lm_lf() is used /
#
?optim
# 
optim(c(5,5,5),                          # Inaccurate starting values (Intercept, slope, sigma), optimized over all parameters...
      lm_lf,                            # Likelihood function
      control=list(trace=1, fnscale=-1), # trace: progress info; fnscale=-1 for maximization...
      y=y, X=X   )                       # "..." inserted into lm_lf()
# 
# optimization repeated and saved into an object for comparison.
MLE.est <- optim(c(5,5,5),                           
                 lm_lf,                             
                 control=list(trace=1, fnscale=-1),  
                 y=y, X=X )                          
#
#
## [6] Final evaluation  
#
MLE.est$par     # MLE-estimated parameters (Intercept, Slope, sigma)
MLE.est$value   # maximized Log-Likelihood of the MLE-optimized function
#
theta.ols
logLik(lm(y ~ X[,2]))
#
#
