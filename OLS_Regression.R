# Chapter 2: The Simple Linear Regression
------------------------------------------------------------------------------------
data(ceosal1, package = 'wooldridge')
(model1 <- summary(lm(salary ~ roe, data = ceosal1)))
(ceo_regres <- summary(lm(salary ~ roe, data = ceosal1)))
with(ceosal1, plot(roe, salary, ylim = c(0,4000)))
abline(ceo_regres)

# Wooldridge, Example 2,4: Wage & Education
data(wage1, package = 'wooldridge')
(wage_reg <- summary(lm(wage ~ educ, data = wage1)))
--------------------------------------------------------------------------------------
# Wooldridge, Example 2.5: Voting Outcomes & Campaign Expenditures
data(vote1, package = 'wooldridge')
(model2 <- summary(lm(voteA ~ shareA, data = vote1)))
with(vote1, plot(shareA, voteA))
abline(model2)
--------------------------------------------------------------------------------------
# Wooldridge, Example 2.6: CEO Salary & Return on Equity
data(ceosal1, package = 'wooldridge')
sal <- ceosal1$salary
roe <- ceosal1$roe
CEOregres <- lm(sal~roe)
sal.hat <- fitted(CEOregres)
u.hat <- resid(CEOregres)
cbind(roe, sal, sal.hat, u.hat)[1:15,]
----------------------------------------------------------------------------------
# Wooldridge, Example 2.7: Wage & Education
data(wage1, package = 'wooldridge')             
WAGEregres <- lm(wage ~ educ, data = wage1)   # Model of wages to education
b.hat      <- coef(WAGEregres)                # Obtain the coefficients
wage.hat   <- fitted(WAGEregres)              # Predicted values
u.hat      <- resid(WAGEregres)               # Residuals
###
mean(u.hat)                            # Confirm property(1)
cor(wage1$educ, u.hat)                 # Confirm property(2)
mean(wage1$wage)                       # Confirm property(3)
b.hat[1] + b.hat[2] * mean(wage1$educ) # Determination of intercept
-------------------------------------------------------------------------------------
# Section 2.3 Goodness of Fit

# wooldridge, Example 2.8: CEO Salary & Return on Equity:
data(ceosal1, package = 'wooldridge')
CEOregres       <- lm(salary ~ roe, data = ceosal1) # Model of salary to roe
sal.hat         <- fitted(CEOregres)                 # Predicted values

## Calculate R-squared in one of 3 ways ##
sal   <- ceosal1$salary
var(sal.hat)/var(sal)    
1 - var(u.hat) / var(sal)
cor(sal, sal.hat)^2
-------------------------------------------------------------------------------------
# Wooldridge, Example: 2.9: Voting Outcomes & Campaign Expenditures:
data(vote1, package = "wooldridge")
(VOTEres <- summary(lm(voteA ~ shareA , data = vote1)))
-------------------------------------------------------------------------------------
# Section 2.4: Nonlinearities

# Wooldridge, Example: 2.10: Wage & Education:
data(wage1, package = 'wooldridge')
lm(log(wage) ~ educ, data = wage1)  
---

# Example 2.11: CEO Salary & Firm Sales:
data(ceosal1, package = 'wooldridge')
lm(log(salary) ~ log(sales), data = ceosal1)  
------------------------------------------------------------
# Section 2.5: Regression through the Origin & Regression on a Constant

data(ceosal1, package = 'wooldridge')
(reg1 <- lm(salary ~ roe, data = ceosal1))      # Usual OLS Regression (intercept a the mean of y)
(reg2 <- lm(salary ~ 0 + roe, data = ceosal1))  # Regression w/o intercept (through origin)
(reg3 <- lm(salary ~ 1, data = ceosal1))        # Regression w/o slope (horizontal line of a constant)

mean((ceosal1$salary))   # Average of y
plot(ceosal1$roe, ceosal1$salary, ylim = c(0,4000)) # Scatter plot wilt "roe" = y and "salary" = x
abline(reg1, lwd = 2, lty = 1)   # lwd = line width(pg. 29)| lty = line type (pg. 30)
abline(reg2, lwd = 2, lty = 2)   # line type 2 (---)
abline(reg3, lwd = 2, lty = 3)   # line type 3 (...)
legend("topleft", c("full", "through origin", "const only"), lwd = 2, lty = 1:3)
----------------------------------------------------------------------------------------------------------
# Section 2.6: Expected Values, and Standard Errors
  
# Example 2.12: Student Math Performance & the School Lunch Program:
data(meap93, package ='wooldridge')
results  <- lm(math10 ~ lnchprg, data = meap93)                 # Estimate the model
(n       <- nobs(results))                                      # Number of observations
(SER     <- sd(resid(results)) * sqrt((n-1)/(n-2)))             # *** What is SER??? ***
SER/sd(meap93$lnchprg)/sqrt(n-1) * sqrt(mean(meap93$lnchprg^2)) # Standard Error of b0hat & b1hat, respectively
SER/sd(meap93$lnchprg)/sqrt(n-1)

## Automatic calculations ##
summary(results)
-------------------------------------------------------------------------------------------------------------------
# Section 2.7: Monte Carlo Simulations

# Example with one sample:
set.seed(1234567)      # Set the number of seed
n <- 1000              # Sample size
b0=1; b1=0.5; su=2     # Set true parameters: betas and sd of u

x <- rnorm(n,4,1)      #|
u <- rnorm(n,0,su)     #| Draw a sample of size n
y <- b0 + b1*x + u     #|

(olsres <- lm(y ~ x))  # Estimate parameters by OLS
mean(x^2)              # Used for variance formula later
sum((x-mean(x))^2)     # Used for variance formula later

plot(x,y,col = "grey", xlim = c(0, 8))  # Plot of x and y
abline(b0, b1, lwd = 2)
abline(olsres, col = "grey", lwd = 2)
legend("topleft", c("pop. regr. fct.", "OLS regr. fct."), lwd = 2, col = c("black", "grey"))



