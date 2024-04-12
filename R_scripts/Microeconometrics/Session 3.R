### Binary response

library(MASS)
library(ggplot2)

# Generate random data
set.seed(321)
n = 100   # Number of observations
x = runif(n)
# P(y=1|x)=Phi(2x), beta = (0,2)
y = rbinom(n, 1, pnorm(2*x))  

# Fit linear model (misspecified)
linear_model = lm(y ~ x)
summary(linear_model)

# Fit probit model
probit_model = glm(y ~ x, family=binomial(link="probit"))
summary(probit_model)

# Fit logit model
logit_model = glm(y ~ x, family=binomial(link="logit"))
summary(logit_model)

# Compute marginal effects (wrt. x_2)
linear_model_me = coef(linear_model)[2]
probit_model_me = dnorm(coef(probit_model)[1]+coef(probit_model)[2]*x)*coef(probit_model)[2]   # heterogeneous
probit_model_ame = mean(probit_model_me)
logit_model_me = dlogis(coef(logit_model)[1]+coef(logit_model)[2]*x)*coef(logit_model)[2]   # heterogeneous
logit_model_ame = mean(logit_model_me)


### Multinomial response

# Multinomial / Conditional logit
# Two alternative-specific variables (price and catch)
# One individual-specific variable (income)
# Four fishing modes: beach, pier, boat, charter

library(mlogit)
data("Fishing", package = "mlogit")
Fish = dfidx(Fishing, varying=2:9, shape="wide", choice="mode")
#str(Fish)
m = mlogit(mode ~ price + catch | income, data=Fish)
summary(m)


### Censored regression 
library(censReg)

# Generate random data
set.seed(321)
n = 100   # sample size
x = rnorm(n)   # covariates
# beta = (1,2), u~N(0,1)
y = rnorm(n, mean=1+2*x)   # latent outcomes
censored_y = y * as.numeric(y>=1)   # c=1
cbind(y, censored_y)

# Fit linear regression model on uncensored data (cheating)
uncensored_model = lm(y ~ x)
summary(uncensored_model)

# Fit linear regression model ignoring censoring
ig_censored_model = lm(censored_y ~ x)
summary(ig_censored_model)   # inconsistent

# Fit the correct censored regression model
censored_model = censReg(censored_y ~ x, left=1)
summary(censored_model)

# Plot the results (see solutions)


### Sample selection model (class: something is wrong here!)
library(sampleSelection)

# Generate data
set.seed(123)
n = 100
x = rnorm(n)   # covariates
t = rbinom(n, 1, 0.5)   # selection indicator
u = rnorm(n)   # error term for outcome equation
y_star = 1 + 2*x + 0.5*t + u   # latent outcome
y = ifelse(y_star>0, y_star, 0) # can reformulate selection indicator!!!

# Heckman two-stage
heckman_model = heckit(outcome = y ~ x, selection = t ~ x)
summary(heckman_model)
# often in practice, no significant result, similar to IVs

cbind(y,t)


### Sample selection model (own example)
library(sampleSelection)
library(mvtnorm)

# Generate data
set.seed(123)
n = 150
x = rnorm(n)   # covariates
z = 0.7*x + rnorm(n, -1, 0.5)   # IV

mean_vec <- c(0, 0)
cov_mat <- matrix(c(0.36, 0.2, 0.2, 1), nrow = 2)
uw = rmvnorm(n, mean_vec, cov_mat)
u = uw[1:n, 1]   # error term for outcome equation
w = uw[1:n, 2]   # error term for selection equation

y_star = 1 - 2*x + u   # latent outcome, beta=(1,-2)
t = as.numeric(z >= w)   # selection indicator, gamma=(0,1)  
selected = sum(t)
y = ifelse(t==1, y_star, 0)

# Heckman two-stage
heckman_model = heckit(outcome = y ~ x, selection = t ~ z)
summary(heckman_model)
