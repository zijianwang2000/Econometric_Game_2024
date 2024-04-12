n = 100000   # sample size

##### Random assignment

### Homogeneous treatment effects (Delta = 1)
set.seed(123)
W = rnorm(n)   # Control variables
D = rbinom(n, 1, .5)   # Binary treatment indicator Ber(p=0.5)
U = rnorm(n, 0, .7)   # Unobservables

# Generate potential outcomes given (D,W,U)
Y_1 = 1 + W + U   # Treated
Y_0 = W + U   # Not treated

# Observable outcomes
Y = D * Y_1 + (1-D) * Y_0   # Only (Y,D,W) is observed

# Use OLS estimation
lm(Y ~ D + W)

# Here, the coefficient on D is very close to the true homogeneous TE=1.
# This is proven.


### Heterogeneous treatment effects
set.seed(123)
W = rnorm(n)   # Control variables
D = rbinom(n, 1, .5)   # Binary treatment indicator Ber(p=0.5)
U = rnorm(n, 0, .7)   # Unobservables
Delta = runif(n)   # Individual treatment effect

# Generate potential outcomes given (D,W,U)
Y_1 = Delta + W + U   # Treated
Y_0 = W + U   # Not treated

# Observable outcomes
Y = D * Y_1 + (1-D) * Y_0   # Only (Y,D,W) is observed

# Use OLS estimation
lm(Y ~ D + W)

# Here, the coefficient on D is very close to the true ATE=1/2.
# This is proven.



##### Unconfoundedness (D depends on W)

### Homogeneous treatment effects

## 1. Treatment D is a deterministic function of control W
set.seed(123)
W = rnorm(n)   # Control variables
D = as.numeric(W<=0)   # Binary treatment indicator 1_{W<=0}, Ber(0.5))
U = rnorm(n, 0, .7)   # Unobservables

# Generate potential outcomes given (D,W,U)
Y_1 = 1 + W + U   # Treated
Y_0 = W + U   # Not treated

# Observable outcomes
Y = D * Y_1 + (1-D) * Y_0   # Only (Y,D,W) is observed

# Use OLS estimation
lm(Y ~ D + W)

# Here, the coefficient on D is very close to the true homogeneous TE=1.
# This is proven.


## 2. Treatment D is NOT a deterministic function of control W
set.seed(123)
W = rnorm(n)   # Control variables
D = as.numeric(W<=rnorm(n,0,1/4))   # Binary treatment indicator
U = rnorm(n, 0, .7)   # Unobservables

# Generate potential outcomes given (D,W,U)
Y_1 = 1 + W + U   # Treated
Y_0 = W + U   # Not treated

# Observable outcomes
Y = D * Y_1 + (1-D) * Y_0   # Only (Y,D,W) is observed

# Use OLS estimation
lm(Y ~ D + W)

# Here, the coefficient on D is very close to the true homogeneous TE=1.
# This is proven.


### Heterogeneous treatment effects
set.seed(123)
W = rnorm(n)   # Control variables
D = as.numeric(W<=rnorm(n,0,1/4))   # Binary treatment indicator
U = rnorm(n, 0, .7)   # Unobservables
Delta = runif(n)   # Individual treatment effect

# Generate potential outcomes given (D,W,U)
Y_1 = Delta + W + U   # Treated
Y_0 = W + U   # Not treated

# Observable outcomes
Y = D * Y_1 + (1-D) * Y_0   # Only (Y,D,W) is observed

# Use OLS estimation
lm(Y ~ D + W)

# Here, the coefficient on D deviates more from the true ATE=1/2,
# since we only estimate an average weighted version of treatment effects.
# Alternatively, one can use propensity score matching, leading to right ATE.


##### IV assumptions 
library(MASS)
set.seed(123)
help.mat = mvrnorm(n, c(0,0,0), matrix(c(1,.5,.3,.5,1,0,.3,0,1),3,3))
X = as.numeric(help.mat[,1]<=0)   # Binary treatment indicators
Z = as.numeric(help.mat[,2]<=0)   # Binary instrument
U = help.mat[,3]

cov(X,Z)   # relevance
cov(X,U)   # endogeneity
cov(U,Z)   # uncorrelated with error

# Potential outcomes given (X,U)
Y_1 = 1 + U
Y_0 = U

Y = X * Y_1 + (1-X) * Y_0   # Observed outcomes

lm (Y ~ X)
# Endogeneity problem: Coefficient on X is close to 1/2, 
# but actual treatment effect is 1.

library(AER) 
ivreg(Y ~ X | Z)
# Using IV, we are back to true treatment effect (since Delta homogeneous!)
