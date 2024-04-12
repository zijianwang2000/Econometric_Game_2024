library(gmm)

# Model: y = beta_0 + x*beta_1 + e, exogeneity, sigma² = E(e²)
# Moments: E[y-beta_0-beta_1*x]=0, E[(y-beta_0-beta_1*x)x]=0, E[(y-beta_0-beta_1*x)²-sigma²]=0

moment_cond = function(theta, data){
  x = data$x
  y = data$y
  
  moment1 = (y - theta[1] - theta[2] * x)
  moment2 = (y - theta[1] - theta[2] * x) * x
  moment3 = (y - theta[1] - theta[2] * x)^2 - theta[3]
  
  return(cbind(moment1, moment2, moment3))
}

# Generate data
set.seed(123)
n = 100
x = rnorm(n)
e = rnorm(n, 0, sqrt(.5))
y = 2 + 3 * x + e

# Want to estimate theta = (2, 3, 0.5)
data = data.frame(x=x, y=y)

# Estimate GMM
# 1st step: Usual OLS -> residuals, estimate optimal weight matrix
# 2nd step: By default, gmm uses the inverse of the estimated 
# covariance matrix of g_i(theta_0) as weighting matrix (optimal)
gmm_model = gmm(moment_cond, data, t0=c(0,0,2))   # starting value for numerical optimization
summary(gmm_model)

# Different weight matrix (no difference in this case)
gmm_model = gmm(moment_cond, data, t0=c(0,0,2), wmatrix="ident")
summary(gmm_model)

# Just-identified case: Weight matrix does not matter, same as OLS / MM
lm_model = lm(y ~ x)
summary(lm_model) 




