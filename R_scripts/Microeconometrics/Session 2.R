### Panel data

library(plm)

## A balanced panel of 10 observational units (firms) from 1935 to 1954
data("Grunfeld", package = "plm")   # Data are flattened


## Pooled estimator
pooled_ols_lm = lm(inv ~ capital, data = Grunfeld)
summary(pooled_ols_lm)

# We can also get pooled estimators from the plm package
pooled_ols_plm = plm(inv ~ capital, data = Grunfeld, 
                     index=c("firm", "year"),   # (i,t) in our notation
                     model = "pooling")
summary(pooled_ols_plm)


## Random effects estimator (rather unrealistic assumptions)
re_ols_plm = plm(inv ~ capital, data = Grunfeld, 
                 index=c("firm", "year"),   
                 model = "random")
summary(re_ols_plm)   # smaller standard error (GLS)


## Fixed effects estimator

# Firm fixed effect (account for u_i)
fe_ols_plm_firm = plm(inv ~ capital, data = Grunfeld, 
                 index=c("firm", "year"),   
                 effect = "individual", model = "within")
summary(fe_ols_plm_firm)   # coefficient quite similar to random effects estimator

# Time fixed effect (account for v_t)
fe_ols_plm_time = plm(inv ~ capital, data = Grunfeld, 
                 index=c("firm", "year"),   
                 effect = "time", model = "within")
summary(fe_ols_plm_time)   # year fixed effect important to include (crisis)!

# Remark: Year fixed effect leads to different estimation results
# While in firm fixed effects, results are similar to random effects (not good)

# Two way fixed effect
fe_ols_plm_2way = plm(inv ~ capital, data = Grunfeld, 
                      index=c("firm", "year"),   
                      effect = "twoways", model = "within")
summary(fe_ols_plm_2way)

# Estimated coefficient is in between time and firm fixed effects estimators


## First difference estimator
fd_ols_plm_firm = plm(inv ~ capital, data = Grunfeld, 
                      index=c("firm", "year"),   
                      effect = "individual", model = "fd")
summary(fd_ols_plm_firm)

# First difference estimator has larger standard errors 
# than fixed effect estimator!


## Check for T=2, that FE is equivalent to FD 

# Firm fixed effect
fe_ols_plm_firm_check = plm(inv ~ capital, data = Grunfeld, 
                            subset = year %in% c(1935,1936),
                            index=c("firm", "year"),   
                            effect = "individual", model = "within")
lmtest::coeftest(fe_ols_plm_firm_check)

# First difference estimator
fd_ols_plm_firm_check = plm(inv ~ capital-1, data = Grunfeld, 
                            subset = year %in% c(1935,1936),
                            index=c("firm", "year"),   
                            effect = "individual", model = "fd")
lmtest::coeftest(fd_ols_plm_firm_check)





