
 N   <- 500      # sample size
 X   <- rnorm(N) # exogenous regressor
 err <- rnorm(N) # error term

 Z1 <- runif(N, -1, 1)  # instrumental variable
 D  <- 1 + Z1 + 0.5*err # endogenous regressor
 Y  <- 1 + D + X + err  # outcome variable

 cov(D, err)  # endogeneity
 cov(Z1, err) # exclusion restriction
 cov(Z1, D)   # relevance condition
 
# OLS estimation #

 lm(Y ~ D + X)

# 2SLS estimation #

 H <- cbind(1,D, X)
 Z <- cbind(1,Z1,X)
 
 Proj  <- Z%*%solve(t(Z)%*%Z)%*%t(Z)
 theta <- solve(t(H)%*%Proj%*%H)%*%t(H)%*%Proj%*%Y
 theta

 
# Weak instrument problem #

 D  <- 1 + 0.2*Z1 + 0.5*err # endogenous regressor
 Y  <- 1 + D + X + err      # outcome variable

 cov(Z1, D) # relevance condition

# 2SLS estimation #

 H <- cbind(1,D, X)
 Z <- cbind(1,Z1,X)
 
 Proj  <- Z%*%solve(t(Z)%*%Z)%*%t(Z)
 theta <- solve(t(H)%*%Proj%*%H)%*%t(H)%*%Proj%*%Y
 theta
