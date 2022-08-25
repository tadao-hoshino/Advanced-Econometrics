
 N <- 300                             # sample size
 X <- c(t(matrix(rnorm(10), 10, 30))) # 10 groups, 30 for each.
 Z <- X + X^2 + rnorm(N)              # E[Z | X] = X + X^2
 U <- rnorm(N)                        # no correlated effect
 
 alpha <- 1
 beta  <- 0.5
 gamma <- 1
 eta   <- 1

 EZ_X <- X + X^2
 EY_X <- (alpha + EZ_X*(gamma + eta)) / (1 - beta)
 Y    <- alpha + beta*EY_X + gamma*EZ_X + eta*Z + U

 lm(Y ~ EY_X + EZ_X + Z)
 lm(Y ~ EZ_X + EY_X + Z)

 EY_X <- (alpha + EZ_X*eta) / (1 - beta)
 Y    <- alpha + beta*EY_X + eta*Z + U

 lm(Y ~ EY_X + Z)