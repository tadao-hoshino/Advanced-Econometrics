rm(list = ls())
 #setwd(---) # Set your working directory

 # Data #

  data <- as.data.frame(read.csv("TokyoCrime.csv"))
  n    <- nrow(data)

 # Spatial weight matrix #

  W <- as.matrix(read.csv("TokyoCrime_W.csv", header = FALSE))

 # Variables #

 Y  <- data$burglary
 X1 <- with(data, cbind(nhmem, dnsty, owner, elder, high, manag, retail))

 ward23 <- as.numeric(data$ward)
 wdum   <- matrix(0,n,22)
 for(i in 1:22) wdum[,i] <- ifelse(ward23 == i, 1, 0)

 X  <- cbind(1, X1, wdum)
 H  <- cbind(W%*%Y, X)
 Z1 <- cbind(W%*%X1, W%*%W%*%X1, W%*%W%*%W%*%X1)
 Z  <- cbind(X, Z1)

 ##### Spatial Lag Model #####

 # 2SLS estimation #

 P     <- Z%*%solve(t(Z)%*%Z)%*%t(Z)
 theta <- solve(t(H)%*%P%*%H)%*%t(H)%*%P%*%Y

 # Covariance matrix under homoscedasticity #

 sigma2 <- mean((Y - H%*%theta)^2)
 Q_ZZ   <- t(Z)%*%Z/n
 Q_ZH   <- t(Z)%*%H/n
 Cov    <- (sigma2/n)*solve(t(Q_ZH)%*%solve(Q_ZZ)%*%Q_ZH)

 sd   <- diag(Cov)^0.5 # Standard error
 tval <- theta/sd      # t-value

 ##### Spatial Error Model #####

 # OLS estimation #

 beta <- lm(Y ~ X - 1)$coef



 E  <- Y - X%*%beta; E1 <- W%*%E; E2 <- W%*%W%*%E
 
 Gam1 <- c(2*t(E)%*%E1/n,              -t(E1)%*%E1/n, 1)
 Gam2 <- c(2*t(E1)%*%E2/n,             -t(E2)%*%E2/n, sum(diag(t(W)%*%W))/n) 
 Gam3 <- c(t(E1)%*%E1/n + t(E2)%*%E/n, -t(E1)%*%E2/n, 0)
 Gam  <- rbind(Gam1, Gam2, Gam3)
 gam  <- c(t(E)%*%E/n, t(E1)%*%E1/n, t(E1)%*%E/n)

# MM estimation #

 ObjF <- function(param){

	g <- Gam%*%c(param[1], param[1]^2, param[2]^2) - gam
	sum(g^2)

	}

 NLS    <- optim(c(0.5, sd(E)), ObjF)
 lambda <- NLS$par[1]
 sigma2 <- NLS$par[2]^2
 c(lambda, sigma2)

 # GLS estimation #

 Omega <- solve(diag(n) - lambda*W)%*%solve(diag(n) - lambda*t(W))
 beta_GLS <- solve(t(X)%*%solve(Omega)%*%X)%*%t(X)%*%solve(Omega)%*%Y
 cbind(beta, beta_GLS)

 # Covariance matrix #

 Cov <- (sigma2/n)*solve(t(X)%*%solve(Omega)%*%X/n)

 sd   <- diag(Cov)^0.5 # Standard error
 tval <- beta_GLS/sd   # t-value

 cbind(beta_GLS, sd, tval)