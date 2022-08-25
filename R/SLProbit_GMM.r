
 # Data #

  data <- as.data.frame(read.csv("TokyoCrime.csv"))
  n    <- nrow(data)

 # Spatial weight matrix #

  W <- as.matrix(read.csv("TokyoCrime_W.csv", header = FALSE))

 # Variables #

 D  <- ifelse(data$assault + data$injury > 0, 1, 0)
 X1 <- with(data, cbind(nhmem, dnsty, owner, elder, high, manag, retail))

 ward23 <- as.numeric(data$ward)
 wdum   <- matrix(0,n,22)
 for(i in 1:22) wdum[,i] <- ifelse(ward23 == i, 1, 0)

 X  <- cbind(1, X1, wdum)
 Z  <- cbind(X, W%*%X1)

 # Generalized residual #

 g <- function(a) (D - pnorm(a))*dnorm(a)/(pnorm(a)*(1 - pnorm(a)))

 # Neumann-series approximation #

 S <- function(rho) diag(n) + rho*W + (rho^2)*W%*%W + (rho^3)*W%*%W%*%W

 ##### Spatial Lag Probit Model #####

 # 1st stage GMM estimation #

 GMM <- function(theta){

	rho  <- theta[1]
	beta <- theta[-1]

	stdev <- sqrt(rowSums(S(rho)^2))
	Xi    <- S(rho)%*%X/stdev
	g_bar <- t(Z)%*%g(Xi%*%beta)/n
	t(g_bar)%*%g_bar

	}

 start <- c(0.5, lm(D ~ X - 1)$coef)
 GMM1 <- optim(start, GMM, method = "BFGS")
