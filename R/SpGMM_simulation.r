
 Spmat <- function(m, n){

	x_ax <- numeric(m*n)
	y_ax <- numeric(m*n)

	for(i in 1:n) x_ax[m*(i-1) + (1:m)] <- 1:m
	for(i in 1:n) y_ax[m*(i-1) + (1:m)] <- i

	x_diff <- abs(outer(x_ax, x_ax, FUN="-"))
	y_diff <- abs(outer(y_ax, y_ax, FUN="-"))
	D      <- sqrt((x_diff^2) + (y_diff^2))
	W      <- ifelse(D == 1, 1, 0)
	W      <- W/rowSums(W)
	W

	}


 N  <- 800
 W  <- Spmat(20,40)
 X1 <- rnorm(N)
 X2 <- rnorm(N)
 Ystar <- solve(diag(N) - 0.5*W)%*%(1 + X1 + X2 + rnorm(N))
 D  <- (Ystar > 0)
 
 X  <- cbind(1, X1, X2)
 Z  <- cbind(X, W%*%X1, W%*%X2)

 # Generalized residual #

 g <- function(a) (D - pnorm(a))*dnorm(a)/(pnorm(a)*(1 - pnorm(a)))

 # Neumann-series approximation #

 S <- function(rho) diag(N) + rho*W + (rho^2)*W%*%W + (rho^3)*W%*%W%*%W

 ##### Spatial Lag Probit Model #####


 # 1st stage GMM estimation #

 GMM <- function(theta){

	rho  <- theta[1]
	beta <- theta[-1]

	stdev <- sqrt(rowSums(S(rho)^2))
	Xi    <- S(rho)%*%X/stdev
	g_bar <- t(Z)%*%g(Xi%*%beta)/N
	t(g_bar)%*%g_bar

	}

 start <- c(0.5, glm(D ~ X - 1, family = binomial(link = "probit"))$coef)
 GMM1 <- optim(start, GMM, method = "BFGS")

# 2nd stage GMM estimation #

 V   <- function(theta){

	rho  <- theta[1]
	beta <- theta[-1]

	stdev <- sqrt(rowSums(S(rho)^2))
	Xi    <- S(rho)%*%X/stdev
	g2    <- as.numeric(g(Xi%*%beta)^2)
	t(Z)%*%diag(g2)%*%Z/N

	}

 Omega <- solve(V(GMM1$par))

 GMM <- function(theta){

	rho  <- theta[1]
	beta <- theta[-1]

	stdev <- sqrt(rowSums(S(rho)^2))
	Xi    <- S(rho)%*%X/stdev
	g_bar <- t(Z)%*%g(Xi%*%beta)/N
	t(g_bar)%*%Omega%*%g_bar

	}

 GMM2 <- optim(GMM1$par, GMM, method = "BFGS")

 GMM2
