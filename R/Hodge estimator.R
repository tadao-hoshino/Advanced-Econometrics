rm(list = ls())

 ns  <- c(10, 50, 500)
 mus <- seq(-1.5, 1.5, 0.05)

 mse <- function(n, mu){

	se <- function(seed){
		set.seed(seed)
		X <- rnorm(n, mean = mu, sd = 1)
		Xbar <- mean(X)
		H <- 0*(abs(Xbar) < n^{-1/4}) + Xbar*(abs(Xbar) >= n^{-1/4})
		se <- (sqrt(n)*(H - mu))^2
		return(se)
		}
	R <- numeric(2000)
	for(r in 1:2000) R[r] <- se(r)
	return(mean(R))

	}

 result <- matrix(0, length(ns), length(mus))
 for(i in 1:length(ns)) for(j in 1:length(mus)) result[i,j] <- mse(ns[i],mus[j])

 plot(mus, result[1,], ylim = c(0,13), xlab = "mu", ylab = "MSE", type = "l", col = 1)
 par(new = T)
 plot(mus, result[2,], ylim = c(0,13), xlab = "mu", ylab = "MSE", type = "l", col = 2)
 par(new = T)
 plot(mus, result[3,], ylim = c(0,13), xlab = "mu", ylab = "MSE", type = "l", col = 4)
 legend("topleft", legend = c("n = 10", "n = 50", "n = 500"), lty = c(1, 1, 1), col = c(1, 2, 4))