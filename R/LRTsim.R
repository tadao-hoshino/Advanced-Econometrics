rm(lst = lm())
 LL <- function(p){ 

	LogP <- X*log(p) + (1 - X)*log(1 - p)
	ell  <- sum(LogP)
	return(ell)

	}

 nrep <- 5000
 T0   <- numeric(nrep)
 T1   <- numeric(nrep)
 T2   <- numeric(nrep)

 for(i in 1:nrep){

 	X   <- rbinom(500, 1, 0.5)  # flipping an even coin 500 times
      mle <- mean(X)              # MLE = sample average

	T0[i] <- 2*(LL(mle) - LL(0.5 ))
	T1[i] <- 2*(LL(mle) - LL(0.55))
	T2[i] <- 2*(LL(mle) - LL(0.6 ))

	}

 xlm <- c(0, 20)
 ylm <- c(0, 1)
 xlb <- "T_n"
 ylb <- "Density"
 
 plot(density(T0), xlim = xlm, ylim = ylm, col = 2, main = "", xlab = xlb, ylab = ylb)
 par(new = T)
 plot(density(T1), xlim = xlm, ylim = ylm, col = 3, main = "", xlab = xlb, ylab = ylb)
 par(new = T)
 plot(density(T2), xlim = xlm, ylim = ylm, col = 4, main = "", xlab = xlb, ylab = ylb)
 par(new = T)
 curve(dchisq(x, 1), xlim = xlm, ylim = ylm, lwd = 2, lty = 2, main = "", xlab = xlb, ylab = ylb)
 legend(
	"topright",
	c("p = 0.5", "p = 0.55", "p = 0.6", "ChiSq(1)"), 
	lwd = c(1,1,1,2),
	lty = c(1,1,1,2),
	col = c(2,3,4,1)
	) 
