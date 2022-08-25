
 m <- function(x){
    if(-pi <= x & x < -3*pi/4) return(4*x + 4*pi)
    if(-3*pi/4 <= x & x < -pi/2) return(-3*x - 5*pi/4)
    if(-pi/2 <= x & x < pi/2) return(-(x^2)/10 + (pi^2)/40 + pi/4)
    if( pi/2 <= x & x < 3*pi/4) return(2*x - 3*pi/4)
    if( 3*pi/4 <= x & x < pi) return(-3*x + 3*pi)
  }

 xx <- -(50*pi):(50*pi)/50
 Y <- mapply(m,xx)

 Pol <- function(k){
	pol <- 1
	for(i in 1:k) pol <- cbind(pol, xx^i)
	pol
	}

 pol3 <- predict(lm(Y ~ Pol(3) - 1))
 pol6 <- predict(lm(Y ~ Pol(6) - 1))
 pol12 <- predict(lm(Y ~ Pol(12) - 1))

 par(mfrow = c(1,2))

 plot(xx, Y, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, lty = 2, xlab = "x", ylab = "m(x)", main = "Power Series")
 par(new = T)
 plot(xx, pol3, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, xlab = "x", ylab = "m(x)", main = "", col = 2)
 par(new = T)
 plot(xx, pol6, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, xlab = "x", ylab = "m(x)", main = "", col = 3)
 par(new = T)
 plot(xx, pol12, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, xlab = "x", ylab = "m(x)", main = "", col = 4)
 legend("topright", c("True m(x)", "k = 3", "k = 6", "k = 12"), lty = c(2,1,1,1), lwd = c(2,2,2,2), col = c(1,2,3,4))

 Fou <- function(k){
	fou <- 1
	for(i in 1:k) fou <- cbind(fou, cos(i*xx), sin(i*xx))
	fou[, 1:(1 + k)]
	}

 fou3 <- predict(lm(Y ~ Fou(3) - 1))
 fou6 <- predict(lm(Y ~ Fou(6) - 1))
 fou12 <- predict(lm(Y ~ Fou(12) - 1))

 plot(xx, Y, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, lty = 2, xlab = "x", ylab = "m(x)", main = "Fourier Series")
 par(new = T)
 plot(xx, fou3, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, xlab = "x", ylab = "m(x)", main = "", col = 2)
 par(new = T)
 plot(xx, fou6, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, xlab = "x", ylab = "m(x)", main = "", col = 3)
 par(new = T)
 plot(xx, fou12, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, xlab = "x", ylab = "m(x)", main = "", col = 4)
 legend("topright", c("True m(x)", "k = 3", "k = 6", "k = 12"), lty = c(2,1,1,1), lwd = c(2,2,2,2), col = c(1,2,3,4))