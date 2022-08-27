
 g <- function(x){
    if(-pi <= x & x < -3*pi/4) return(4*x + 4*pi)
    if(-3*pi/4 <= x & x < -pi/2) return(-3*x - 5*pi/4)
    if(-pi/2 <= x & x < pi/2) return(-(x^2)/10 + (pi^2)/40 + pi/4)
    if( pi/2 <= x & x < 3*pi/4) return(2*x - 3*pi/4)
    if( 3*pi/4 <= x & x < pi) return(-3*x + 3*pi)
  }

 xx <- -(50*pi):(50*pi)/50
 Y <- mapply(g,xx)

 Pol <- function(k){
	pol <- 1
	for(i in 1:k) pol <- cbind(pol, xx^i)
	pol
	}

 pol2 <- predict(lm(Y ~ Pol(2) - 1))
 pol4 <- predict(lm(Y ~ Pol(4) - 1))
 pol10 <- predict(lm(Y ~ Pol(10) - 1))

 par(mfrow = c(1,2))

 plot(xx, Y, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, lty = 2, xlab = "x", ylab = "g0(x)", main = "Power Series")
 par(new = T)
 plot(xx, pol2, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, xlab = "x", ylab = "g0(x)", main = "", col = 2)
 par(new = T)
 plot(xx, pol4, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, xlab = "x", ylab = "g0(x)", main = "", col = 3)
 par(new = T)
 plot(xx, pol10, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, xlab = "x", ylab = "g0(x)", main = "", col = 4)
 legend("topright", c("True g0(x)", "k = 2", "k = 4", "k = 10"), lty = c(2,1,1,1), lwd = c(2,2,2,2), col = c(1,2,3,4))

 Fou <- function(k){
	fou <- 1
	for(i in 1:k) fou <- cbind(fou, cos(i*xx), sin(i*xx))
	fou
	}

 fou2 <- predict(lm(Y ~ Fou(2) - 1))
 fou4 <- predict(lm(Y ~ Fou(4) - 1))
 fou10 <- predict(lm(Y ~ Fou(10) - 1))

 plot(xx, Y, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, lty = 2, xlab = "x", ylab = "g0(x)", main = "Fourier Series")
 par(new = T)
 plot(xx, fou2, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, xlab = "x", ylab = "g0(x)", main = "", col = 2)
 par(new = T)
 plot(xx, fou4, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, xlab = "x", ylab = "g0(x)", main = "", col = 3)
 par(new = T)
 plot(xx, fou10, xlim = range(xx), ylim = range(Y), type = "l", lwd = 2, xlab = "x", ylab = "g0(x)", main = "", col = 4)
 legend("topright", c("True g0(x)", "k = 2", "k = 4", "k = 10"), lty = c(2,1,1,1), lwd = c(2,2,2,2), col = c(1,2,3,4))