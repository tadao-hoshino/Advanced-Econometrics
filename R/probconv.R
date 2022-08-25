rm(list = ls())
 dist <- function(n, a){

	(1 - 1/n)*pnorm(a) + (1/n)*pchisq(a, df = 2)

	}

 aa  <- -60:60/10
 D1  <-  dist(1,aa)
 D2  <-  dist(2,aa)
 D10 <-  dist(10,aa)
 Y   <-  pnorm(aa)


 par(mfrow = c(1,3))
 par(cex = 1.1)
 plot(aa, D1, type = "l", xlab = "z", ylab = "Probability", main = "n = 1", ylim = c(0,1), lwd = 2)
 par(new = T)
 plot(aa, Y, type = "l", xlab = "z", ylab = "Probability", col = "red", ylim = c(0,1), lwd = 2)
 legend("topleft", c("Pr(Zn < z)", "Pr(Z < z)"), lwd = c(2,2), col = c(1,2))

 plot(aa, D2, type = "l", xlab = "z", ylab = "Probability", main = "n = 2", ylim = c(0,1), lwd = 2)
 par(new = T)
 plot(aa, Y, type = "l", xlab = "z", ylab = "Probability", col = "red", ylim = c(0,1), lwd = 2)
 legend("topleft", c("Pr(Zn < z)", "Pr(Z < z)"), lwd = c(2,2), col = c(1,2))


 plot(aa, D10, type = "l", xlab = "z", ylab = "Probability", main = "n = 10", ylim = c(0,1), lwd = 2)
 par(new = T)
 plot(aa, Y, type = "l", xlab = "z", ylab = "Probability", col = "red", ylim = c(0,1), lwd = 2)
 legend("topleft", c("Pr(Zn < z)", "Pr(Z < z)"), lwd = c(2,2), col = c(1,2))