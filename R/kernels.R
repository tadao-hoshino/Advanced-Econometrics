
 Uni <- function(u) 0.5*ifelse(abs(u) < 1, 1, 0)
 Epa <- function(u) (3/4)*(1 - u^2)*ifelse(abs(u) < 1, 1, 0)

 par(mfrow = c(1,3))
 curve(Uni, xlim = c(-2.5,2.5), ylim = c(0, 0.8), main = "Uniform kernel", col = "red")
 curve(Epa, xlim = c(-2.5,2.5), ylim = c(0, 0.8), main = "Epanechnikov kernel", col = "red")
 curve(dnorm, xlim = c(-2.5,2.5), ylim = c(0, 0.8), main = "Gaussian kernel", col = "red")