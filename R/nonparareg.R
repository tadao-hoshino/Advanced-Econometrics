rm(list = ls())

 library(np) # run nonparametric kernel regression
 library(splines) # compute b-spline functions
 
 data("Engel95")
 dim(Engel95)
 food <- Engel95$food # share of food consumption
 exp  <- Engel95$logexp # log of total expenditure 
 
 plot(exp, food)

 mhat_kernel <- npreg(food ~ exp)

 knots <- quantile(exp, (1:3)/4)
 p <- bs(exp, knots = knots)
 dim(p)

 mhat_spline <- predict(lm(food ~ p))

 plot(exp, food, xlim = range(exp), ylim = range(food), 
	main = "Nonparametric regression", xlab = "exp", ylab = "food")
 par(new = T)
 plot(exp[order(exp)], mhat_kernel$mean[order(exp)], xlab = "", ylab = "",
	xlim = range(exp), ylim = range(food), type = "l", lwd = 2, col = 2)
 par(new = T)
 plot(exp[order(exp)], mhat_spline[order(exp)], xlab = "", ylab = "",
	xlim = range(exp), ylim = range(food), type = "l", lwd = 2, col = 3)
 legend("topright", c("Kernel regression", "Spline series regression"), 
	lty = c(1,1), lwd = c(2,2), col = c(2,3))