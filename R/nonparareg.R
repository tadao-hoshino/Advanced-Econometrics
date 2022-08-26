rm(list = ls())

 library(np) # run nonparametric kernel regression
 library(splines) # compute b-spline functions
 
 data("Engel95")
 dim(Engel95)
 food <- Engel95$food # share of food consumption
 inc  <- Engel95$logexp # log of total expenditure 
 
 plot(inc, food)

 mhat_kernel <- npreg(food ~ inc)

 knots <- quantile(inc, (1:3)/4)
 p <- bs(inc, knots = knots)
 dim(p)

 mhat_spline <- predict(lm(food ~ p))

 plot(inc, food, xlim = range(inc), ylim = range(food), 
	main = "Nonparametric regression", xlab = "inc", ylab = "food")
 par(new = T)
 plot(inc[order(inc)], mhat_kernel$mean[order(inc)], xlab = "", ylab = "",
	xlim = range(inc), ylim = range(food), type = "l", lwd = 2, col = 2)
 par(new = T)
 plot(inc[order(inc)], mhat_spline[order(inc)], xlab = "", ylab = "",
	xlim = range(inc), ylim = range(food), type = "l", lwd = 2, col = 3)
 legend("topright", c("Kernel regression", "Spline series regression"), 
	lty = c(1,1), lwd = c(2,2), col = c(2,3))