

library(lattice)
library(scatterplot3d)

 x1 <- runif(20,-3,3)
 x2 <- runif(20,-3,3)
 x3 <- runif(20,-3,3)



 par(mfrow = c(1,3))
 x <- cbind(x1,0)
         
 plot(x,bty='n',xaxt='n',yaxt='n',ylab='', xlab='x1', pch=21, cex=2, xlim = c(-3, 3),col = "blue", main = "1D")
 axis(side=1,seq(-4,4,1),pos=0)


 
 plot(x1, x2, col = "blue", xlim = c(-3, 3), ylim = c(-3, 3), cex = 2, main = "2D")
 grid()

 scatterplot3d(x = x1, y = x2, z = x3, xlim = c(-3, 3), ylim = c(-3, 3), zlim = c(-3,3),
	color = "blue", cex.symbols = 2, grid = TRUE, main = "3D")