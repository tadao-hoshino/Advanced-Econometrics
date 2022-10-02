
 library(mgcv)
 library(AER)

 data("GSS7402")

## kids: Number of children. 
## education: Highest year of school completed.
## agefirstbirth: Woman�fs age at birth of first child

 mod_lm = gam(kids ~ education + agefirstbirth, data = GSS7402)
 summary(mod_lm)

 mod_gam = gam(kids ~ s(education) + s(agefirstbirth), data = GSS7402)
 par(mfrow = c(1,2))
 plot(mod_gam) 