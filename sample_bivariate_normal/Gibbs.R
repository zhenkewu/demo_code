rm(list=ls())
set.seed(20150311)
#install.packages("mixtools")
library(mixtools)

N   <- 50
rho <- 0.0
s   <- 1-rho^2

# plot contours of the bivariate normal:
plot(0:10,type="n",xlim=c(-3,3),ylim=c(-3,3),main="Simple Gibbs Sampler",asp=1,
     xlab=expression(Y[1]),ylab=expression(Y[2]),bty="n")
legend("topleft","starting point",pch=17,bty="n")
res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .05, npoints = 250) 
points(res,type="l")
res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .5, npoints = 250, col="red") 
points(res,type="l",col="blue")

res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .95, npoints = 250, col="red") 
points(res,type="l",col="red")

# store samples:
mat <- matrix(NA,nrow=N,ncol=2)

Y1 <- -2#runif(1,-5,5)
Y2 <- 2 #runif(1,-5,5)
mat[1,] <- c(Y1,Y2) 
points(Y1,Y2,pch=17,lwd=2)
for (i in 2:N){
  Y1 <- rnorm(1,rho*Y2,sqrt(s))
  Y2 <- rnorm(1,rho*Y1,sqrt(s))
  mat[i,1] <- Y1
  mat[i,2] <- Y2
#   arrows(mat[i-1,1],mat[i-1,2],mat[i,1],mat[i-1,2],length=0.1,angle=40)
#   Sys.sleep(.5)
#   arrows(mat[i,1],mat[i-1,2],mat[i,1],mat[i,2],length=0.1,angle=40)
#   Sys.sleep(.5)
  arrows(mat[i-1,1],mat[i-1,2],mat[i,1],mat[i,2],length=0.1,angle=40)
  Sys.sleep(.5)
}



