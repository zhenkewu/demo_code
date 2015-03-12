rm(list=ls())
set.seed(20150311)
library(mixtools)

N   <- 50
rho <- 0#0.995
s   <- 1-rho^2
par(mfrow=c(1,3))

# plot contours of the bivariate normal:
plot(0:10,type="n",xlim=c(-3,3),ylim=c(-3,3),main="rho=0",asp=1,
     xlab=expression(Y[1]),ylab=expression(Y[2]),bty="n",cex.main=4)
res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .05, npoints = 250) 
points(res,type="l")
res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .5, npoints = 250, col="red") 
points(res,type="l",col="blue")

res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .95, npoints = 250, col="red") 


rho <- 0.5#0.995
s   <- 1-rho^2

# plot contours of the bivariate normal:
plot(0:10,type="n",xlim=c(-3,3),ylim=c(-3,3),main="rho=0.5",asp=1,
     xlab=expression(Y[1]),ylab=expression(Y[2]),bty="n",cex.main=4)
res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .05, npoints = 250) 
points(res,type="l")
res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .5, npoints = 250, col="red") 
points(res,type="l",col="blue")

res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .95, npoints = 250, col="red") 



rho <- 0.995#0.995
s   <- 1-rho^2

# plot contours of the bivariate normal:
plot(0:10,type="n",xlim=c(-3,3),ylim=c(-3,3),main="rho=0.995",asp=1,
     xlab=expression(Y[1]),ylab=expression(Y[2]),bty="n",cex.main=4)
res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .05, npoints = 250) 
points(res,type="l")
res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .5, npoints = 250, col="red") 
points(res,type="l",col="blue")

res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .95, npoints = 250, col="red") 


