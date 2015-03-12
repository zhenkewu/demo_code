rm(list=ls())
set.seed(20150311)
library(mixtools)

Nsamp   <- 50
rho <- 0.995#0.995
s   <- 1-rho^2
# plot contours of the bivariate normal:
plot(0:10,type="n",xlim=c(-3,3),ylim=c(-3,3),main="",asp=1,
     xlab=expression(Y[1]),ylab=expression(Y[2]),bty="n",cex.main=4)
res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .05, npoints = 250) 
points(res,type="l")
res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .5, npoints = 250, col="red") 
points(res,type="l",col="blue")

res <- ellipse(mu=c(0,0), sigma=matrix(c(1,rho,rho,1),nrow=2,ncol=2), 
               alpha = .95, npoints = 250, col="red") 


#install.packages("SamplerCompare")
library(SamplerCompare)

N2 <- make.gaussian(c(0,0), rho=rho)

mat <- interval.slice.sample(N2,c(-2,2),Nsamp)$X
points(mat[1,1],mat[1,2],pch=17,lwd=2)
for (i in 2:Nsamp){
  arrows(mat[i-1,1],mat[i-1,2],mat[i,1],mat[i,2],length=0.1,angle=40)
#Sys.sleep(.5)
}