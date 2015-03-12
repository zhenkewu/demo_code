rm(list=ls())
set.seed(20150311)
library(mixtools)

N   <- 50
rho <- 0.995#0.995
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
mat_trans <- matrix(NA,nrow=N,ncol=2)

# transformation:
A <- matrix(c(1,rho,rho,1),nrow=2,ncol=2)
e <- eigen(A)
V <- e$vectors
B <- V %*% diag(sqrt(e$values)) %*% t(V)
Binv <- solve(B)


Y1 <- -2 #runif(1,-5,5)
Y2 <-  2 #runif(1,-5,5)
Yvec_trans <- Binv%*%matrix(c(Y1,Y2),nrow=2,ncol=1)
Y1_trans <- Yvec_trans[1]
Y2_trans <- Yvec_trans[2]

mat_trans[1,]  <- Yvec_trans

points(Y1,Y2,pch=17,lwd=2)
for (i in 2:N){
  Y1_trans <- rnorm(1,0,1)
  Y2_trans <- rnorm(1,0,1)
  mat_trans[i,] <- c(Y1_trans,Y2_trans)
#   arrows(mat[i-1,1],mat[i-1,2],mat[i,1],mat[i-1,2],length=0.1,angle=40)
#   Sys.sleep(.5)
#   arrows(mat[i,1],mat[i-1,2],mat[i,1],mat[i,2],length=0.1,angle=40)
#   Sys.sleep(.5)

}

mat <- mat_trans%*%B
for (i in 2:N){
  arrows(mat[i-1,1],mat[i-1,2],mat[i,1],mat[i,2],length=0.1,angle=40)
  Sys.sleep(.5)
}



