# EXAMPLE 2: HYBRID MONTE CARLO SAMPLING -- BIVARIATE NORMAL
rm(list=ls())
set.seed(20150311)
# STEP SIZE
delta    = 0.1
nSamples = 50

RHO      = 0.995

semiaxis <- svd(matrix(c(1,RHO,RHO,1),nrow=2,ncol=2))$d

L        = 20#max(20,max(semiaxis)/min(semiaxis))

library(mixtools)
s   <- 1-RHO^2

# plot contours of the bivariate normal:
plot(0:10,type="n",xlim=c(-3,3),ylim=c(-3,3),main="Hamiltonian Monte Carlo",asp=1,
     xlab=expression(Y[1]),ylab=expression(Y[2]),bty="n")
legend("topleft","starting point",pch=17,bty="n")
res <- ellipse(mu=c(0,0), sigma=matrix(c(1,RHO,RHO,1),nrow=2,ncol=2), 
               alpha = .05, npoints = 250) 
points(res,type="l")
res <- ellipse(mu=c(0,0), sigma=matrix(c(1,RHO,RHO,1),nrow=2,ncol=2), 
               alpha = .5, npoints = 250, col="red") 
points(res,type="l",col="blue")

res <- ellipse(mu=c(0,0), sigma=matrix(c(1,RHO,RHO,1),nrow=2,ncol=2), 
               alpha = .95, npoints = 250, col="red") 
points(res,type="l",col="red")


# DEFINE POTENTIAL ENERGY FUNCTION
U <- function(x,rho=RHO){# x is a column vector; rho is correleation:
  t(x)%*%solve(matrix(c(1,rho,rho,1),nrow=2,ncol=2))%*%x
}

# DEFINE GRADIENT OF POTENTIAL ENERGY
dU <- function(x,rho=RHO){
  t(x)%*%solve(matrix(c(1,rho,rho,1),nrow=2,ncol=2))
}

# DEFINE KINETIC ENERGY FUNCTION
K <- function(p){
  sum(p^2)/2
}

# INITIAL STATE
x  = matrix(0,nrow=2,ncol=nSamples)
x[,1] = c(-2,2)
points(x[1,1],x[2,1],pch=17,lwd=2)
t = 1
for (t in 1:(nSamples-1)){
    t = t + 1;
    print(t)
    # SAMPLE RANDOM MOMENTUM
    p0 = matrix(rnorm(2),nrow=2,ncol=1)
    
    # SIMULATE HAMILTONIAN DYNAMICS
    # FIRST 1/2 STEP OF MOMENTUM
    pStar = p0 - delta/2*t(dU(x[,t-1]))
    
    # FIRST FULL STEP FOR POSITION/SAMPLE
    xStar = x[,t-1] + delta*pStar
    
    # FULL STEPS
    for (jL in 1:L-1){
      # MOMENTUM
      pStar = pStar - delta*t(dU(xStar))
      # POSITION/SAMPLE
      xStar = xStar + delta*pStar
    }
    
    # LAST HALP STEP
    pStar = pStar - delta/2*t(dU(xStar));
     
        # COULD NEGATE MOMENTUM HERE TO LEAVE
        # THE PROPOSAL DISTRIBUTION SYMMETRIC.
        # HOWEVER WE THROW THIS AWAY FOR NEXT
        # SAMPLE, SO IT DOESNT MATTER
    
    # EVALUATE ENERGIES AT
    # START AND END OF TRAJECTORY
    U0 = U(x[,t-1])
    UStar = U(xStar)
    
    K0 = K(p0)
    KStar = K(pStar)
    
    # ACCEPTANCE/REJECTION CRITERION
    alpha = min(1,exp((U0 + K0) - (UStar + KStar)))
    
    u = runif(1)
    if (u < alpha){
      x[,t] = xStar
      print("accept")
    } else{
      x[,t] = x[,t-1]
    }
    
    i=t
#     arrows(x[1,i-1],x[2,i-1],x[1,i],x[2,i-1],length=0.1,angle=40)
#     Sys.sleep(.5)
#     arrows(x[1,i],x[2,i-1],x[1,i],x[2,i],length=0.1,angle=40)
#     Sys.sleep(.5)
    arrows(x[1,i-1],x[2,i-1],x[1,i],x[2,i],length=0.1,angle=40)
    Sys.sleep(.5)
}
