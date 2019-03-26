#!/bin/sh.


set.seed(100) 

# number of rows
N <- 100000 

# number of column of X matrix
K <- 10 

#standard deviation
sigma <- 0.5 

#create the matrix
X <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K) 
X[,1] <- 1 


#generate vector of error terms
eps <- rnorm(N,mean=0,sd=sigma)

#generate beta
betaTrue <- c( 1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

#generate y
y <- X%*%betaTrue + eps 

#Matrix algebra solution 
beta.hat.matrix <- solve(t(X)%*%X)%*%(t(X)%*%y)


#Using Gradient Descent to estimate beta_hat


# create a vector to contain all beta's for all steps

#step size
alpha <- 0.0000003


#gradient function

gradient <- function(beta,y,X) {
  return (as.vector(-2*t(X)%*%(y-X%*%beta)) )
}


# gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta.hat.matrix
beta<- runif(dim(X)[2])
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,y,X)
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))

## Closed-form solution
print(summary(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris)))


cbind(beta.hat.matrix,beta.hat.matrix)



#LBFGS

library(nloptr)
## Our objective function
objfun <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
}

## initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
LBFGS.result <- result$solution




## Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
NM.result <- result$solution

#MLE

library(nloptr)
## Our objective function
objfun  <- function(theta,y,X) {
  
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

gradient <- function (theta ,y,X) {
  grad <- as.vector (rep (0, length (theta )))
  beta <- theta [1:( length ( theta) -1)]
  sig <- theta [ length (theta )]
  grad [1:( length ( theta) -1)] <- -t(X)%*%(y - X%*%beta)/(sig ^2)
  grad[ length (theta )] <- dim(X)[1]/sig - crossprod (y-X%*%beta)/(sig^3)
  return ( grad )                                                 
}

## initial values
beta0 <- runif(dim(X)[2]+1)

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
BETA.HAT.MLE.lbfg.final <- result$solution[1:(length(result$solution)-1)]
print(BETA.HAT.MLE.lbfg.final)

#LM

#estimate model
est <- lm(y ~ X -1)
summary(est)
betalm <- est$coefficients
library(stargazer)
stargazer(est)

cbind(betaTrue,beta.hat.matrix,LBFGS.result,NM.result, BETA.HAT.MLE.lbfg.final,betalm)