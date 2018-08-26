# N <- 65
# K <- 3
# rs <- randtoolbox::sobol(n=N, dim=2*K, scrambling=1)  # default scrambling 'owen'
# A <- rs[, 1:K]
# B <- rs[, (K+1):(2*K)]
# UPPER.ini <- c(10, 100, 1000)
# LOWER.ini <- c(1, 11, 101)
# An <- A * (UPPER.ini - LOWER.ini) + LOWER.ini
# Bn <- B * (UPPER.ini - LOWER.ini) + LOWER.ini
# 
# matrix( rep(1:5, 10), nrow=10, byrow=TRUE)


?sensitivity::parameterSets                                                      # OR IS IT USING THIS FUNCTION??
X.sobol<-parameterSets(par.ranges=list(V1=c(1,1000),V2=c(1,4)),
                       samples=100,method="sobol")
windows()
plot(X.sobol)


# Converting sobol samples to actual values -------------------------------
# based on https://www.r-bloggers.com/sampling-for-monte-carlo-simulations-with-r/
# Generate a Monte Carlo sample
generateMCSample <- function(n, vals) {                                           # The function
  # Packages to generate quasi-random sequences
  # and rearrange the data
  require(randtoolbox)
  require(plyr)
  
  # Generate a Sobol' sequence
  sob <- randtoolbox::sobol(n, length(vals))
  
  # Fill a matrix with the values
  # inverted from uniform values to
  # distributions of choice
  samp <- matrix(rep(0,n*(length(vals)+1)), nrow=n)              # First column is sample no. 
  samp[,1] <- 1:n             
  for (i in 1:length(vals)) {
    l <- vals[[i]]
    dist <- l$dist
    params <- l$params
    samp[,i+1] <- eval(call(paste("q",dist,sep=""),sob[,i],params[1],params[2])) # call function "qunif", "qnorm" etc.
  }
  
  # Convert matrix to data frame and label
  samp <- as.data.frame(samp)
  names(samp) <- c("n",laply(vals, function(l) l$var))
  return(samp)
}


n <- 100  # number of simulations to run                               # Testing the function

# List described the distribution of each variable
vals <- list(list(var="Uniform",
                  dist="unif",
                  params=c(11,100)),
             list(var="Normal",
                  dist="norm",
                  params=c(101,500)),
             list(var="Weibull",
                  dist="weibull",
                  params=c(2,1)))

# Generate the sample
samp <- generateMCSample(n,vals)

# Plot with ggplot2
require(ggplot2)
samp.mt <- melt(samp,id="n")
gg <- ggplot(samp.mt,aes(x=value)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap(~variable, ncol=3,scale="free")




# Sobol with multi output -------------------------------------------------


## Not run:
# Functional toy function: Arctangent temporal function (Auder, 2011)
# X: input matrix (in [-7,7]^2)
# q: number of discretization steps of [0,2pi] interval
# output: vector of q values
atantemp <- function(X, q = 100){
  n <- dim(X)[[1]]
  t <- (0:(q-1)) * (2*pi) / (q-1)
  res <- matrix(0,ncol=q,nrow=n)
  for (i in 1:n) res[i,] <- atan(X[i,1]) * cos(t) + atan(X[i,2]) * sin(t)
  return(res)
}
# Tests functional toy fct

y0 <- atantemp(matrix(c(-7,0,7,-7,0,7),ncol=2))
#plot(y0[1,],type="l")
#apply(y0,1,lines)
n <- 100
X <- matrix(c(runif(2*n,-7,7)),ncol=2)
y <- atantemp(X)
x11()
plot(y0[2,],ylim=c(-2,2),type="l")
apply(y,1,lines)
# Sobol indices computations
n <- 1000
X1 <- data.frame(matrix(runif(2*n,-7,7), nrow = n))
X2 <- data.frame(matrix(runif(2*n,-7,7), nrow = n))
x11()
sa <- sobolMultOut(model=atantemp, q=100, X1, X2,
                   MCmethod="soboljansen", plotFct=T)
print(sa)
x11()
plot(sa)
## End(Not run)




# Tell() for external model -----------------------------------------------
x <- sobolroalhs(model = NULL, factors = 3, runs = 1000, order =1, nboot=0)
# X1 follows a log-normal distribution:
x$X[,1] <- qlnorm(x$X[,1])
# X2 follows a standard normal distribution:
x$X[,2] <- qnorm(x$X[,2])
# X3 follows a gamma distribution:
x$X[,3] <- qgamma(x$X[,3],shape=0.5)
# toy example
toy <- function(x){rowSums(x)}
y <- toy(x$X)
tell(x, y)
print(x)
plot(x)



# Shapley effect ----------------------------------------------------------
##################################
# Test case : the Ishigami function (3 uniform independent inputs)
# See Iooss and Prieur (2017)

library(gtools)

d <- 3
Xall <- function(n) matrix(runif(d*n,-pi,pi),nc=d)
Xset <- function(n, Sj, Sjc, xjc) matrix(runif(n*length(Sj),-pi,pi),nc=length(Sj))

x <- shapleyPermEx(model = ishigami.fun, Xall=Xall, Xset=Xset, d=d, Nv=1e4, No = 1e3, Ni = 3)
print(x)
plot(x)

##################################
# Test case : Linear model (3 Gaussian inputs including 2 dependent)
# See Iooss and Prieur (2017)

library(gtools)
library(mvtnorm) # Multivariate Gaussian variables
library(condMVNorm) # Conditional multivariate Gaussian variables

modlin <- function(X) apply(X,1,sum)

d <- 3
mu <- rep(0,d)
sig <- c(1,1,2)
ro <- 0.9
Cormat <- matrix(c(1,0,0,0,1,ro,0,ro,1),d,d)
Covmat <- ( sig %*% t(sig) ) * Cormat

Xall <- function(n) mvtnorm::rmvnorm(n,mu,Covmat)

Xset <- function(n, Sj, Sjc, xjc){
  if (is.null(Sjc)){
    if (length(Sj) == 1){ rnorm(n,mu[Sj],sqrt(Covmat[Sj,Sj]))
    } else{ mvtnorm::rmvnorm(n,mu[Sj],Covmat[Sj,Sj])}
  } else{ condMVNorm::rcmvnorm(n, mu, Covmat, dependent.ind=Sj, given.ind=Sjc, X.given=xjc)}}

x <- shapleyPermEx(model = modlin, Xall=Xall, Xset=Xset, d=d, Nv=1e4, No = 1e3, Ni = 3)
print(x)
plot(x)

## End(Not run)



# Partal correlation ################################################################

library(boot)
n <- 100
X <- data.frame(X1 = runif(n, 0.5, 1.5),
                X2 = runif(n, 1.5, 4.5),
                X3 = runif(n, 4.5, 13.5))

# linear model : Y = X1 + X2 + X3
y <- with(X, X1 + X2 + X3)

# sensitivity analysis
x <- pcc(X, y, nboot = 100)
print(x)
plot(x) # TODO: find another example...
