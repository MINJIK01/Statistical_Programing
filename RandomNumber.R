###################################
# Statisitcal Simulation Example1 #
###################################

# Random Number Generation
# Linear Congruential Generator ====================================

LGC = function(n,I0,a,b,M)
# n: the number of random numbers
# I0: initial seed
# a,b,M: constants of the linear congruential generator
{
	X=I0
	for (i in 1:(n-1))
	{
		Ij=(a*I0+b) %% M
		X=c(X,Ij)
		I0=Ij
	}
	return(X/M)
}

# Example
LGC(32,0,1,1,16)
	# A sequence of 16 random numbers is repeated.

LGC(32,0,5,4,16)
	# A sequence of 4 random numbers is repeated.

x=LGC(1000,0,1664525,1013904223,2^(32))
	# By Numerical Recipes
hist(x)

# Different seed
init=floor(proc.time()[3])
x=LGC(1000,init,1664525,1013904223,2^(32))
hist(x)

# Built-in function
y=runif(1000)

par(mfrow=c(1,2))
hist(x)
hist(y)


# Inverse Transformation Method ==================================

# exponential distribution
exp.R = function(n,lambda)	# E(X)=1/lambda
{
	U=runif(n)
	X=-(1/lambda)*log(1-U)
	return(X)
}

x=exp.R(1000,1)

# Built-in function
y=rexp(1000,rate=1) 		# E(X)=1/rate

par(mfrow=c(1,2))
hist(x)
hist(y)

# Bernoulli(p) distribution

Ber.R = function(n,p)
{
  U = runif(n)
  X = ifelse(U <= p,1,0)
  return(X)
}

x = Ber.R(1000,0.3)
mean(x)
var(x)


# Transformation Method ==========================================

# Box-Muller Transformation

BM = function(n)	# n is an even integer number
{
	X=NULL
	p=n %% 2
	if (p==0) m=n/2
	if (p==1) m=n/2+1
	for (i in 1:m)
	{
		U=runif(2)
		R=sqrt(-2*log(U[1]))
		theta=2*pi*U[2]
		x1=R*cos(theta)
		x2=R*sin(theta)
		X=c(X,x1,x2)
	}
	if (p==1) X=X[1:n]
	return(X)
}

x=BM(1000)

# Built-in function
y=rnorm(1000)

par(mfrow=c(1,2))
hist(x)
hist(y)

# Acceptance-Rejection Method ====================================

# Discrete case:
# p(x): p(1) = 0.1, p(2)=0.1, p(3)=0.4, p(4)=0.3, p(5)=0.1
# q(x): discrete uniform(1,2,3,4,5)
# c = 2

px = function(x)
{
	n = length(x)
	p = numeric(n)
	p[x==1] = 0.1
	p[x==2] = 0.1
	p[x==3] = 0.4
	p[x==4] = 0.3
	p[x==5] = 0.1
	p[x < 1 | x >5] = 0
	return(p)
}

n = 1000

X = NULL
N = 0
while (N <= n)
{
	Y = floor(5*runif(n))+1
	U = runif(n)
	C = px(Y) / 0.4
	X = c(X,Y[U <= C])
	N = length(X)
	if (N > n) X = X[1:n]
}


# Built-in function
Y = sample(1:5,n,replace=T,prob=c(0.1,0.1,0.4,0.3,0.1))

par(mfrow=c(1,2))
hist(X)
hist(Y)

# Continuous case:
# f(x) = 20x(1-x)^3, 0 < x < 1
# g(x) = 1, 0< x < 1

f = function(x) 
{
	if (x >= 0 && x <= 1) fx = 20*x*(1-x)^3
	else fx = 0
	return(fx)
}

n = 2000

X = NULL
N = 0
while (N <= n)
{
	Y = runif(n)
	U = runif(n)
	C = (256/27) * Y * (1-Y)^3
	X = c(X,Y[U <= C])
	N = length(X)
	if (N > n) X = X[1:n]
}

hist(X,freq=F,ylim=c(0,2.2))
x = seq(0,1,0.01)
y = f(x)
lines(x,y,col='red',lwd=2)


# Multivariate Normal (Spectral Decomposition) ====================

rmvn.eigen = function(n, mu, Sigma) 
{
  # generate n random vectors from MVN(mu, Sigma)
  # dimension is inferred from mu and Sigma
  d = length(mu)
  ev = eigen(Sigma, symmetric = TRUE)
  lambda = ev$values
  V = ev$vectors
  R = V %*% diag(sqrt(lambda)) %*% t(V)
  Z = matrix(rnorm(n*d), nrow = n, ncol = d)
  X = Z %*% R + matrix(mu, n, d, byrow = TRUE)
  return(X)
}

# mean and covariance parameters
mu = c(0, 1)
Sigma = matrix(c(1, .9, .9, 1), nrow = 2, ncol = 2)


# generate the sample
X = rmvn.eigen(1000, mu, Sigma)

plot(X, xlab = "x", ylab = "y", pch = 20)
colMeans(X)
var(X)


