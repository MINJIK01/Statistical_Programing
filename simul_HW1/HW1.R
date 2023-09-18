###############################Statistical Simulation HW1#######################
################################################################################

# 1. Consider the binomial distribution with a parameter (n = 5, p = 0.2)
# 1) Generate 100 random numbers using the inverse transformation method.

binomial_inverse = function(n,size, p)
{
  U=runif(n) # generate random numbers following uniform distribution
  
  sum = 0
  table = c()
  for (t in 0:size){
    sum = sum + choose(size,t)*p^t*(1-p)^(size-t)
    table = c(table, sum)
  }

  # check where does U locate
  X = c()
  for (i in 1:n){
    for (j in 1:(size+1)){
      if (U[i] <= table[j]) {
        X = c(X, j-1)
        break
      }
    }
  }
  
  length(X)
  
  return(X)
}

binomial_inverse(100, 5, 0.2)

# 2) Generate 100 random numbers using the the transformation method.

# function for generating Beroulli(p)
Ber.R = function(n,p)
{
  U = runif(n)
  X = ifelse(U <= p,1,0)
  return(X)
}

# rbinom(n, 1,...) 해서 사용할 수도 있을 듯.
# generate binomial random number
binomial_transform = function(n,size, p){
  X = c()
  for (i in 1:n){
    x = sum(Ber.R(size,p))
    X = c(X,x)
  }
  
  return(X)
}

binomial_transform(100,5,0.2)

# 두 값의 차이를 계산하는 것이 좋을 거 같다.
# 3) Calculate the mean and variance for random numbers generated from parts (1) and (2), respectively, and compare them with the theoretical mean and variance.
X = binomial_inverse(100, 5, 0.2)
mean = mean(X)
var = var(X)
print(paste("inverse transformation mean: " , mean,"//Compare with theoreotical mean: ", mean - size*p))
print(paste("inverse transformation var: ", var, "//Compare with theoreotical var: ", var - size*p*(1-p)))

X2 = binomial_transform(100, 5, 0.2)
mean = mean(X2)
var = var(X2)
print(paste("transformation mean: " , mean,"//Compare with theoreotical mean: ", mean - size*p))
print(paste("transformation var: " , var, "//Compare with theoreotical var: ", var - size*p*(1-p)))


################################################################################

# 2. Generate 100 Poisson (lambda = 2) random numbers using the inverse transformation method, and then compare with the theoretical mean and variance.

pois = function(n, lambda){
  U=runif(n)
  
  X = NULL
  for (i in 1:n){
    y = 0
    p = exp(-lambda)
    f = p
    while(U[i] >= f){
      p = (lambda/(y+1))*p
      f = f + p
      y = y + 1
    }
    X = c(X,y)
  }
  
  return(X)
}

pois(100,2)
# compute mean and variance
print(paste("mean: ", mean(pois(100, 2))))
print(paste("var: " , var(pois(100, 2))))

################################################################################

# 3. Consider the pdf of the random variable X as follow:


# 1) Generate 1,000 random numbers of X using the inverse transformation method and estimate E(X) and Var(X) using the 1,000 random numbers.
func = function(n){
  U = runif(n)     #uniroot(function(x) (x^2) / 4 + x / 2 + 1 / 4 - rand_unf, lower = -1, upper = 1, tol = 0.0001)$root
  X = 2*sqrt(U) - 1
  
  return(X)
}

func(1000)
X = func(1000)

print(paste("mean: ", mean(X)))
print(paste("var: " ,var(X)))

# 2) Let Y = X^2. Estimate E(Y) and Var(Y) using the 1,000 random numbers obtained part(1).
Y = X^2
print(paste("mean: ", mean(Y)))
print(paste("var: " ,var(Y)))


################################################################################

# 4. Suppose that we want to generate random numbers from the following distribution:
# Let g(x) be the uniform (0,1) density. To generate random numbers from f(x), consider the acceptance-rejection method.

# 1) Obtain min(c: f(x)/g(x) <= c)

min_c = 3/2

# 2) Using the acceptance-rejection method and c obtaind in part (1), generate 100 random numbers from f(x), and then compute the mean and variance of the 100 random numbers.
n = 100

X = NULL
N = 0
trials = 0
while (N <= n){
  Y = runif(1)
  U = runif(1)
  C = 4 * Y * (1-Y)
  X = c(X,Y[U <= C])
  N = length(X)
  if (N > n) X = X[1:n]
  trials = trials + 1
}

print(paste("mean: ",mean(X)))
print(paste("var: " ,var(X)))

# 3) in part(2), estimate the average number of trials for an acceptance occurs. (i.e, (the total number of trials to get 100 random numbers from f(x)) / 100)

trials / 100



################################################################################

# 5. Generate 200 random numbers from the 3-dimensional multivariate normal distribution with mean vector mu = (0,1,2)' and covariance matrix
# using the Choleski decomposition method. Use the 'paris' R function to plot an array of scatter plots for each pair of variables.


rmvn.eigen = function(n, mu, Sigma) 
{
  # generate n random vectors from MVN(mu, Sigma)
  # dimension is inferred from mu and Sigma
  d = length(mu)
  Z = matrix(rnorm(n*d), nrow = n, ncol = d)
  L = t(chol(Sigma))
  X = Z %*% L + mu
  return(X)
}

# mean and covariance parameters
mu = c(0, 1, 2)
Sigma = matrix(c(1, -0.5, 0.5, -0.5, 1, -0.5, 0.5, -0.5, 1), nrow = 3, ncol = 3)

# generate the sample
X = rmvn.eigen(200, mu, Sigma)

colnames(X) <- c("Var1", "Var2", "Var3")
pairs(~Var1 + Var2 + Var3, data = X)
