# Practice Midterm Exam

# 1. 
# 1)
generate_t <- function(){
  U <- runif(1000)
  X=-3/2*log(1-U)
  return(X)
}
T <- generate_t()
mean(generate_t())

# 2)
V <- ifelse(T > 3, 2*T, 5)
mean(V)

# 2.
# g = xexp(-x)
# 최고점 갖는 x 값 = 1
c <- exp(1/2)
n = 1000

X = NULL
N = 0
while (N <= n){
  Y = runif(1)
  U = runif(1)
  C = c * exp(-(Y^2)/2 +Y)
  X = c(X,Y[U <= C])
  N = length(X)
  if (N > n) X = X[1:n]
}

X

seta_tilda <- numeric(0)
seta_hat <- numeric(0)

# 3.
for (j in 1:10000){
  X <- rnorm(10,1,2)
  
  seta_tilda <- c(seta_tilda, 1/9 * sum((X-mean(X))^2))
  seta_hat <- c(seta_hat, 1/10 * sum((X-mean(X))^2))
}

# MSE of seta_tilda
mean((1 - mean(seta_tilda))^2)
# MSE of seta_hat
mean((1 - mean(seta_hat))^2)

print("tilda is better than hat")

# 4.

# Compute T
T <- qchisq(0.95, 9)

alpha <- 0.05
I <- numeric(10000)
for (j in 1:10000){
  # generate data under null hypothesis
  X <- rexp(10,1/4)
  
  Tj <- (9 * sd(X)^2) / 4
  if (Tj > T) I[j]=1
  else I[j]=0
}

sum(I) /10000

# 5.

X <- c(4,8,5,3,2,7,8,12,6,6)
real <- median(X)
X <- sort(X)
plot(X)

Y <- X
X <- seq(1,10,1)
fit <- lm(Y ~ X)
beta <- fit$coefficients

f <- function(beta, X){
  Y <- beta[1] + beta[2]*X
}

med_E <- NULL
for (j in 1:10000){
  X <- runif(10,1,10)
  Y <- f(beta, X)
  med_E <- c(med_E, median(Y))
}


mean((real - med_E)^2)
