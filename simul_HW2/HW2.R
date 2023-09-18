
############################# 1. #########################################
# generate random number
theta_hat <- NULL
for (j in 1:10000){
  X <- rexp(10,2) # mean이 아닌 rate이기 때문에 값 그대로 rate argument에 넣는다.
  Y <- rexp(10,3)
  Z <- NULL
  for (i in 1:length(X)){
    Z <- c(Z, min(X[i],Y[i]))
  }
  
  theta_hat <- c(theta_hat, mean(Z))
}

# standard error
cat("standard error: ", sd(theta_hat), "\n")

# confidence interval
# 95% CI for theta
a = seq(0.001,0.049,0.001)
CI = NULL

for (k in a)
{
  conf = quantile(theta_hat, prob=c(k,(0.95+k)))
  CI = rbind(CI,conf)
}


leng = CI[,2] - CI[,1]
CI[which.min(leng),]

############################# 2. #########################################
# chi square
alpha <- 0.05

print("chi square")
for (number in c(10, 30, 50)){
  m <- 10000
  I = numeric(m)
  for (i in 1:m){
    X=rchisq(1:number, df=1)
    Tj = (mean(X) - 1)/(sd(X)/sqrt(number))
    if ((Tj > qt((1-alpha/2),df=(number-1)))|(Tj < qt((alpha/2),df=(number-1)))) I[i]=1
    else I[i]=0
    }
  # Empirical Type I error rate;
  TypeI = mean(I)
  cat("n: ", number,", type1 error: ", TypeI, "\n")
}

# Uniform

print("Uniform")
for (number in c(10, 30, 50)){
  m <- 10000
  I = numeric(m)
  for (i in 1:m){
    X=runif(number,0,2)
    Tj = (mean(X) - 1)/(sd(X)/sqrt(number))
    if ((Tj > qt((1-alpha/2),df=(number-1)))|(Tj < qt((alpha/2),df=(number-1)))) I[i]=1
    else I[i]=0
  }
  # Empirical Type I error rate;
  TypeI = mean(I)
  cat("n: ", number,", type1 error: ", TypeI, "\n")
}

# exponential
print("exponential")
for (number in c(10, 30, 50)){
  m <- 10000
  I = numeric(m)
  for (i in 1:m){
    X=rexp(number,1)
    Tj = (mean(X) - 1)/(sd(X)/sqrt(number))
    if ((Tj > qt((1-alpha/2),df=(number-1)))|(Tj < qt((alpha/2),df=(number-1)))) I[i]=1
    else I[i]=0
  }
  # Empirical Type I error rate;
  TypeI = mean(I)
  cat("n: ", number,", type1 error: ", TypeI, "\n")
}

print("The result shows that if n is bigger, then Type 1 error rate becomes smaller.")

############################# 3. #########################################
coverage = 0
p <- 0.3
alpha <- 0.05

p_hat_set <- NULL
for (j in 1:10000){
  X <- rbinom(10,1,0.3)
  p_hat <- mean(X)
  
  z <- qnorm(1-alpha/2)
  p_hat_set <- c(p_hat_set, p_hat)
  lowerinterval <- p_hat - z * sqrt(p_hat*(1-p_hat)/10)
  upperinterval <- p_hat + z * sqrt(p_hat*(1-p_hat)/10)
  if ((lowerinterval < p) & (p < upperinterval)){
    coverage = coverage+1
  }
}

# calculated type1 error rate
cat("the rate of coverage ratio: ", (coverage/10000))

print("It doesn't work well")

############################# 4. #########################################
LSE <- NULL
WLSE <- NULL
X <- runif(30,1,10) # generate X
W <- diag(X) # weight

for (j in 1:10000){
  f <- function(x){
    error <- NULL
    X <- x
    for(i in 1:30){
      error <- c(error, rnorm(1, 0, sqrt(36/X[i]))) # generate errors
    }
    Y <- 2 + 3*X + error # generate Y
    return(Y)
  }
  Y <- f(X)
  
  X_matrix <- cbind(1,X)
  LSE <- c(LSE, (solve(t(X_matrix) %*% X_matrix) %*% t(X_matrix) %*% Y)[2]) # caculate LSE
  WLSE <- c(WLSE, (solve(t(X_matrix) %*% W %*% X_matrix) %*% t(X_matrix) %*% W %*% Y)[2]) # caculate WLSE
}

cat("MSE","\n", "LSE: ", mean((3-LSE)^2), "\n", "WLSE: ", mean((3-WLSE)^2), "\n", "Result: WLSE is better than LSE")

