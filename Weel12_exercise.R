set.seed(2000)
n = 30; p =50
X = matrix(rnorm(n*p), ncol = p)
X = cbind(rep(1,n), X)

beta = c(rep(1,10), rep(0,p-9))
y = X %*% beta + 0.5*rnorm(n)
lam = 5
fn = function(x){
  sum((y - X%*%x)^2) / (2*n) + 0.5*lam*sum(x[2:(p+1)]^2)
}

par = rep(0, p+1)
results = optim(par, fn, method="BFGS")
results$par
