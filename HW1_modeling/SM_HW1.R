############################################### HW1 ###########################
library(data.table)
library(tidyverse)

# set the directory of the file
setwd('Type your directory')
getwd()

#############################################################################
################################## P4.########################################
#############################################################################

# load the file
data <- fread("Q4.csv")

# Check the sturcture of the file
data %>% str
data %>% head

# ############### (1) ######################################################
f <- function(beta, x){
  ys <- beta[1] + beta[2]*exp(beta[3] * x)
  
  return(ys)
}

# Objective function: RSS
RSS = function(beta,Y,X) sum((Y-f(beta,X))^2)

# find parameter
ml1 = optim(rep(0.1, 3), RSS, method='BFGS', X=data$X, Y=data$Y)
beta.hat = ml1$par

# calculate Y hat using parameter
Yhat = f(beta.hat,data$X)
r = data$Y - Yhat
par(mfrow = c(1,1))
plot(Yhat,r)

print(paste("Estimated parameters bata hat: ", beta.hat))

# ############### (2) ######################################################
# define fucntion for caculating dwtest
dwtest_residual <- function(x){
  r <- x
  sum <- 0
  for (i in 1:80){
    sum <- sum + r[i]^2
  }
  
  deduction_result <- 0
  for (j in 2:80){
    deduction_result <- deduction_result + (r[j] - r[j-1])^2
  }
  
  return(deduction_result / sum)
}

# the result is 0.68==> normal assumption is violated
dwtest_residual(r)

# ############### (3) ######################################################
library(astsa)
acf2(r)

ar1 = sarima (r, 1,0,0, no.constant=T)   #AR(1)
ar1$fit

X_matrix = cbind(1,exp(data$X), data$X)
Y = data$Y
n = length(Y)
S = diag(rep(1,n))    # initial covariance matrix

mdif = 1000000
beta.old = beta.hat

while(mdif > 0.0000001)
{
  beta.new = as.vector(solve(t(X_matrix) %*% solve(S) %*% X_matrix) %*%t(X_matrix) %*% solve(S) %*% Y)
  r = as.vector(Y - f(beta.new,data$X))
  ar1 = sarima (r, 1,0,0, no.constant=T, details=F)
  alpha = ar1$fit$coef
  sigma2 = ar1$fit$sigma2
  
  mdif = max(abs(beta.new - beta.old))
  beta.old = beta.new
  # Construct covariance matrix
  S = matrix(nrow=n,ncol=n)
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      if (i == j) S[i,j] = 1
      if (i != j) S[i,j] = alpha^(abs(i-j))
    }
  }
  S = (sigma2 / (1-alpha^2)) * S
}

par(mfrow = c(1,1))
Yhat <- f(beta.new, data$X)
r <- Y - Yhat

# ACF $ PACF를 통해 자기 상관 확인
acf2(r)
print(paste("beta hat: ", beta.new))
print(paste("Estimated parameter sigma2 of AR(1): ", round(sigma2,4)))
print(paste("Estimated parameter alpha of AR(1): ", round(alpha,4)))
# ############### (4) ######################################################
# covariance matrix
print("Sigma")
S
#############################################################################
################################## P6.########################################
#############################################################################

# load the file
data <- fread("Q6.csv")

# Check the sturcture of the file
data %>% str
data %>% head

################################# (1) ###################################### 
##### Linear regression #####
fit1 = lm(Y ~ ., data=data)
par(mfrow = c(2,2))
plot(fit1)

# X2의 상관관계가 유의하지 않게 나옴
fit1 %>% summary

# stepwise
# 변수 선택법 사용시 X2를 제거하는 것이 가장 좋은 변수 셋으로 나옴
step(lm(Y ~., data), scope = list(lower ~ 1, upper = ~X1 + X2 + X3), direction = "both")


############################# (2) #################################
# X 변수 셋에서 X2제거
fit2 = lm(Y ~ X1 + X3, data)
par(mfrow = c(2,2))
plot(fit2)

# residual 플랏은 보기 좋게 나왔지만, correlation을 확인하기 위해 dwtest 시행
# 그 결과 autocorrelation이 있는 것으로 확인됨.
# 따라서 추가적으로 모델의 fitting을 향상시키기 위해 linear 외의 parametric한 모델을 찾기로 결정.
library(lmtest)
dwtest(fit2)

##### GAM #####
library(gam)

# 정답 코드
# fit = gam(Y ~ s(X1,5) + s(X2,5)+s(X3,5),data=data) 
# summary(fit)
# par(mfrow=c(1,3))
# plot(fit)


fit2 = gam(Y ~ s(X1,4) + s(X3,4), data = data)
par(mfrow=c(1,2))
plot(fit2)
fit2_1 = gam(Y ~ X1 + X3, data = data)
anova(fit2,fit2_1)

# 결과에 따라 X1은 선형의 관계, X2와는 X^2의 관계가 있다고 판단
f = function(beta,X)
{
  X1 = X[,1]; X2 = X[,2]  
  beta[1] + beta[2]*X1 + beta[3]*X2 + beta[4]*X2^2
}

# Objective function: RSS
RSS = function(beta,Y,X) sum((Y-f(beta,X))^2)

# Gradient vector of the objective function
grv = function(beta,Y,X)
{
  X1 = X[,1]; X2 = X[,2]
  R = Y - f(beta,X)
  c(-2*sum(R), -2*sum(R*X1),-2*sum(R*X2), -2*sum(R*X2^2))  
}

# Optimization
X = cbind(data$X1,data$X3)
colnames(X) = c('X1', 'X3')
Y = data$Y
ml1 = optim(rep(0.1,4), RSS, gr=grv, method='BFGS', X=X, Y=Y)
ml1

beta.hat = ml1$par
beta.hat

# Fitted value
Yhat = f(beta.hat,X)

# Residual plot
r = Y - Yhat
par(mfrow=c(1,1))
plot(Yhat,r)

# 분산이 constant하지 않음
# 따라서 분산을 잡아주는 작업 실시

library(matrixcalc)

# Objective function for mean function: Genearalized least square method.
obj.mean = function(beta,Y,X,S) t(Y-f(beta,X)) %*% solve(S) %*% (Y-f(beta,X))

# S: Covariance matrix

beta.new = ml1$par      # initial parameter.
W = diag(rep(1,length(Y)))
mdif = 100000

while(mdif > 0.000001)
{
  Yhat = f(beta.new,X)
  r = Y - Yhat
  Z = cbind(1,Yhat)
  gam.hat = solve(t(Z) %*% W %*% Z) %*% t(Z) %*% W %*% abs(r)
  sigma = Z %*% gam.hat
  S = diag(as.vector(sigma^2))
  
  if (is.non.singular.matrix(S)) W = solve(S)
  else W = solve(S + 0.000000001*diag(rep(1,nrow(S))))
  
  ml2 = optim(beta.new, obj.mean,method='BFGS', Y=Y, X=X, S=S)
  beta.old = beta.new
  beta.new = ml2$par
  mdif = max(abs(beta.new - beta.old))
}

beta.new

Yhat = f(beta.new,X)
sigma = Z %*% gam.hat
r = (Y - Yhat)/sigma

############################# (3) #################################
# 최종 Residual plot
# 분산도 잡힌 것을 확인.
plot(Yhat,r)
# 1.9의 값으로 2에 가까운 값이 나와 autocorrelation도 잡힌 것으로 확인.
dwtest_residual(r)

############################# (4) #################################
# X1은 Y와 선형의 관계, X2는 Y와 -X^2의 관계가 있다.

