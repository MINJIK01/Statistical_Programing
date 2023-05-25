##################Q2###############################
###################################################

library(faraway)
data <- sat
str(data)

# (a)
fit1 <- lm(total ~ expend + ratio, data)          # H0: salary == 0
fit2 <- lm(total ~ expend + ratio + salary, data) # H1: salary != 0
anova(fit1, fit2)

# p-value is higher than 0.05 so we can't reject H0. The coefficient of salary
# is 0.

# (b)
fit1 <- lm(total ~ I(expend + ratio + salary), data) # H0: salary = ratio = expend
fit2 <- lm(total ~ expend + ratio + salary, data)    # H1: some coefficients of
                                                    # variables might not be 0.
anova(fit1, fit2)

# p-value is higher than 0.05 so we can't reject H0. The coeeficient of
# salary & ratio & expend is same.

# (c)
## 1)
fit1 <- lm(total ~ expend + ratio + takers, data)           # H0: salary == 0
fit2 <- lm(total ~ expend + ratio + salary + takers, data)  # H1: salary != 0
anova(fit1, fit2)

fit1 <- lm(total ~ I(expend + ratio + salary) + takers, data) # H0: salary = ratio = expend
fit2 <- lm(total ~ expend + ratio + salary + takers, data)    # H1: some coefficients of
                                                              # variables might not be 0.
anova(fit1, fit2)

# The first p-value is higher than 0.05 but second one is lower than 0.05, 
# so we can't reject H0 in first case but can reject in second case.
# It means that those variable could explain (contain) 'salary' variable's effect.
# in 1st case.
# However, in second case, after adding 'takers', it became hard to
# predict Y variable when we consider "expend, ratio, and salary" are the same.
# Therefore, it is hard to consider that
# the coefficients of 'salary', 'ratio', and 'expend' are same.


##################Q3###############################
###################################################

# (b)
data <- gala

fit1 <- lm(Species ~ Elevation + Adjacent, data)
fit2 <- lm(Species ~ Elevation + Scruz + Adjacent + Nearest, data)

anova(fit1, fit2)

# (c)

fit1 <- lm(Species ~ I((-4.5)*Elevation + Adjacent) + Scruz + Nearest, data)

anova(fit1, fit2)


##################Q4###############################
###################################################

library(glmnet)
data <- seatpos
str(data)

# (a)
lam_set1 = seq(0,1000,1)
lam_set2 = seq(0, 10, 0.1)
ridge <- glmnet(data[,-9], data[,9], alpha=0, lambda = lam_set1)
lasso <- glmnet(data[,-9], data[,9], alpha=1, lambda = lam_set2)

par(mfrow = c(1,2))
names = c("Ridge", "Lasso")
result = list(ridge, lasso)
lam = list(lam_set1, lam_set2)

for (i in 1:2){
  plot(lam[[i]], rev(result[[i]]$beta[1,]), type = "l",
       xlim=range(lam[[i]]), ylim=range(result[[i]]$beta),
       main = names[i],
       xlab = expression(lambda), ylab="Coefficients", cex.lab=2)
  lines(lam[[i]], rev(result[[i]]$beta[2,]), col="blue", lty=2)
  lines(lam[[i]], rev(result[[i]]$beta[3,]), col="red", lty=2)
  lines(lam[[i]], rev(result[[i]]$beta[4,]), col="green", lty=2)
  lines(lam[[i]], rev(result[[i]]$beta[5,]), col="grey", lty=2)
  lines(lam[[i]], rev(result[[i]]$beta[6,]), col="orange", lty=2)
  lines(lam[[i]], rev(result[[i]]$beta[7,]), col="yellow", lty=2)
  lines(lam[[i]], rev(result[[i]]$beta[8,]), col="purple", lty=2)
  abline(h=0, lwd=2)
  
  legend("bottomright", legend = expression(beta[Age],
                                            beta[Weight],
                                            beta[HtShoes],
                                            beta[Ht],
                                            beta[Seated],
                                            beta[Arm],
                                            beta[Thigh],
                                            beta[Leg]),
         col=c("blue", "red", "green", "grey", "orange", "yellow", "purple"), lty=1:8, cex=1)
}


##################Q5###############################
###################################################

# (b)
set.seed(1000)
n = 100
p = 200
X = matrix(rnorm(n*p), ncol = p)
X = cbind(rep(1,n), X)

norm_vec = sqrt(apply(X^2, 2, mean))
X = X / matrix(rep(norm_vec, each = n), nrow = n)

beta = runif(p+1)
beta[6:(p+1)] = 0                           # beta 개수 수정
y = X %*% beta + 0.1*rnorm(n)               # 기준 수식 for normal Lasso


# normal Lasso for calculating beta-tilda
lasso = glmnet(X[,2:(p+1)], y, fammily = "gaussian", alpha = 1, lambda = 0.02*sqrt(log(p)/n))
lasso_beta_tilda = rbind(lasso$a0, lasso$beta)                # get the beta tilda

err = 0.001                                                   # set the error
# if beta-tilda is lower than 0.001, than change the value
lasso_beta_tilda <- ifelse(abs(lasso_beta_tilda) <= 0.001, 0.001, abs(lasso_beta_tilda))

# for the Adaptive Lasso
# create x-tilda
for (j in 2:(p+1)){
  X[,j] <- X[,j] / lasso_beta_tilda[j - 1]
}

# penalty parameter for Adaptive lasso
lam_Lasso =seq(0.001 , 1, 0.001) * sqrt(log(p)/n)

# Adaptive lasso
Adap_lasso = glmnet(X[,2:(p+1)], y, fammily = "gaussian", alpha = 1, lambda = lam_Lasso)
lasso_beta = rbind(Adap_lasso$a0, Adap_lasso$beta)            # get the beta

# calculate errors
err_lasso = NULL
for (i in 1:length(lam_Lasso)){
  err_lasso = c(err_lasso, sqrt(sum((lasso_beta[,i] - beta)^2)))
}
err_lasso = rev(err_lasso)

names = "Errors plot"
result = err_lasso
lam = lam_Lasso

# draw plot
par(mfrow = c(1,1))
plot(lam, result, type = "l", xlim = range(lam), ylim=range(result), lwd = 2, main = names,
    xlab=expression(lambda), ylab="Estiamtion errors")
ind = which(err_lasso == min(err_lasso))
round(lasso_beta[,ncol(lasso_beta)-ind+1], 3)

# The smallest error value.
min(result)
