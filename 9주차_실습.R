library(faraway)
library(tidyverse)

# data loading
data(stat500)

# Check the structure of the data
stat500 %>% head
stat500 %>% str
stat500 %>% dim
stat500 %>% colnames

# scatter plot
plot(final~midterm, data = stat500)

# apply linear regression
fit <- lm(final~midterm, data = stat500)

# plotting
par(mfrow=c(2,2))
fit %>% plot

par(mfrow=c(1,1))
abline(fit %>% coefficients(), col = "red")

# summary
fit %>% summary

################################################
fit2 <- lm(final ~ midterm + hw, data = stat500)
fit2 %>% summary

################################################
X <- stat500[, c(1,3)]; y <- stat500[,2]
X <- cbind(rep(1,nrow(X)), X)
X <- X %>% as.matrix(); y <- y %>% as.matrix()
OLS <- solve(t(X)%*%X, t(X)%*%y)
OLS

#################################################
#################################################
data(gala)
gala %>% head
gala %>% str

fit3 <- lm(Species~Area+Elevation+Scruz + Adjacent, data = gala)
fit3 %>% summary
par(mfrow=c(2,2))
fit3 %>% plot

##################################################
# R^2
Y <- gala$Species
R2 <- 1 - deviance(fit3) / sum((Y - mean(Y))^2)
R2

# Adjusted R^2
n=30; p=5
R2_adjusted <- 1 - (1-R2)*(n-1)/(n-p-1)
R2_adjusted

#################################################
fit4 <- lm(Species ~ Area + Elevation + Scruz + Adjacent + Nearest, data = gala)
fit4 %>%  summary

# ==> Nearest는 Adusted R-square 오히려 감소 시킴. 즉, 쓸모 없는 변수
F_score <- (sum((fit4 %>% fitted  - mean(Y))^2) / p) / (deviance(fit4) / (n-p-1))
F_score
##################################################
##################################################
# Practice for F test

fit5 <- lm(Species ~ Area+Elevation+Scruz+Adjacent ,gala)
names(fit5)

# Comput F-statistic
n = 30; p = 4
SST <- sum((gala$Species - mean(gala$Species))^2)
SSE <- deviance(fit5)
MSE <- SSE/n-p-1
SSR <- SST - SSE
MSR <- SSR/p
Fstat <- MSR/MSE

crit_value = qf(0.95, p, n-p-1) # save critical value to decide to reject H0 or not
Fstat > crit_value # reject H0
pvalue <- 1 - pf(Fstat, p, n-p-1) #pf:누적 분포//확률 변수 f가 Fstat의 값을 가질 때
pvalue < 0.05
