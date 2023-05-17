##########################################################
###################### 1. ################################
##########################################################


##################### Basic setting ######################
##########################################################

# Call the libraries
library(data.table)     # for "fread" function
library(tidyverse)      # for pipe line
library(psych)          # for "describe" function
library(moments)        # for "skewness" function
library(caret)          # for "cv" function
library(randomForest)   # for randomforest modeling
library(lubridate)      # for the time variable controlling
library(lmtest)         # for the dw test
library(xgboost)        # for xgboost modeling
library(astsa)          # for checking autocorrelation

# for analysis of missing pattern
library(mice)
library(finalfit)
library(rms)

# for transformation
library(bestNormalize)

# Set the directory of the data files
setwd("Type your directory")
getwd()                 # check the path

# Load the data
train <- fread("train.csv")
test <- fread("test.csv")

################# Checking data structure ################
##########################################################

# Check the data structure
train %>% head
train %>% str
train %>% describe

# Check the missing pattern
train %>% md.pattern    # Result shows that there are more missing rows
                        # than the others which missed one values solely
sum(is.na(train$Y))     # Also, there aren't any missing values for Y

# Check the missing pattern based on Y variables
train %>% missing_pairs('Y', c('X1', 'X2', 'X3'))
                        # The result shows that there are some strange point in the case of
                        # missing X3 variables. The plot seems that centers are different 
                        # between missing data and observed data

# Clustering for variables with missing values for the same obs.
train %>% md.pairs()

missing.clus <- train %>% naclus(method='average')
missing.clus
missing.clus %>% plot()
                        # The result shows that X1 and X2 are more likely to be missed together
                        # also same result for X3 and X4


############## Applying MICE for imputation ##############
##########################################################

# set seed
set.seed(0)

# impute
imp <- mice(train, m=10, method = c('rf', 'pmm', 'pmm', 'pmm', ''), print=F)
                        # The number of imputed sets is 10 and didn't impute
                        # Y cause there is no missing value

# predictors for imputation of each column
imp$predictorMatrix     # Matrix shows that Y is used for imputation

# Check the effect of Y variables
pred <- imp$predictorMatrix
pred[,'Y'] <- 0
pred                    # delete the interference of Y

# impute again with deletion of influence of Y variable
set.seed(0)
imp1 <- mice(train, m=10, method = c('rf', 'pmm', 'pmm', 'pmm', ''),predictorMatrix = pred, print=F)
imp1$predictorMatrix    # Y is deleted from predictors' factor

# Check the imputed values for X1 this time
imp1$imp$X1

# Check the imputed set (just for first set this time)
imp1 %>% complete(1)

# Checking the convergence of MICE
plot(imp1, c('X1', 'X2', 'X3', 'X4'))
                        # The lines are tangled, it seems to converge


######### Visualization of final imputed data ############
##########################################################

# Compare the imputed data and observed data
imp1 %>% stripplot(pch=20, cex=1.2)
                        # The imputed values are well spread

imp1 %>% densityplot(scales=list(relation='free', layout=c(4,1)))
                        # The distribution between blue line and red line look
                        # a lit similar. It means that data are imputed well.


############# Prediction model with MICE #################
##########################################################

# set seed
set.seed(0)

# Apply prediction model to each imputed dataset.
fit <- with(imp1, lm(Y~X1+X2+X3+X4))

# Model Averaging
fit %>% pool %>% summary
                        # The p-values of each variable are very small!

# Even though in this analysis we used imputed dataset which didn't use Y,
# just want to know the effect of the imputation.
# So, apply prediction model for original complete set.


############## Checking imputation effect #################
##########################################################


comp.dat <- train %>% na.omit         # Make complete data set
fit1 <- lm(Y~X1+X2+X3+X4, comp.dat)   # Model fit
fit1 %>% summary                      # Result shows that imputation didn't affect
                                      # the final result a lot.

############# Prediction of test dataset #################
##########################################################

M <- imp1$m           # Save the number of imputed dataset

# For prediction, make imputed dataset as list
imp.dat <- vector(mode='list', length = M)
for (m in 1:M) imp.dat[[m]] = complete(imp1,m)

# Define model for prediction
p.model <- function(dat) lm(Y~X1+X2+X3+X4, data = dat)

# Fit the model to the imputed dataset
fit.imp <- lapply(imp.dat, p.model)

# Predict Y from test dataset
yhat <- lapply(fit.imp, predict, newdata=test)
yhat <- matrix(unlist(yhat), nrow(test), M)

# Calculating mean
mean_y <- mean(test$Y)        # save the mean of each column

# Final MSE
mean(apply((yhat-mean_y)^2, 2, mean))   # Average MSE 

# print residual plot (from 4 sets)
par(mfrow=c(2,2))
plot(yhat[,1], yhat[,1]-test$Y) + plot(yhat[,2], yhat[,2]-test$Y) +
  plot(yhat[,3], yhat[,3]-test$Y) + plot(yhat[,3], yhat[,3]-test$Y)


# It seems linear model can't represent the data
############# Applying Random Forest #####################
##########################################################

# Even though the p-value looks good, when draw the residual plot and
# scatter plots of each variable for Y,
# they show the relationships are not a linear.
par(mfrow=c(2,2))
plot(train$Y, train$X1) + plot(train$Y, train$X2) + plot(train$Y, train$X3) +
  plot(train$Y, train$X4)

                                        # X2 and X4 are not a complete linear

rf_params <- expand.grid(mtry = c(2, 3, 4),
                         ntree = c(100, 200, 300),
                         MSE = NA)    # making a parameter sets
                                      # for parameter tuning

# apply random forest
rf_MSE <- NULL                          # for saving MSE of random forest
for (k in 1:M){
  set <- complete(imp1,k)               # choose set form imputed data set
  cv <- createFolds(set$Y, k = 5)       # making indexes for a cross validation 
  
  # 5-fold CV
  for (i in 1:nrow(rf_params)){
    MSE_tmp <- NULL
    
    for(j in 1:5){
      set.seed(0)
      
      valid_idx <- cv[[j]]
      
      cv_train_set <- set[-valid_idx, ] # set the train set
      cv_valid_set <- set[valid_idx, ]  # set the validation set
      
      # Modeling
      rf_model <- randomForest(Y~., cv_train_set, 
                               mtry = rf_params$mtry[i],
                               ntree = rf_params$ntree[i])
      # Predict
      rf_yhat <- predict(rf_model, newdata = cv_valid_set)
      
      # calculate MSE
      MSE_tmp[j] <- mean((rf_yhat - cv_valid_set$Y)^2)
    }
    rf_params$MSE[i] <- mean(MSE_tmp)
  }
  
  # Print the result of the parameter tuning
  cat(k, "번째 set의 파라미터 튜닝결과:\n")
  print(rf_params)
  
  # Save the best parameter and print it.
  rf_best_param <- rf_params %>% filter(MSE == min(MSE))
  cat("best parameter:\n")
  print(rf_best_param)
  
  # Training based on whole train data
  rf_model_te <- randomForest(Y~., set, 
                              mtry = rf_best_param$mtry,
                              ntree = rf_best_param$ntree)
  
  # Predict
  rf_yhat <- predict(rf_model_te, test)
  
  par(mfrow = c(1,1))
  
  # draw residual plot
  plot(rf_yhat, rf_yhat - test$Y)     # better than linear model.
  
  # calculalte for each imputed set
  rf_MSE[k] <- mean((rf_yhat- test$Y)^2)
  cat(k, "번째 set의 MSE", rf_MSE[k])
}

# Final average MSE
mean(rf_MSE)

print("The MSE value is incredibly reduced!!")
##########################################################
###################### 2. ################################
##########################################################

# Load the data
pm25_tr <- fread("pm25_tr.csv")
pm25_te <- fread("pm25_te.csv")

################## data pre-processing ###################
##########################################################

# Check the data structure
pm25_tr %>% head
pm25_tr %>% str
pm25_tr %>% describe

# No missing data in data set. 
pm25_tr %>% apply(2, is.na) %>% colSums

# Factorization for "cbwd" variable
pm25_tr$cbwd %<>% as.factor() 
pm25_te$cbwd %<>% as.factor()

# Create time variable
pm25_tr %<>% 
  mutate(
  t = make_datetime(year, month, day, hour)
)
pm25_te %<>% 
  mutate(
    t = make_datetime(year, month, day, hour)
)

# Draw plot of fluctuation of pm25 based on time.
plot(pm25_tr$t, pm25_tr$pm25, type='l', col="red", main="pm25 based on time", xlab="Time", ylab="pm25")
# seems like there are some pattern
# depend on day.

# Draw plot of fluctuation of pm25 based on day.
plot(pm25_tr$day, pm25_tr$pm25, pch = 20, col="blue", main="pm25 based on day", xlab="day", ylab="pm25")
abline(v =6, col = 2, lty = 2, lwd = 2)
abline(v =13, col = 2, lty = 2, lwd = 2)
abline(v =23, col = 2, lty = 2, lwd = 2)

# There are some section which pm25
# become lower than normal day.
# Therefore create new factor variable
# divided by 4 sections.

# Create factor variables
pm25_tr %<>% 
  mutate(
    day_depend = ifelse(pm25_tr$day < 7, "first_Week", 
                        ifelse(pm25_tr$day < 14, "second_Week", 
                               ifelse(pm25_tr$day < 24, "third_Week", "last_Week")))
  )
pm25_tr$day_depend %<>% factor(levels = c("first_Week", "second_Week", "third_Week", "last_Week"))

pm25_te %<>% 
  mutate(
    day_depend = ifelse(pm25_te$day < 7, "first_Week", 
                        ifelse(pm25_te$day < 14, "second_Week", 
                               ifelse(pm25_te$day < 24, "third_Week", "last_Week")))
  )
pm25_te$day_depend %<>% factor(levels = c("first_Week", "second_Week", "third_Week", "last_Week"))

# skewness check
par(mfrow=c(2,2))
hist(pm25_tr$DEWP, main="DEWP", xlab = skewness(pm25_tr$DEWP)) 
hist(pm25_tr$TEMP, main="TEMP", xlab = skewness(pm25_tr$TEMP))
hist(pm25_tr$PRES, main="PRES", xlab = skewness(pm25_tr$PRES))
hist(pm25_tr$Iws, main="Iws", xlab = skewness(pm25_tr$Iws))
par(mfrow=c(1,1))
hist(pm25_tr$pm25, main="pm25", xlab = skewness(pm25_tr$pm25))
                                      #The "Iws" variable is skewed

# Apply Yeo-Jonson transformation to "Iws" variable
pm25_tr$Iws <- yeojohnson(pm25_tr$Iws)$x.t
pm25_tr$pm25 <- yeojohnson(pm25_tr$pm25)$x.t

pm25_te$Iws <- yeojohnson(pm25_te$Iws)$x.t
pm25_te$pm25 <- yeojohnson(pm25_te$pm25)$x.t

# re-draw the histogram of "Iws"
par(mfrow=c(1,2))
hist(pm25_tr$Iws, main="Iws", xlab = skewness(pm25_tr$Iws))
hist(pm25_tr$pm25, main="pm25", xlab = skewness(pm25_tr$pm25))
                                        # skewness problem is solved

# check if data have data leakage problem
sum(pm25_tr$t >= ymd("2010-05-21"))  # No data over May 20

# remove useless variables
pm25_tr$year <- NULL         # Delete because every data have same value
pm25_tr$day <- NULL          # Delete because have created "day_depend" function
par(mfrow=c(1,1))
plot(pm25_tr$hour, pm25_tr$pm25)
pm25_tr$hour <- NULL         # Delete because hour doesn't have significant pattern
                             # based on pm25
plot(pm25_tr$month, pm25_tr$pm25)
pm25_tr$month <- NULL

pm25_te$year <- NULL
pm25_te$day <- NULL
pm25_te$hour <- NULL
pm25_te$month <- NULL

################# XG_boost Modeling ######################
##########################################################

# set seed
set.seed(0)

# Make factor variables as dommy cause XGBoost only take numeric input
onehot <- dummyVars(" ~.", data = pm25_tr)
train_onehot <- predict(onehot, newdata = pm25_tr) %>% as.data.frame
test_onehot <- predict(onehot, newdata = pm25_te) %>% as.data.frame

# Parameter sampling for random search
max_depth <- sample(4:10, 10, replace = TRUE)
min_child_weight <- sample(4:10, 10, replace = TRUE)
subsample <-runif(min = 0.5, max = 1, n = 10)
colsample_bytree <- runif(min = 0.5, max = 1, n = 10)

# Create data fram for showing result.
xgb_result <- data.frame(iterations = 1:10, 
                         max_depth = max_depth,
                         min_child_weight = min_child_weight,
                         subsample = subsample,
                         colsample_bytree = colsample_bytree,
                         MSE = NA)

# Modeling
for(i in 1:10){
  MSE_tmp <- NULL
  
  cv <- createFolds(train_onehot$pm25, k = 5)       # making indexes for a cross validation 

  # 5-fold cross validation
  for(j in 1:5){
    set.seed(0)
    valid_idx <- cv[[j]]
    
    # make a matrix for XGBoost
    cv_train_set <- xgb.DMatrix(data = as.matrix(train_onehot[-valid_idx, -1]), 
                                label = as.matrix(train_onehot[-valid_idx, 1]))
    cv_valid_set <- xgb.DMatrix(data = as.matrix(train_onehot[valid_idx, -1]), 
                                label = as.matrix(train_onehot[valid_idx, 1]))
    
    # fit
    xgb_model <- xgboost(data = cv_train_set,
                         max_depth = max_depth[i], 
                         min_child_weight = min_child_weight[i],
                         subsample = subsample[i],
                         colsample_bytree = colsample_bytree[i],
                         eta = 0.01, nrounds = 1000, early_stopping_rounds = 0.05*1000, 
                         print_every_n = 50)
    
    # Predict
    xgb_yhat <- predict(xgb_model, newdata = cv_valid_set)
    
    # Calculate MSE
    MSE_tmp[j] <- mean((xgb_yhat - train_onehot[valid_idx, 1])^2)
  }
  
  # Save MSE
  xgb_result$MSE[i] <- mean(MSE_tmp)
}
xgb_result

# Choose best parameter
xgb_best_param <- xgb_result %>% filter(MSE == min(MSE))


# training based on whole train data
xgb_train <- xgb.DMatrix(data = as.matrix(train_onehot[, -1]), label = as.matrix(train_onehot[, 1]))
xgb_test <- xgb.DMatrix(data = as.matrix(test_onehot[, -1]), label = as.matrix(test_onehot[, 1]))

# Fitting
xgb_model <- xgboost(data = xgb_train,
                     max_depth = xgb_best_param$max_depth,
                     min_child_weight = xgb_best_param$min_child_weight,
                     subsample = xgb_best_param$subsample,
                     colsample_bytree = xgb_best_param$colsample_bytree,
                     eta = 0.01, nrounds = 1000, early_stopping_rounds = 0.05*1000, 
                     print_every_n = 50)

# Predict
xgb_yhat <- predict(xgb_model, newdata = xgb_test)

# Final MSE
xgb_MSE <- mean((xgb_yhat - test_onehot[,1])^2)
xgb_MSE

########### Considering Auto correlation #################
##########################################################

# draw a plot of residual
r <- test_onehot[,1] - xgb_yhat
plot(r)                                     

# However, it is a time-series data so check the autocorrelation using dwtest
dw.stat = sum((r[2:length(xgb_yhat)] - r[1:(length(xgb_yhat) - 1)])^2) / sum(r^2) 
dw.stat                                     # The value is a bit far from 2

# Check the correlation using ACF graph.
acf2(r)
                                            # Graph shows that this case is AR(1)

# Apply AR(1) to the XGBoost
pm25_tr$lagged_pm25 <- lag(pm25_tr$pm25, 1)
pm25_te$lagged_pm25 <- lag(pm25_te$pm25, 1)

lag_pm25_tr <- pm25_tr[-1,]                 # delete first row which has NA value
lag_pm25_te <- pm25_tr[-1,]

lag_pm25_tr$pm25 <- NULL                    # delete "pm25"
lag_pm25_te$pm25 <- NULL


################# Re-modeling with AR(1) #################
##########################################################
# Generate new train and test set
onehot <- dummyVars(" ~.", data = lag_pm25_tr)
train_onehot <- predict(onehot, newdata = lag_pm25_tr) %>% as.data.frame
test_onehot <- predict(onehot, newdata = lag_pm25_te) %>% as.data.frame

# Create data frame for showing result.
xgb_result <- data.frame(iterations = 1:10, 
                         max_depth = max_depth,
                         min_child_weight = min_child_weight,
                         subsample = subsample,
                         colsample_bytree = colsample_bytree,
                         MSE = NA)

# Modeling
for(i in 1:10){
  MSE_tmp <- NULL
  
  cv <- createFolds(train_onehot$lagged_pm25, k = 5)       # making indexes for a cross validation 
  
  # 5-fold cross validation
  for(j in 1:5){
    set.seed(0)
    valid_idx <- cv[[j]]

    # make a matrix for XGBoost
    cv_train_set <- xgb.DMatrix(data = as.matrix(train_onehot[-valid_idx, -14]), 
                                label = as.matrix(train_onehot[-valid_idx, 14]))
    cv_valid_set <- xgb.DMatrix(data = as.matrix(train_onehot[valid_idx, -14]), 
                                label = as.matrix(train_onehot[valid_idx, 14]))
    
    # fit
    xgb_model <- xgboost(data = cv_train_set,
                         max_depth = max_depth[i], 
                         min_child_weight = min_child_weight[i],
                         subsample = subsample[i],
                         colsample_bytree = colsample_bytree[i],
                         eta = 0.01, nrounds = 1000, early_stopping_rounds = 0.05*1000, 
                         print_every_n = 50)
    
    # Predict
    xgb_yhat <- predict(xgb_model, newdata = cv_valid_set)
    
    # Calculate MSE
    MSE_tmp[j] <- mean((xgb_yhat - train_onehot[valid_idx, 14])^2)
  }

  # Save MSE
  xgb_result$MSE[i] <- mean(MSE_tmp)
}
xgb_result

# Choose best parameter
xgb_best_param <- xgb_result %>% filter(MSE == min(MSE))


# training based on whole train data
xgb_train <- xgb.DMatrix(data = as.matrix(train_onehot[, -14]), label = as.matrix(train_onehot[, 14]))
xgb_test <- xgb.DMatrix(data = as.matrix(test_onehot[, -14]), label = as.matrix(test_onehot[, 14]))

# Fitting
xgb_model <- xgboost(data = xgb_train,
                     max_depth = xgb_best_param$max_depth,
                     min_child_weight = xgb_best_param$min_child_weight,
                     subsample = xgb_best_param$subsample,
                     colsample_bytree = xgb_best_param$colsample_bytree,
                     eta = 0.01, nrounds = 1000, early_stopping_rounds = 0.05*1000, 
                     print_every_n = 50)

# Predict
xgb_yhat <- predict(xgb_model, newdata = xgb_test)

# Final MSE
xgb_MSE <- mean((xgb_yhat - test_onehot[,14])^2)
xgb_MSE

# draw a plot of residual
r <- test_onehot[,14] - xgb_yhat
plot(r) 

dw.stat = sum((r[2:length(xgb_yhat)] - r[1:(length(xgb_yhat) - 1)])^2) / sum(r^2) 
dw.stat

print(paste("RESULT: MSE is significantly become small and residual plot shows that",
            "this model explain everything about the data"))
