library(glmnet)

set.seed(06262017)
# First, let's simulate a design matrix (X) with N = 100 data point
N=100
K=100 #num of features

# create 200 rows of zero for each of the models for train and test
mse_test_lasso <- rep(0, 200)
mse_test_ols <- rep(0, 200)
mse_test_ridge <- rep(0, 200)
mse_train_lasso <- rep(0, 200)
mse_train_ols <- rep(0, 200)
mse_train_ridge <- rep(0, 200)

for (simulation in 1:200) {
X = matrix(rnorm(N*K, mean=0, sd=1), N, K)

# let's simulate a K × 1 matrix of coe???cients (??) and a N ×1 matrix of error terms (e) each from a normal distribution
b = matrix(rnorm(1*K, mean=0, sd=1), K, 1)

#sparse beta
#b_sparse = matrix(c(c(5,5,5,5), c(rep0,K-4)), K,1) #matrix of k to 1

e = matrix(rnorm(1*N, mean=0, sd=1), N, 1)

# let us calculate our observed outcomes as: Y = X ????? + e
Y = X %*% b + e

# lasso model
lasso <- cv.glmnet(X, Y, alpha=1, lambda = 10^seq(-5,-1,.1))
#plot(lasso)

# ridge model
ridge <- cv.glmnet(X, Y, alpha=0, lambda = 10^seq(-5,1, .1))
#plot(ridge)

# OLS model
training_data <- data.frame(Y,X)
ols <- glm(Y ~ X, data = training_data)
#head(coef(ols))



###  Repeat this process to generate a corresponding set of test data. 
x_test = matrix(rnorm(N*K, mean=0, sd=1), N, K)

e_test = matrix(rnorm(1*N, mean=0, sd=1), N, 1)



### let us calculate our observed outcomes as: Y = X ????? + e
Y_test = x_test %*% b + e_test

test_data <-data.frame(Y_test, x_test)



### prediction for training data
# ols
predict_ols <- predict(ols, newdata=training_data)
mse_train_ols[simulation] <- mean((training_data$Y - predict_ols)^2)
#mse_train_ols

#lasso
predict_lasso <- predict(lasso, newx=X, s="lambda.min")
#predict_lasso
mse_train_lasso[simulation] <- mean((training_data$Y - predict_lasso)^2)
#mse_train_lasso

#ridge 
predict_ridge <- predict(ridge, new=X, s="lambda.min")
#predict_ridge
mse_train_ridge[simulation] <- mean((training_data$Y - predict_ridge)^2)
#mse_train_ridge



### prediction for test data
predict_test_ols <- predict(ols, newdata=test_data)
mse_test_ols[simulation] <- mean((test_data$Y_test - predict_test_ols)^2)
#mse_test_ols

#lasso
predict_test_lasso <- predict(lasso, newx=x_test, s="lambda.min")
#predict_test_lasso
mse_test_lasso[simulation] <- mean((test_data$Y_test - predict_test_lasso)^2)
#mse_test_lasso

#ridge 
predict_test_ridge <- predict(ridge, new=x_test, s="lambda.min")
#predict_test_ridge
mse_test_ridge[simulation] <- mean((test_data$Y - predict_test_ridge)^2)
#mse_train_ridge
}

simulation_results <- data.frame( mse_test_lasso = mean(mse_test_lasso),
                                  mse_test_ols = mean(mse_test_ols),
                                  mse_test_ridge = mean(mse_test_ridge),
                                  mse_train_lasso = mean(mse_train_lasso),
                                  mse_train_ols = mean(mse_train_ols),
                                  mse_train_ridge = mean(mse_train_ridge)
                                  )
#simulation_results
