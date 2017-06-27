library(rpart)
library(rpart.plot)

N = 100
X = matrix(seq(1:N)) #1 to the number of rows
e = matrix(rnorm(N)) #automatically sets standard dev
b = rep(1,1) #only adding 1 b, beta = 1
Y = X + e # X %*% b + e would be the same

df <- data.frame(X,Y)

trees <- rpart(Y ~ X, df)
rpart.plot(trees)

## y squared
X_sq = X*X
y_sq = I(X*X) + e
trees_sq <- rpart(y_sq ~ X, df)
rpart.plot(trees_sq)

## y cubed
X3 <- ifelse(df$X>50, 1, 0)
#X3 <- matrix(as.numeric(X>50), nrow = N, ncol = 1)
y_cube =  X3 * 2 + e
#df1 <- data.frame(X, y_cube)
trees_cube <- rpart(y_cube ~ X, df1)
rpart.plot(trees_cube)
y3_tree_pred = predict(trees_cube, df)
df$y3_tree_squareerr = predict(y_cube-df$y3_tree_pred, df)


mse_trees <- mean((y_cube - predict(trees_cube, df1))^2)
mse_trees
