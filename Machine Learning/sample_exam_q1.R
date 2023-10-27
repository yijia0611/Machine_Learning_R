install.packages("wooldridge")
data(gpa2, package="wooldridge")
View(gpa2)

# Part a) Define quadratic, cubic and fourth order terms of tothrs, colgpa, hsize, hsrank and hsperc e.g. 
gpa2$tothrs2 <- gpa2$tothrs^2
gpa2$colgpa2 <- gpa2$colgpa^2
gpa2$hsize2 <- gpa2$hsize^2
gpa2$hsrank2 <- gpa2$hsrank^2
gpa2$hsperc2 <- gpa2$hsperc^2

# Cubic terms
gpa2$tothrs3 <- gpa2$tothrs^3
gpa2$colgpa3 <- gpa2$colgpa^3
gpa2$hsize3 <- gpa2$hsize^3
gpa2$hsrank3 <- gpa2$hsrank^3
gpa2$hsperc3 <- gpa2$hsperc^3

# Fourth order terms
gpa2$tothrs4 <- gpa2$tothrs^4
gpa2$colgpa4 <- gpa2$colgpa^4
gpa2$hsize4 <- gpa2$hsize^4
gpa2$hsrank4 <- gpa2$hsrank^4
gpa2$hsperc4 <- gpa2$hsperc^4

# Interaction of white and female
gpa2$whitefemale <- gpa2$female * gpa2$white
View(gpa2)
----------------------------------------------------------
# Part b) Use all of the above variables (polynomials and interactions) to estimate 
# LASSO models with various lambda of a sufficiently wide range.
install.packages("glmnet")
install.packages("caret")
install.packages("prodlim")
install.packages("tidyverse")

library(glmnet)
library(caret)
library(prodlim)
library(tidyverse)

# Set seed for reproducibility
set.seed(123)

# Split data into training and testing sets
trainsize <- floor(0.8 * nrow(gpa2))
train_ids <- sample(seq_len(nrow(gpa2)), size = trainsize)
gpa2$train <- 0
gpa2$train[train_ids] <- 1
# Create train and test sample variables
train <- gpa2 %>% filter(train == 1)
test <- gpa2 %>% filter(train == 0)

x <- model.matrix(sat ~ . -train, data = train)[, -1]
y <- train$sat

lambdaSeq <- 10^seq(-5, 1, by = 0.5)
lambdaSeq

# Fit LASSO model with cross-validation
lassoFit <- cv.glmnet(x, y, alpha = 1, lambda = lambdaSeq, nfolds = 5)
lassoFit
plot(lassoFit)

plot(lassoFit)
--------------------------------------------------------
#c. Use cross-validation to determine the optimal lambda. Write down the optimal lambda.
optLambda <- lassoFit$lambda.min
optLambda #0.01
cat("Optimal lambda =", optLambda, "\n")

--------------------------------
#For the optimal lambda, write down the expression that is minimized in LASSO estimation

  
---------------------------------
# Calculate the cross-validated mean squared error for the optimal lambda.

mse <- lassoFit$cvm[lassoFit$lambda == optLambda]
cat("Cross-validated mean squared error =", mse, "\n") 

# CV MSE = 13118.39

---------------------------------
# Which variables are used in the optimal model?

lassoModel <- glmnet(x, y, alpha = 1, lambda = optLambda)
coef(lassoModel) 












# Q2
# a. Define a training and a test sample (e.g. 80% and 20%).

set.seed(123)

# Split data into training and testing sets
trainsize <- floor(0.8 * nrow(gpa2))
train_ids <- sample(seq_len(nrow(gpa2)), size = trainsize)
gpa2$train <- 0
gpa2$train[train_ids] <- 1
# Create train and test sample variables
train <- gpa2 %>% filter(train == 1)
test <- gpa2 %>% filter(train == 0)


-----------------------
# part b:  Fit a regression tree with pruning on the training sample.
install.packages("rpart")
library(rpart)
library(rpart.plot)
train1 <- sample(nrow(gpa2), 0.8*nrow(gpa2))
tree <- rpart(sat ~., data = train, method = "anova")
pruned_tree <- prune(tree, cp = 0.01)
rpart.plot(pruned_tree)

View(train1)

---------------------
# part c
--------------------- 
  
# part d
install.packages("randomForest")
library(randomForest)
set.seed(123)
rf <- randomForest(sat ~., data = train, ntree = 500, mtry = sqrt(ncol(gpa2)-1))
rf

-----------------------
# part f
# compare the MSE of the pruned tree and the random forest on the test set

pred_tree <- predict(pruned_tree, newdata = test, type = "vector")
mse_tree <- mean((test$sat - pred_tree)^2)
mse_tree #15170

pred_rf <- predict(rf, newdata = test)
mse_rf <- mean((test$sat - pred_rf)^2)
mse_rf #13722
