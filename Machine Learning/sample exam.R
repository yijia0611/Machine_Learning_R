# prepare the packages 
library(tidyverse)
library(lmtest)
library(sandwich)
library(haven)
library(stargazer)
library(caret)
library(grid)
library(glmnet)
library(rpart)
library(rpart.plot)
library (gbm)
library(randomForest)


install.packages("wooldridge") 
data<-data(gpa2, package="wooldridge")
# Define quadratic, cubic and fourth order terms of tothrs, colgpa, hsize, hsrank and hsperc
gpa2$tothr2<-gpa2$tothrs^2
gpa2$tothr3<-gpa2$tothrs^3
gpa2$tothr4<-gpa2$tothrs^4
gpa2$colgpa2<-gpa2$colgpa^2
gpa2$colgpa3<-gpa2$colgpa^3
gpa2$colgpa4<-gpa2$colgpa^4
gpa2$hsize2<-gpa2$hsize^2
gpa2$hsize3<-gpa2$hsize^3
gpa2$hsize4<-gpa2$hsize^4
gpa2$hsrank2<-gpa2$hsrank^2
gpa2$hsrank3<-gpa2$hsrank^3
gpa2$hsrank4<-gpa2$hsrank^4
gpa2$hsperc2<-gpa2$hsperc^2
gpa2$hsperc3<-gpa2$hsperc^3
gpa2$hsperc4<-gpa2$hsperc^4
gpa2[1,]
#define interaction of white and female
gpa2$whitefemale<-gpa2$female*gpa2$white

#b estimate LASSO models with various lambda of a sufficiently wide range.
X <- model.matrix(sat ~.,gpa2)[, -1] # predictor variables excluding wage, total credits and ids  in training set
y <- gpa2$sat # response variable in training set
set.seed(123) # for reproducibility
lambda_seq <- 10^seq(-5, 1, by = 0.5) # sequence of lambda values
lasso_models<-glmnet(X, y, alpha = 1)

#c use cv to determine best lambda, and write down the best lambda
cv_results <- cv.glmnet(X, y, alpha = 1, lambda = lambda_seq,nfolds = 5)
best_lambda <- cv_results$lambda.min
best_lambda
#d 
# way 1 
cvMSE <- cv_results$cvm[which.min(cv_results$lambda)]

#way 2
#make the prediction at test set 
lasso.pred <- predict(cv_results, s = best_lambda,newx = X)
mean((lasso.pred - y)^2)

##f
lasso_coef <- coef(cv_results, s = best_lambda)
lasso_vars <- rownames(lasso_coef)[-1]
lasso_coef
lasso_vars


#II
data<-data(gpa2, package="wooldridge")
# create a test set (20% of observations)
smp_size <- floor(0.2 * nrow(gpa2))
#create ids
test_ids <- sample(seq_len(nrow(gpa2)), size = smp_size)
gpa2$test <- 0
gpa2$test[test_ids] <- 1
#test set
gpa2_test <- gpa2 %>% filter(test == 1)
#Working data set
gpa2_train <- gpa2 %>% filter(test == 0)

# b fit a regression tree with pruning on the training sample
ctrl <- rpart.control(cp = 0, 
                      minsplit =600, 
                      maxdepth = 10, 
                      xval = 5)
#fit the tree into  training and test set combined (twoyear_work)
tree <- rpart(sat ~.,data=gpa2_train,method = "anova", control = ctrl)
# let the cost complexity to decide the optimal size of the tree
result<-printcp(tree)
#0.0028355
pruned.tree <- prune(tree, cp = 0.0028355)
#plot the tree
rpart.plot(pruned.tree)
##explain the tuning parameter 
#the xerror column represents the cross-validation error. 
#The row with the lowest cross-validation error represents the optimal value for the cost complexity
#parameter.
#cost complexity parameter controls the trade-off between the complexity of the tree 
#and its fit to the training data.
#minsplit is set to 1000 to ensure that each terminal node has at least 1000 observations,
#maxdepth is set to 10 to limit the depth of the tree, and xval is set to 5 to perform 5-fold cross-validation

#d  fit a random forest on the traning set 
#Random forest with T = 25 trees,
#Number of trees: 25,larger number of trees can improve the accuracy of the model
#but also increases the computational cost and may lead to overfitting
#No. of variables tried at each split: 12, which is the variable amount
#The no. of variables helps to reduce the correlation between trees and improve the diversity of the ensemble. T
#One of the advantages of random forest is that it can provide measures of variable importance, 
#which can help with feature selection and interpretation of the model. 
rf <- randomForest(sat ~., data = gpa2_train,mtry=ncol(gpa2_train), ntree = 25)


#fit a boosted trees with various tuning parameter
#Learning rate (or shrinkage rate): This parameter controls the 
#contribution of each tree to the final model. A small learning rate can 
#improve the stability and prevent overfitting,
#Number of trees: This parameter determines the number of trees that 
#will be added to the model.
library (gbm)
set.seed (1)
gbm<- gbm( sat ~., data = gpa2_train,
            distribution = "gaussian", n.trees = 100, shrinkage = 0.1,             
            interaction.depth = 3, bag.fraction = 0.5, train.fraction = 0.5,  
            n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
            verbose = FALSE, n.cores = 1)


#f Calculate  the mean squared error of the pruned tree and the rf on test set
predrf <- predict(rf, newdata = gpa2_test)
mse_rf <- mean((gpa2_test$sat - predrf)^2)
mse_rf # 13287.58

predtree<-predict(pruned.tree,newdata = gpa2_test)
mse_tree<-mean((gpa2_test$sat - predtree)^2)
mse_tree #14435.74
plot(predtree)
rpart.plot(predtree)

# prunned tree , random forest and boosted trees 
#A pruned tree is a single decision tree that has been simplified by removing some of the branches, 
#with the goal of reducing overfitting and improving generalization to new data.
#a random forest reduces variance by aggregating the predictions of multiple trees,
# which helps to smooth out the noise in the data and produce more accurate predictions. 
#Additionally, a random forest can reduce bias by using multiple decision trees,
# which helps to capture more complex relationships in the data.
#powerful ensemble learning technique that combines multiple weak decision trees to create a strong model



