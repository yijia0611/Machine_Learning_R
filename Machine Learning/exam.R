data<-data(htv,package="wooldridge")
htv<-subset(htv,select=-c(lwage))

#seperate into train and test
smp_size <- floor(0.3 * nrow(htv))
#create ids
test_ids <- sample(seq_len(nrow(htv)), size = smp_size)
htv$test <- 0
htv$test[test_ids] <- 1
#test set
htv_test <- htv %>% filter(test == 1)
#Working data set
htv_train <- htv %>% filter(test == 0)

#######a
#abil and educ
htv$abil2<-(htv$abil)^2
htv$abil3<-(htv$abil)^3
htv$abil4<-(htv$abil)^4
htv$educ2<-(htv$educ)^2
htv$educ3<-(htv$educ)^4
htv$educ3<-(htv$educ)^4

X_train <- model.matrix(wage ~. ,htv_train)[, -1]
y_train <- htv_train$wage # response variable in training set
X_test <- model.matrix(wage ~. ,htv_test)[, -1]
y_test <- htv_test$wage # response variable in test set

set.seed(123) # for reproducibility
lambda_seq <- 10^seq(-10, 1, by = 0.5) # sequence of lambda values
lasso_models<-glmnet(X_train, y_train, alpha = 1)


####Predict the wage variable, y_pred, on the test set using different lambda values
y_pred <- matrix(0, nrow = length(y_test), ncol = length(lambda_seq)) 
for (i in 1:length(lambda_seq)) {
  y_pred[, i] <- predict(lasso_models, newx = X_test, s = lambda_seq[i])
}
# Compute test set MSE for each lambda value
test_mse <- sapply(lambda_seq, function(lambda) {
  y_pred <- predict(lasso_models, newx = X_test, s = lambda)
  mean((y_pred - y_test)^2)
})
# Plot test set MSE as a function of lambda
library(ggplot2)
df <- data.frame(lambda = log10(lambda_seq), mse = test_mse)
ggplot(df, aes(x = lambda, y = mse)) +
  geom_line() +
  labs(x = "log(lambda)", y = "Test set MSE", title = "Test set MSE vs. Lambda")

########b
#use cv to determine best lambda, and write down the best lambda
cv_results <- cv.glmnet(X_train, y_train, alpha = 1, lambda = lambda_seq,nfolds = 4)
best_lambda <- cv_results$lambda.min
best_lambda # 0.1

lasso_coef <- coef(cv_results, s = best_lambda)
lasso_vars <- rownames(lasso_coef)[-1]
lasso_coef
lasso_vars

###d

best_model <-cv.glmnet(X_train, y_train, alpha = 1, lambda = lambda_seq,nfolds = 4)

pred <- predict(best_model, newx =X_test )
rmse <- sqrt(mean((pred - y_test  )^2))# 8.966293

sd(htv$wage)

##### e linear regression 
### renew the data
data<-data(htv,package="wooldridge")
htv<-subset(htv,select=-c(lwage))

model1 <- as.formula(wage~.)
reg1 <- lm(model1, data=htv_train)
pred_wage<-predict(reg1,newdata = htv_test)
RMSE_OLS<-sqrt( mean( ( pred_wage - htv_test$wage )^2 ) )
RMSE_OLS

#####f
ctrl <- rpart.control(cp = 0, 
                      minsplit =400, 
                      maxdepth = 1, 
                      xval = 5)
#fit the tree into  training and test set combined 
tree <- rpart(wage~.,data=htv_train,method = "anova", control = ctrl)
# let the cost complexity to decide the optimal size of the tree
printcp(tree)
pruned.tree <- prune(tree, cp = 0.10084 )
#plot the tree
rpart.plot(pruned.tree)

#####g 
predtree<-predict(pruned.tree,newdata = htv_test)
mse_tree<-sqrt(mean((htv_test$wage - predtree)^2))
mse_tree

#### h 
# Random forest with default tuning parameters
rf1 <- randomForest(wage~., data = htv_train)

# Random forest with m = the number of variables
rf2 <- randomForest(wage~., data = htv_train, mtry = ncol(htv_train))

# Random forest with m = the square root of the number of variables
rf3 <- randomForest(wage~., data = htv_train, mtry = sqrt(ncol(htv_train)))

# Random forest with T = 10 trees
rf4 <- randomForest(wage~., data = htv_train, ntree = 25)

# Random forest with T = 50 trees
rf5 <- randomForest(wage~., data = htv_train, ntree = 50)

rf6 <- randomForest(wage~., data = htv_train, ntree = 250)

rf7 <- randomForest(wage~., data = htv_train, mtry = 0.5*ncol(htv_train))

rf8 <- randomForest(wage~., data = htv_train, ntree = 300)

rf9 <- randomForest(wage~., data = htv_train, mtry = 0.25*ncol(htv_train))

# Make predictions on the test data for each random forest
pred1 <- predict(rf1, newdata = htv_test)
pred2 <- predict(rf2, newdata = htv_test)
pred3 <- predict(rf3, newdata = htv_test)
pred4 <- predict(rf4, newdata = htv_test)
pred5 <- predict(rf5, newdata = htv_test)
pred6 <- predict(rf6, newdata = htv_test)
pred7 <- predict(rf7, newdata = htv_test)
pred8 <- predict(rf8, newdata = htv_test)
pred9 <- predict(rf9, newdata = htv_test)

# Calculate the mean squared error for each random forest
mse1 <- mean((htv_test$wage - pred1)^2)
mse2 <- mean((htv_test$wage - pred2)^2)
mse3 <- mean((htv_test$wage - pred3)^2)
mse4 <- mean((htv_test$wage - pred4)^2)
mse5 <- mean((htv_test$wage - pred5)^2)
mse6 <- mean((htv_test$wage - pred6)^2)
mse7 <- mean((htv_test$wage - pred7)^2)
mse8 <- mean((htv_test$wage - pred8)^2)
mse9 <- mean((htv_test$wage - pred9)^2)

mse1 
mse2
mse3 
mse4 
mse5 
mse6 
mse7 
mse8 
mse9 


#####i
library (gbm)
set.seed (1)
gbm1 <- gbm(wage~., data = htv_train, 
            distribution = "gaussian", n.trees = 100, shrinkage = 0.1,             
            interaction.depth = 3, bag.fraction = 0.5, train.fraction = 0.5,  
            n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
            verbose = FALSE, n.cores = 1)  

gbm2 <- gbm(wage~., data = htv_train, 
            distribution = "gaussian", n.trees = 100, shrinkage = 0.01,             
            interaction.depth = 5, bag.fraction = 0.5, train.fraction = 0.5,  
            n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
            verbose = FALSE, n.cores = 1)

gbm3 <- gbm(wage~., data = htv_train, 
            distribution = "gaussian", n.trees = 100, shrinkage = 0.001,             
            interaction.depth = 10, bag.fraction = 0.5, train.fraction = 0.5,  
            n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
            verbose = FALSE, n.cores = 1)

# Make predictions on the test data for each random forest
predct1 <- predict(gbm1, newdata = htv_test)
predct2 <- predict(gbm2, newdata = htv_test)
predct3 <- predict(gbm3, newdata = htv_test)


MSE1 <- mean((htv_test$wage - predct1)^2)
MSE2 <- mean((htv_test$wage - predct2)^2)
MSE3 <- mean((htv_test$wage - predct3)^2)
MSE1
MSE2
MSE3
