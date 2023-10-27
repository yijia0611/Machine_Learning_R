install.packages("wooldridge")
data<-data(twoyear, package='wooldridge')

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

#create variables including wage and univsq
twoyear$wage<-exp(twoyear$lwage)
twoyear$univsq<-(twoyear$univ)^2
# seperate the data into holdout set, training set and test set
# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(twoyear))
#create ids
holdout_ids <- sample(seq_len(nrow(twoyear)), size = smp_size)
twoyear$holdout <- 0
twoyear$holdout[holdout_ids] <- 1
#Hold-out set Set
twoyear_holdout <- twoyear %>% filter(holdout == 1)
#Working data set
twoyear_work <- twoyear %>% filter(holdout == 0)
# create test and train samples (75% of observations in working sample)
trainsize <- floor(0.75 * nrow(twoyear_work))
train_ids <- sample(seq_len(nrow(twoyear_work)), size = trainsize)
twoyear_work$train <- 0
twoyear_work$train[train_ids] <- 1
# Create train and test sample variables
twoyear_train <- twoyear_work %>% filter(train == 1)
twoyear_test <- twoyear_work %>% filter(train == 0)
mean(twoyear$wage)
#a

#fit wage in the 3 regressions
model1 <- as.formula(wage ~ female+phsrank+BA+AA+black+hispanic)
model2 <- as.formula(wage ~ female+phsrank+BA+AA+black+hispanic+exper+jc+univ+smcity*female+medcity)
model3 <- as.formula(wage ~ female+phsrank+BA+AA+black+hispanic+exper+jc+univ+smcity+medcity+univsq)

reg1 <- lm(model1, data=twoyear_train)
reg2 <- lm(model2, data=twoyear_train)
reg3 <-lm(model3,  data=twoyear_train)
test1 <- lm(model1, data=twoyear_test)
test2 <- lm(model2, data=twoyear_test)
test3 <-lm(model3,  data=twoyear_test)

#combine them into one table 
stargazer(reg1,reg2,reg3,align=TRUE,type = "text", no.space = TRUE,title = "Table X", out = "path/fit.tex")
stargazer(reg1,reg2,reg3,align=TRUE,type = "text")



#b
model4 <- as.formula(lwage ~ female+phsrank+BA+AA+black+hispanic)
model5 <- as.formula(lwage ~ female+phsrank+BA+AA+black+hispanic+exper+jc+univ+smcity*female+medcity)
model6 <- as.formula(lwage ~ female+phsrank+BA+AA+black+hispanic+exper+jc+univ+smcity+medcity+univsq)

reg4 <- lm(model4, data=twoyear_train)
reg5 <- lm(model5, data=twoyear_train)
reg6 <-lm(model6,  data=twoyear_train)
test4 <- lm(model4, data=twoyear_test)
test5 <- lm(model5, data=twoyear_test)
test6 <-lm(model6,  data=twoyear_test)

stargazer(reg4,reg5,reg6,align=TRUE,type = "text", no.space = TRUE,title = "Table X", out = "path/fit.tex")

models <- c("reg1", "reg2","reg3","test1", "test2","test3", "reg4", "reg5","reg6","test4", "test5","test6")
for ( i in 1:length(models)){
  AIC[i] <- AIC(get(models[i]))
  BIC[i] <- BIC(get(models[i]))
  RMSE[i] <-sqrt( mean( ( predict(get(models[i])) - get(models[i])$model$wage )^2 ) )
  RMSE[i] <-sqrt( mean( ( predict(get(models[i])) - get(models[i])$model$lwage )^2 ) )
  RSquared[i] <-summary(get(models[i]))$r.squared
  k[i] <- get(models[i])$rank -1
}

#c align them to be in one table 
eval <- data.frame(models, AIC,BIC,RMSE, RSquared,regr, k)
stargazer(eval,type="text", summary = F, digits=2, float = F, no.space = T)


#########d

X_train <- model.matrix(lwage ~ female+phsrank+BA+AA+black+hispanic+exper+jc+univ+smcity+medcity+univsq,twoyear_train)[, -1] # predictor variables excluding wage, total credits and ids  in training set
y_train <- twoyear_train$lwage # response variable in training set
X_test <- model.matrix(lwage ~ female+phsrank+BA+AA+black+hispanic+exper+jc+univ+smcity+medcity+univsq,twoyear_test)[, -1] # predictor variables excluding wage, total credits and ids in test set
y_test <- twoyear_test$lwage # response variable in test set

set.seed(123) # for reproducibility
lambda_seq <- 10^seq(-5, 1, by = 0.5) # sequence of lambda values
lasso_models<-glmnet(X_train, y_train, alpha = 1)


####Predict the outcome variable on the test set using different lambda values
y_pred <- matrix(0, nrow = length(y_test), ncol = length(lambda_seq)) 
for (i in 1:length(lambda_seq)) {
  y_pred[, i] <- predict(lasso_models, newx = X_test, s = lambda_seq[i])
}

#predict the wage by the best lambda value in test set
bestlam<-min(lasso_models$lambda)
lwage_predict<-predict(lasso_models, s = bestlam,
                       newx = X_test)
lwage_predict
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


### e select the best LASSO by cross-validation
# Cross-validate to select best lambda value
cv_results <- cv.glmnet(X_train, y_train, alpha = 1, lambda = lambda_seq,nfolds = 5)
best_lambda <- cv_results$lambda.min # best lambda value

# Fit best LASSO model on entire training set
best_model <- glmnet(X_train, y_train, alpha = 1, lambda = best_lambda)
# Extract coefficients and variables from the best LASSO model
lasso_coef <- coef(cv_results, s = best_lambda)
lasso_vars <- rownames(lasso_coef)[-1]
lasso_coef
lasso_vars




#####f tuning parameter 
#the xerror column represents the cross-validation error. 
#The row with the lowest cross-validation error represents the optimal value for the cost complexity
#parameter.
#cost complexity parameter controls the trade-off between the complexity of the tree 
#and its fit to the training data.
#minsplit is set to 1000 to ensure that each terminal node has at least 1000 observations,
#maxdepth is set to 10 to limit the depth of the tree, and xval is set to 5 to perform 5-fold cross-validation
ctrl <- rpart.control(cp = 0, 
                      minsplit =1000, 
                      maxdepth = 10, 
                      xval = 5)
#fit the tree into  training and test set combined (twoyear_work)
tree <- rpart(lwage ~ .-wage,data=twoyear_work,method = "anova", control = ctrl)
# let the cost complexity to decide the optimal size of the tree
printcp(tree)
#when xerror is min ,cp = 0.0047506, to prun the tree : 
pruned.tree <- prune(tree, cp = 0.0047506)
#plot the tree
rpart.plot(pruned.tree)




####g fit 5 random forests with different tuning parameters and compare them in training and test set
library(randomForest)
##baseline : lwage ~.-wage-totcoll-id
# Random forest with default tuning parameters
rf1 <- randomForest(lwage ~.-wage-totcoll-id, data = twoyear_train)

# Random forest with m = the number of variables
rf2 <- randomForest(lwage ~.-wage-totcoll-id, data = twoyear_train, mtry = ncol(twoyear_train))

# Random forest with m = the square root of the number of variables
rf3 <- randomForest(lwage ~.-wage-totcoll-id, data = twoyear_train, mtry = sqrt(ncol(twoyear_train)))

# Random forest with T = 25 trees
rf4 <- randomForest(lwage ~.-wage-totcoll-id, data = twoyear_train, ntree = 25)

# Random forest with T = 500 trees
rf5 <- randomForest(lwage ~.-wage-totcoll-id, data = twoyear_train, ntree = 500)
rf1
rf2
rf3
rf4
rf5
stargazer(rf1,rf2,rf3,rf4,rf5,align=TRUE,type = "text")

# Make predictions on the test data for each random forest
pred1 <- predict(rf1, newdata = twoyear_test)
pred2 <- predict(rf2, newdata = twoyear_test)
pred3 <- predict(rf3, newdata = twoyear_test)
pred4 <- predict(rf4, newdata = twoyear_test)
pred5 <- predict(rf5, newdata = twoyear_test)

# Calculate the mean squared error for each random forest
mse1 <- mean((twoyear_test$lwage - pred1)^2)
mse2 <- mean((twoyear_test$lwage - pred2)^2)
mse3 <- mean((twoyear_test$lwage - pred3)^2)
mse4 <- mean((twoyear_test$lwage - pred4)^2)
mse5 <- mean((twoyear_test$lwage - pred5)^2)

# Print the mean squared error for each random forest
cat("Random forest with default tuning parameters:", mse1, "\n")
cat("Random forest with m = the number of variables:", mse2, "\n")
cat("Random forest with m = the square root of the number of variables:", mse3, "\n")
cat("Random forest with T = 25 trees:", mse4, "\n")
cat("Random forest with T = 500 trees:", mse5, "\n")
# the one with smallest MSE is the best
#plot the importance of the variables 
var.imp <- importance(rf3)
varImpPlot(rf3)


#####h Fit 5 boosted trees with various tuning parameter 
#Set up the parameters and create a list of models with different tuning parameters
#baseline : lwage ~.-wage-totcoll-id
library (gbm)
set.seed (1)
gbm1 <- gbm(lwage ~.-wage-totcoll-id, data = twoyear_train, 
            distribution = "gaussian", n.trees = 100, shrinkage = 0.1,             
            interaction.depth = 3, bag.fraction = 0.5, train.fraction = 0.5,  
            n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
            verbose = FALSE, n.cores = 1)  

gbm2 <- gbm(lwage ~.-wage-totcoll-id, data = twoyear_train, 
            distribution = "gaussian", n.trees = 200, shrinkage = 0.01,             
            interaction.depth = 5, bag.fraction = 0.5, train.fraction = 0.5,  
            n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
            verbose = FALSE, n.cores = 1)

gbm3 <- gbm(lwage ~.-wage-totcoll-id, data = twoyear_train, 
            distribution = "gaussian", n.trees = 300, shrinkage = 0.001,             
            interaction.depth = 10, bag.fraction = 0.5, train.fraction = 0.5,  
            n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
            verbose = FALSE, n.cores = 1)

gbm4 <- gbm(lwage ~.-wage-totcoll-id, data = twoyear_train, 
            distribution = "gaussian", n.trees = 800, shrinkage = 0.0001,             
            interaction.depth = 10, bag.fraction = 0.5, train.fraction = 0.5,  
            n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
            verbose = FALSE, n.cores = 1)

gbm5 <- gbm(lwage ~.-wage-totcoll-id, data = twoyear_train, 
            distribution = "gaussian", n.trees = 150, shrinkage = 0.1,             
            interaction.depth = 6, bag.fraction = 0.5, train.fraction = 0.5,  
            n.minobsinnode = 10, cv.folds = 5, keep.data = TRUE, 
            verbose = FALSE, n.cores = 1)  
?gbm
gbm1
gbm2
gbm3
gbm4
gbm5

# Make predictions on the test data for each random forest
predct1 <- predict(gbm1, newdata = twoyear_test)
predct2 <- predict(gbm2, newdata = twoyear_test)
predct3 <- predict(gbm3, newdata = twoyear_test)
predct4 <- predict(gbm4, newdata = twoyear_test)
predct5 <- predict(gbm5, newdata = twoyear_test)

MSE1 <- mean((twoyear_test$lwage - predct1)^2)
MSE2 <- mean((twoyear_test$lwage - predct2)^2)
MSE3 <- mean((twoyear_test$lwage - predct3)^2)
MSE4 <- mean((twoyear_test$lwage - predct4)^2)
MSE5 <- mean((twoyear_test$lwage - predct5)^2)


# Print the mean squared error for each random forest
cat("Random forest with default tuning parameters:", MSE1, "\n")
cat("Random forest with m = the number of variables:", MSE2, "\n")
cat("Random forest with m = the square root of the number of variables:", MSE3, "\n")
cat("Random forest with T = 25 trees:", MSE4, "\n")
cat("Random forest with T = 500 trees:", MSE5, "\n")


########i
#calculate the MSE for best model in b
predict_b<-predict(reg6,newdata=twoyear_holdout)
mse_b<- mean((predict_b - twoyear_holdout$lwage)^2)

#calculate the MSE for best model in e
predict_e<-predict.glmnet(best_model,newx=twoyear_holdout)
mse_e<- mean((predict_e - twoyear_holdout$lwage)^2)

#calculate the MSE for best model in f
prediction_tree <- predict(pruned.tree, newdata = twoyear_holdout,na.action = na.pass)
mse_tree <- mean((prediction_tree - twoyear_holdout$lwage)^2)

#calculate the MSE for best model in g
predict_rf<-predict(rf3,newdata = twoyear_holdout)
mse_g <- mean((predict_rf - twoyear_holdout$lwage)^2)

#calculate the MSE for best model in h
predict_gmb<-predict(gbm5, twoyear_holdout, 500, type = "response", single.tree = FALSE)
mse_h <- mean((predict_gmb - twoyear_holdout$lwage)^2)