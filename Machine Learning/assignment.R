# import libraries
library(tidyverse)
library(lmtest)
library(sandwich)
library(haven)
library(stargazer)
library(caret)
library(grid)
library(wooldridge)

install.packages("wooldridge")
install.packages("tidyverse")

#importing dataset
data <- data(twoyear, package='wooldridge')
View(twoyear)

summary(twoyear)

# creating wage from lwage variable
twoyear$wage<-exp(twoyear$lwage)

# creating new polynomial and interaction variables
twoyear$expersq<- twoyear$exper * twoyear$exper
twoyear$BA_female<- twoyear$BA * twoyear$female
twoyear$BA_black<- twoyear$BA * twoyear$black
twoyear$BA_hispanic <- twoyear$BA * twoyear$hispanic
twoyear$jc_female<- twoyear$jc * twoyear$female
twoyear$univ_female<- twoyear$univ * twoyear$female

-----------------------------------------------------
# PART A)
# creating a random 60% sample for the training set, 20% for the test set and 20% for the hold-out set.
smp_size <- floor(0.2 * nrow(twoyear))

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table

holdout_ids <- sample(seq_len(nrow(twoyear)), size = smp_size)
twoyear$holdout <- 0
twoyear$holdout[holdout_ids] <- 1

#Hold-out set 
twoyear_holdout <- twoyear %>% filter(holdout == 1)

#Working data set
twoyear_work <- twoyear %>% filter(holdout == 0)

# divide into training and tets samples
train_size <- floor(0.75 * nrow(twoyear_work))
train_ids <- sample(seq_len(nrow(twoyear_work)), size = train_size)
twoyear_work$train <- 0
twoyear_work$train[train_ids] <- 1

# Create train and test sample variables
twoyear_train <- twoyear_work %>% filter(train == 1)
twoyear_test <- twoyear_work %>% filter(train == 0)

# regression 1
reg1 <- lm(wage ~ BA +  female + black + hispanic , data=twoyear_train)
reg1
# regression 2
reg2 <- lm(wage ~ BA  + female + black + hispanic + exper + expersq, data=twoyear_train)

# regression 3
reg3 <- lm(wage ~ BA + female + black + hispanic + BA_female + BA_black + BA_hispanic + exper + expersq  + medcity + submed + lgcity + sublg + vlgcity + subvlg, data=twoyear_train)

library(stargazer)
stargazer(list(reg1,reg2,reg3),type="text",single.row=TRUE)

# AIC and BIC
cbind(AIC(reg1),AIC(reg2),AIC(reg3))
cbind(BIC(reg1),BIC(reg2),BIC(reg3))

# regression 1
reg1test <- lm(wage ~ BA +  female + black + hispanic , data=twoyear_test)

# regression 2
reg2test <- lm(wage ~ BA  + female + black + hispanic + exper + expersq, data=twoyear_test)

# regression 3
reg3test <- lm(wage ~ BA + female + black + hispanic + BA_female + BA_black + BA_hispanic  + exper + expersq  + medcity + submed + lgcity + sublg + vlgcity + subvlg , data=twoyear_test)

# evaluation of the models
models1 <- c("reg1", "reg2","reg3","reg1test", "reg2test","reg3test")
AIC <- c()
BIC <- c()
RMSE <- c()
RSquared <- c()
regr <- c()
k <- c()

for ( i in 1:length(models1)){
  AIC[i] <- AIC(get(models1[i]))
  BIC[i] <- BIC(get(models1[i]))
  RMSE[i] <-sqrt( mean(( predict(get(models1[i])) - get(models1[i])$model$wage )^2 ) )
  RSquared[i] <-summary(get(models1[i]))$r.squared
  k[i] <- get(models1[i])$rank -1
}

#c align them to be in one table 
eval <- data.frame(models1, AIC,BIC,RMSE, RSquared, k)
stargazer(eval,type="text", summary = F, digits=2, float = F, no.space = T)
---------------------------------------------------------------------

  # PART B
# regression 4
reg4 <- lm(lwage ~ jc + univ, data=twoyear_train)

# regression 5
reg5 <- lm(lwage ~ jc + univ + female + jc_female + univ_female, data=twoyear_train)

# regression 6
reg6 <- lm(lwage ~ jc + univ + female + jc_female + univ_female + exper + expersq, data=twoyear_train)

# regression 4 test
reg4test<- lm(lwage ~ jc + univ, data=twoyear_test)

# regression 5 test
reg5test <- lm(lwage ~ jc + univ + female + jc_female + univ_female, data=twoyear_test)

# regression 6 test
reg6test <- lm(lwage ~ jc + univ + female + jc_female + univ_female + exper + expersq, data=twoyear_test)

library(stargazer)
stargazer(list(reg4,reg5,reg6),type="text",single.row=TRUE)

# evaluation of the models
models2 <- c("reg4", "reg5","reg6","reg4test", "reg5test","reg6test")
AIC1 <- c()
BIC1 <- c()
RMSE1 <- c()
RSquared1 <- c()
regr1 <- c()
k1 <- c()

for ( i in 1:length(models2)){
  AIC1[i] <- AIC(get(models2[i]))
  BIC1[i] <- BIC(get(models2[i]))
  RMSE1[i] <-sqrt( mean(( predict(get(models2[i])) - get(models2[i])$model$lwage )^2 ) )
  RSquared1[i] <-summary(get(models2[i]))$r.squared
  k1[i] <- get(models2[i])$rank -1
}

#c align them to be in one table 
eval1 <- data.frame(models2, AIC1,BIC1,RMSE1, RSquared1, k1)
stargazer(eval1,type="text", summary = F, digits=2, float = F, no.space = T)


----------------------------------------------------------------------

# PART C
reg3testcomp <- lm(wage ~ BA + female + black + hispanic + BA_female + BA_black + BA_hispanic + exper + expersq  + medcity + submed + lgcity + sublg + vlgcity + subvlg , data=twoyear_test)
reg6testcomp <- lm(wage ~ jc + univ + female + jc_female + univ_female + exper + expersq, data=twoyear_test)

# evaluation of the models
models3 <- c("reg3testcomp","reg6testcomp")
AIC2 <- c()
BIC2 <- c()
RMSE2 <- c()
RSquared2 <- c()
regr2 <- c()
k2 <- c()

for ( i in 1:length(models3)){
  AIC2[i] <- AIC(get(models3[i]))
  BIC2[i] <- BIC(get(models3[i]))
  RMSE2[i] <-sqrt( mean(( predict(get(models3[i])) - get(models3[i])$model$wage )^2 ) )
  RSquared2[i] <-summary(get(models3[i]))$r.squared
  k2[i] <- get(models3[i])$rank -1
}

#c align them to be in one table 
eval2 <- data.frame(models3, AIC2,BIC2,RMSE2, RSquared2, k2)
stargazer(eval2,type="text", summary = F, digits=2, float = F, no.space = T)

---------------------------------------------
# PART D
  
x_train <- model.matrix(lwage ~ jc + univ + female + jc_female + univ_female + exper + expersq, twoyear_train)[, -1] # predictor variables excluding wage, total credits and ids  in training set
y_train <- twoyear_train$lwage 
x_test <- model.matrix(lwage ~ jc + univ + female + jc_female + univ_female + exper + expersq,twoyear_test)[, -1] # predictor variables excluding wage, total credits and ids in test set
y_test <- twoyear_test$lwage 

set.seed(123) 
lambda_seq <- 10^seq(-5, 1, by = 0.5) 
lasso_models <-glmnet(x_train, y_train, alpha = 1)

# Predict the outcome variable on the test set 
y_pred <- matrix(0, nrow = length(y_test), ncol = length(lambda_seq)) 
for (i in 1:length(lambda_seq)) {
  y_pred[, i] <- predict(lasso_models, newx = x_test, s = lambda_seq[i])
}

# Predicting the log(wage) by the best lambda value from the test set
bestlambda <-min(lasso_models$lambda)
lwage_predict<-predict(lasso_models, s = bestlambda,
                       newx = x_test)
summary(twoyear$lwage)
summary(lwage_predict)


# Computing test set MSE for each lambda value
test_mse <- sapply(lambda_seq, function(lambda) {
  y_pred <- predict(lasso_models, newx = x_test, s = lambda)
  mean((y_pred - y_test)^2)
})

# Plotting test set MSE as a function of lambda
library(ggplot2)
df <- data.frame(lambda = log10(lambda_seq), mse = test_mse)
ggplot(df, aes(x = lambda, y = mse)) +
  geom_line() +
  labs(x = "log(lambda)", y = "Test set MSE", title = "Test set MSE as a function of lambda") 
  
  
--------------
# PART E
# Fit LASSO model with cross-validation
# Plot mean cross-validated error as a function of lambda
plot(lasso_mod)
lasso_mod

# Get the lambda value that minimizes the mean cross-validated error
lambda_min <- lasso_mod$lambda.min
lambda_min

# Get the coefficients of the final LASSO model
lasso_coef <- coef(lasso_mod, s = lambda_min)
lasso_coef

y_pred <- predict(lasso, newx=x_test)
mse <- mean((y_test - y_pred)^2)
cat("Test set MSE:", mse, "\n")


---------------
# PART F
install.packages("tree")
library(tree)

install.packages("rpart.plot")
library(rpart.plot)

tree <- rpart(lwage ~ . -wage, data = twoyear_train, method = "class")
cv_results <- prune(tree, cp = seq(0, 0.1, 0.01))
cv_errors <- cv_results$criterion[2,] # extract cross-validation error rates
best_cp <- cv_results$cptable[which.min(cv_errors),"CP"] # get optimal CP
sum(best_cp)

ctrl <- rpart.control(cp = 0, 
                      minsplit =1000, 
                      maxdepth = 10, 
                      xval = 5)
tree <- rpart(lwage ~ .-wage,data=twoyear_work,method = "anova", control = ctrl)

printcp(tree)

pruned.tree <- prune(tree, cp = 0)

rpart.plot(pruned.tree)

------------------------------


