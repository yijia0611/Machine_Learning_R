#########################################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com 
#
# License: Free to share, modify and use for educational purposes. 
# 	Not to be used for commercial purposes.

# Chapter 15
# CH15A Predicting used car value with regression trees
# using the used-cars dataset
# version 0.91 2021-01-19
#########################################################################################

# from the cleaned data
# modified and simplified by Peter Elek
install.packages("caret")
install.packages("ggplot2")
library(caret)
# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Descriptive statistics and regressions
library(caret)
library(tidyverse)
library(skimr)
library(ggthemes)
library(gridExtra)
library(lattice)
library(glmnet)
library(rpart)
library(rattle)
library(rpart.plot)
library(xtable)
library(Hmisc)
library(modelsummary)

# directory should be changed
data <- read.csv(paste0("C:/ElekP/Oktatas/Machine_Learning/Data/usedcars_work.csv"), stringsAsFactors = FALSE)
library(readr)
usedcars_work <- read_csv("usedcars_work.csv")
View(usedcars_work)
data<-usedcars_work
#################################
# Create test and train samples #
#################################
# now all stuff runs on training vs test (holdout), alternative: 4-fold CV


# create test and train samples (70% of observations in train sample)
smp_size <- floor(0.7 * nrow(data))
set.seed(20180122)

train_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$train <- 0
data$train[train_ids] <- 1
# Create train and test sample variables
data_train <- data %>% filter(train == 1)
data_test <- data %>% filter(train == 0)

#####################
# Regression tree (rpart package)

summary(data_train$price)
summary(data_train$age)

# AGE IS THE ONLY  PREDICTOR VARIABLE
model1 <- formula(price ~ age)

# Single split
# (make sure it's a single split by setting "maxdepth" to 1)

cart1 <- train(
  model1, data = data_train, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth=1))

summary(cart1)
pred_cart1 <- predict(cart1, data_test)
rmse_cart1 <- sqrt(mean((pred_cart1 - data_test$price)^2))

# Tree graph
rpart.plot(cart1$finalModel, tweak=1.2, digits=-1, extra=1)

# Scatterplot with step function
plot_helper <- seq(min(data_train$age), max(data_train$age))
plot_helper_df <-data.frame(age=plot_helper)
plot_helper_df$xend <- c(plot_helper+1)
plot_helper_df$yend <- predict(cart1, plot_helper_df)
pred_cart1t <- predict(cart1, data_train)

ggplot(data = data_train, aes(x = age, y=price)) +
  geom_point() +
  geom_segment(data = plot_helper_df,  aes(x = age, y=yend, xend=xend, yend=yend), size=1, na.rm=TRUE) +
  scale_y_continuous(expand=c(0.01,0.01), limits=c(0, 20000), breaks=seq(0, 20000, by=2500)) +
  scale_x_continuous(expand=c(0.01,0.01),limits=c(0, 25), breaks=seq(0, 25, by=5)) +
  labs(x = "Age (years)", y = "Price (US dollars)")  

###########
# Splits at two levels
# (make sure it stops by setting "maxdepth" to 2)

cart2 <- train(
  model1, data = data_train, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth=2))

summary(cart2)

tab_cart2 <- data.frame(
  "Category" = c("Age 1-4", "Age 5-7","Age 8-12","Age 13 or more"),
  "Count" = c(summary(cart2)$frame$n[7], summary(cart2)$frame$n[6], summary(cart2)$frame$n[4], summary(cart2)$frame$n[3]),
  "Average_price" = c(summary(cart2)$frame$yval[7], summary(cart2)$frame$yval[6], summary(cart2)$frame$yval[4], summary(cart2)$frame$yval[3])
  )

tab_cart2


pred_cart2 <- predict(cart2, data_test)
rmse_cart2 <- sqrt(mean((pred_cart2 - data_test$price)^2))

# Tree graph
rpart.plot(cart2$finalModel, tweak=1.2, digits=-1, extra=1)

# Scatterplot with step function
plot_helper_df$yend <- predict(cart2, plot_helper_df)
pred_cart1t <- predict(cart1, data_train)

ggplot(data = data_train, aes(x=age , y=price)) +
  geom_point() +
  geom_segment(data = plot_helper_df, aes(x = age, y=yend, xend=xend, yend=yend), size=1, na.rm=TRUE) +
  scale_y_continuous(expand=c(0.01,0.01), limits=c(0, 20000), breaks=seq(0, 20000, by=2500)) +
  scale_x_continuous(expand=c(0.01,0.01),limits=c(0, 25), breaks=seq(0, 25, by=5)) +
  labs(x = "Age (years)", y = "Price (US dollars)")

############
# Splits go on according to rpart defaults
# NB: typo in book, CART is with cp=0.01 not cp=0.001
cart3 <- train(
  model1, data = data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.01))

summary(cart3)
pred_cart3 <- predict(cart3, data_test)
rmse_cart3 <- sqrt(mean((pred_cart3 - data_test$price)^2))


# Tree graph
rpart.plot(cart3$finalModel, tweak=1.2, digits=-1, extra=1)

# Scatterplot with step function - train data
plot_helper_df$yend <- predict(cart3, plot_helper_df)
pred_cart3t <- predict(cart3, data_train)

ggplot(data = data_train, aes(x=age , y=price)) +
  geom_point() +
  geom_segment(data = plot_helper_df, aes(x = age, y=yend, xend=xend, yend=yend), size=1, na.rm=TRUE) +
  scale_y_continuous(expand=c(0.01,0.01), limits=c(0, 20000), breaks=seq(0, 20000, by=2500)) +
  scale_x_continuous(expand=c(0.01,0.01),limits=c(0, 25), breaks=seq(0, 25, by=5)) +
  labs(x = "Age (years)", y = "Price (US dollars)")

#####################
# Age only, Linear regression

linreg1 <- lm(model1 , data=data_train)
linreg1
pred_linreg1 <- predict(linreg1, data_test)
rmse_linreg1 <- sqrt(mean((pred_linreg1 - data_test$price)^2))

# Scatterplot with predicted values
linreg1 <- lm(model1 , data=data_train)
pred_linreg1t<- predict(linreg1, data_train)

ggplot(data = data_train) +
  geom_point(aes(x = age, y = price), size = 1,  shape = 16, alpha = 0.7, show.legend=FALSE, na.rm = TRUE) +
  geom_line(aes(x=age,y=pred_linreg1t), size=0.7) +
  scale_y_continuous(expand=c(0.01,0.01), limits=c(0, 20000), breaks=seq(0, 20000, by=2500)) +
  scale_x_continuous(expand=c(0.01,0.01), limits=c(0, 25), breaks=seq(0, 25, by=5)) +
  labs(x = "Age (years)", y = "Price (US dollars)")

#####################
# Age only, Lowess  regression

lowess1 <- loess(model1, data=data_train)
# no prediction with loess on test
pred_lowess1 <- predict(lowess1, data_test)
rmse_lowess1 <- sqrt(mean((pred_lowess1 - data_test$price)^2))

# Scatterplot with predicted values
lowess1 <- loess(model1, data=data_train)
pred_lowess1t <- predict(lowess1, data_train)

ggplot(data = data_train, aes(x=age , y=price)) +
  geom_point(size=1, colour="black" ) +
  labs(x = "Age", y = "Price") +
  coord_cartesian(xlim=c(0, 25), ylim=c(0, 20000)) +
  geom_smooth(method="loess", colour="darkblue", se=F, size=1.5)


ggplot(data = data_train, aes(x = age, y = price)) +
  geom_point() +
  geom_smooth(method="loess")+
  scale_y_continuous(expand=c(0.01,0.01), limits=c(0, 20000), breaks=seq(0, 20000, by=2500)) +
  scale_x_continuous(expand=c(0.01,0.01),limits=c(0, 25), breaks=seq(0, 25, by=5)) +
  labs(x = "Age (years)", y = "Price (US dollars)")

########################################################
# MULTIPLE PREDICTOR VARIABLES

#####################
# Linear regression with multiple variables
model2 <- formula(price ~ age + odometer + LE + XLE + SE + cond_excellent + cond_good + cylind6 + dealer)
linreg2 <- lm(model2 , data=data_train)
linreg2
pred_linreg2 <- predict(linreg2, data_test, na.action = na.pass)
rmse_linreg2 <- sqrt(mean((pred_linreg2 - data_test$price)^2))
rmse_linreg2

# add squared for age, odometer
model3 <- formula(price ~ age + agesq+ odometer+odometersq +LE + XLE + SE + cond_excellent + cond_good + cylind6 + dealer)
linreg3 <- lm(model3 , data=data_train)
linreg3
pred_linreg3 <- predict(linreg3, data_test, na.action = na.pass)
rmse_linreg3 <- sqrt(mean((pred_linreg3 - data_test$price)^2))
rmse_linreg3

#############
# Tree

# Splits at four levels, for illustrative purposes
# (make sure it stops by setting "maxdepth" to 3)
cart4 <- train(
  model2, data=data_train, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth=4),
  na.action = na.pass)

# alternative to show the use of cp.
# same outcome
cart4 <- train(
  model2, data=data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.01),
  control = rpart.control(minsplit = 20),
  na.action = na.pass)



summary(cart4)
pred_cart4 <- predict(cart4, data_test, na.action = na.pass)
rmse_cart4 <- sqrt(mean((pred_cart4 - data_test$price)^2))


# Tree graph
rpart.plot(cart4$finalModel, tweak=1.2, digits=-1, extra=1)

cart5 <- train(
  model2, data=data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.002),
  control = rpart.control(minsplit = 20),
  na.action = na.pass)

print(cart5)

summary(cart5)
pred_cart5 <- predict(cart5, data_test, na.action = na.pass)
rmse_cart5 <- sqrt(mean((pred_cart5 - data_test$price)^2))

# Tree graph
rpart.plot(cart5$finalModel, tweak=1.2, digits=-1, extra=1)

############################
# prune the tree
############################

# build very large tree

cart6 <- train(
  model2, data=data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.0001),
  control = rpart.control(minsplit = 4),
  na.action = na.pass)

# Tree graph
rpart.plot(cart6$finalModel, tweak=1.2, digits=-1, extra=1)

summary(cart6)
pred_cart6 <- predict(cart6, data_test, na.action = na.pass)
rmse_cart6 <- sqrt(mean((pred_cart6 - data_test$price)^2))
rmse_cart6

# take the last model (large tree) and prune (cut back)
pfit <-prune(cart6$finalModel, cp=0.005 )
summary(pfit)

# getting rmse
pred_cart7 <- predict(pfit, data_test, na.action = na.pass)
rmse_cart7 <- sqrt(mean((pred_cart7 - data_test$price)^2))
rmse_cart7

printcp(pfit)

# Tree graph
rpart.plot(pfit, digits=-1, extra=1, tweak=1)

########x summary performance table

tab_rmse <- data.frame(
  "Model" = c("CART1", "CART2","CART3","CART4", "CART5","CART6","CART7", "OLS multivar", "OLS extended"),
  "Describe" = c("2 term. nodes", "4 term. nodes","5 term. nodes","cp = 0.01","cp = 0.002","cp = 0.0001","pruned", "multi-var", "w/ squared vars"),
  "RMSE" = c(rmse_cart1, rmse_cart2, rmse_cart3, rmse_cart4,rmse_cart5,rmse_cart6,rmse_cart7, rmse_linreg2, rmse_linreg3)
)
#it seems that pruned one is better because of the least RMSE
tab_rmse

#############
# Varimp

cart4_var_imp <- varImp(cart4)$importance

cart4_var_imp/sum(cart4_var_imp)

# nicer table

# plot
cart4_var_imp_df <- data.frame(varname = rownames(cart4_var_imp),imp = cart4_var_imp$Overall) %>%
mutate(varname = gsub("cond_", "Condition:", varname) ) %>%
arrange(desc(imp)) %>%
mutate(imp_percentage = imp/sum(imp))

ggplot(cart4_var_imp_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(size=2) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), size=1.5) +
  ylab("Importance") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1))

############################################################x

## a note for varimp 

# https://topepo.github.io/caret/variable-importance.html
# Recursive Partitioning: The reduction in the loss function (e.g. mean squared error) attributed to each variable at each split is tabulated and the sum is returned. Also, since there may be candidate variables that are important but are not used in a split, the top competing variables are also tabulated at each split. This can be turned off using the maxcompete argument in rpart.control.
# To avoid this, we can rerun cart4 with a new control fn to ensure matching 
  
cart4_rev <- train(
  model2, data=data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.01),
  control = rpart.control(minsplit = 20, maxcompete = FALSE),
  na.action = na.pass)

  cart4_var_imp_rev <- varImp(cart4_rev)$importance
  cart4_var_imp_rev_df <-
    data.frame(varname = rownames(cart4_var_imp_rev),imp = cart4_var_imp_rev$Overall) %>%
    mutate(varname = gsub("cond_", "Condition:", varname) ) %>%
    arrange(desc(imp)) %>%
    mutate(imp_percentage = imp/sum(imp))
  
  cart4_var_imp_plot_rev <- ggplot(cart4_var_imp_rev_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
    geom_point(size=2) +
    geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), size=1.5) +
    ylab("Importance") +
    xlab("Variable Name") +
    coord_flip() +
    scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1))
  cart4_var_imp_plot_rev
