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

# CHAPTER 14
# CH014A Predicting used car value: log prices
# Using the used-cars dataset
# version 0.9 2020-08-31
#########################################################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
install.packages("tidyverse")
install.packages("lmtest")
install.packages("sandwich")
install.packages("cowplot")
install.packages("haven")
install.packages("stargazer")
install.packages("caret)")
library(tidyverse)
library(lmtest)
library(sandwich)
library(cowplot)
library(haven)
library(stargazer)
library(caret)

# directory should be changed
data <- read.csv(paste0("usedcars_work.csv"), stringsAsFactors = FALSE)

# focus only on Chicago
data <- data %>%
  filter(area=="chicago")

###################################
# Linear regressions in logs now

# Model 1: Linear regression on age
model1log <- as.formula(lnprice ~ age +agesq )
# Models 2-5: no quads
model2log <- as.formula(lnprice ~ age  + agesq + odometer + odometersq)
model3log <- as.formula(lnprice ~ age  + agesq + odometer + odometersq + LE + cond_excellent + cond_good + dealer)
model4log <- as.formula(lnprice ~ age  + agesq + odometer + odometersq + LE + XLE + SE + cond_likenew +
                       cond_excellent + cond_good + cylind6 + dealer)
model5log <- as.formula(lnprice ~ age +  agesq + odometer + odometersq + LE*age + XLE*age + SE*age +
                       cond_likenew*age + cond_excellent*age + cond_good*age + cylind6*age + odometer*age + dealer*age)


reg1log <- lm(model1log, data=data)
reg2log <- lm(model2log, data=data)
reg3log <- lm(model3log, data=data)
reg4log <- lm(model4log, data=data)
reg5log <- lm(model5log, data=data)

# evaluation of the models

models <- c("reg1log", "reg2log","reg3log", "reg4log", "reg5log")
AIC <- c()
BIC <- c()
RMSE <- c()
RSquared <- c()
regr <- c()
k <- c()

for ( i in 1:length(models)){
  AIC[i] <- AIC(get(models[i]))
  BIC[i] <- BIC(get(models[i]))
  RMSE[i] <- sqrt( mean( (predict(get(models[i])) - get(models[i])$model$lnprice )^2 ) ) 
  RSquared[i] <-summary(get(models[i]))$r.squared
  regr[[i]] <- coeftest(get(models[i]), vcov = sandwich)
  k[i] <- get(models[i])$rank -1
}

############################################################
# Linear regression evaluation


# All models 
eval <- data.frame(models, k, RSquared, RMSE, BIC)
eval <- eval %>%
  mutate(models = paste0("(",gsub("reg","",models),")")) %>%
  rename(Model = models, "R-squared" = RSquared, "Training RMSE" = RMSE, "N predictors" = k)
stargazer(eval, summary = F, digits=2, float = F, no.space = T, type="text")


#################################################################
# Cross-validation

# set number of folds (4 because of small sample)
k <- 4

# need to set the same seed again and again, on the 2 times of 0,35 valiadation happened in price after controlling , same with standard error 
#
set.seed(13505)
cv1log <- train(model1log, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(13505)
cv2log <- train(model2log, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(13505)
cv3log <- train(model3log, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(13505)
cv4log <- train(model4log, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(13505)
cv5log <- train(model5log, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")

# calculate average rmse
cv <- c("cv1log", "cv2log", "cv3log", "cv4log", "cv5log")
rmse_cv <- c()

for(i in 1:length(cv)){
  rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                        get(cv[i])$resample[[1]][2]^2 +
                        get(cv[i])$resample[[1]][3]^2 +
                        get(cv[i])$resample[[1]][4]^2)/4)
}


# summarize results
cv_matlog <- data.frame(rbind(cv1log$resample[4], "Average"),
                        rbind(cv1log$resample[1], rmse_cv[1]),
                        rbind(cv2log$resample[1], rmse_cv[2]),
                        rbind(cv3log$resample[1], rmse_cv[3]),
                        rbind(cv4log$resample[1], rmse_cv[4]),
                        rbind(cv5log$resample[1], rmse_cv[5])
)

colnames(cv_matlog)<-c("Resample","Model1log", "Model2log", "Model3log", "Model4log", "Model5log")
cv_matlog

#stargazer(cv_matlog, summary = F, digits=3, float=F, out=paste(output,"Ch14_cvmatlog_R.tex",sep=""))
#stargazer(cv_matlog, summary = F, digits=3, float=F, type="text",  out=paste(output,"Ch14_cvmatlog_R.txt",sep=""))




################################################################################

# PREDICTION

# repeat what we did in ch 13, now in logs

# Prediction
#data <- data %>% dplyr::select(age, agesq, odometer, odometersq, SE, LE, XLE, cond_likenew,cond_excellent, cond_good, dealer,price, cylind6)

# Add new observation
new <- list(age=10, agesq=10^2,odometer=12,odometersq=12^2,SE=0,XLE=0, LE=1, 
            cond_likenew=0,cond_excellent=1,cond_good=0, 
            dealer=0, cylind6=0, price=NA)

# Predict lnprice with Model 3 from ch13
# Predict price with all predictors (Model3)
reg3 <- lm(lnprice ~ age  + agesq + odometer +  odometersq + LE + cond_excellent + cond_good + dealer, data=data)
summary(reg3)
# prediction
data$lnp2 <- predict(reg3, data)
rmse3 <- sqrt(mean((data$lnp2-data$lnprice)^2))

# prediction for new observation
predln_new <- predict(reg3, newdata = new,se.fit = TRUE, interval = "prediction")
predln_new80 <- predict(reg3, newdata = new,se.fit = TRUE, interval = "prediction", level=0.80)
predln_new80
lnp2_new <- predln_new$fit[[1]]

# predictions in levels , this 
data$lnplev <- exp(data$lnp2)*exp((rmse3^2)/2)
lnp2_new_lev <- exp(lnp2_new)*exp((rmse3^2)/2)
summary(data$lnp2-data$lnprice)
#log is ok because the mean vaéue is 0,but the other one is not ok , since it is exponential , it would  overvaleu the price , becuase it is convex.
#in this case , we should use log as variable

# prediction intervals (log and level)
lnp2_PIlow <- predln_new$fit[2]
lnp2_PIhigh <- predln_new$fit[3]
lnplev_PIlow <- exp(lnp2_PIlow)*exp(rmse3^2/2)
lnplev_PIhigh <- exp(lnp2_PIhigh)*exp(rmse3^2/2)

#80%
lnp2_PIlow80 <- predln_new80$fit[2]
lnp2_PIhigh80 <- predln_new80$fit[3]
lnplev_PIlow80 <- exp(lnp2_PIlow80)*exp(rmse3^2/2)
lnplev_PIhigh80 <- exp(lnp2_PIhigh80)*exp(rmse3^2/2)

# summary of predictions and PI
sum <- matrix( c( lnp2_new, lnp2_PIlow ,lnp2_PIhigh,
                  lnp2_new_lev, lnplev_PIlow, lnplev_PIhigh) , nrow = 3 ,ncol = 2)

colnames(sum) <- c('Model in logs', 'Recalculated to level')
rownames(sum) <- c('Predicted', 'PI_low', 'PI_high')
sum
stargazer(sum, type = "text", float=F, digits=2)

# summary of predictions and PI 80% version
sum <- matrix( c( lnp2_new, lnp2_PIlow80 ,lnp2_PIhigh80,
                  lnp2_new_lev, lnplev_PIlow80, lnplev_PIhigh80) , nrow = 3 ,ncol = 2)

colnames(sum) <- c('Model in logs', 'Recalculated to level')
rownames(sum) <- c('Predicted', 'PI_low 80%', 'PI_high 80%')


sum
stargazer(sum, type = "text", digits=2)

