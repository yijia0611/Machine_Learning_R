################################################################################################
# Prepared for the textbook:
# Data Analysis for Business, Economics, and Policy
# by Gabor BEKES and  Gabor KEZDI 
# Cambridge University Press 2021
# 
# License: Free to share, modify and use for educational purposes. Not to be used for business purposes.
#
###############################################################################################x

# Modified by Peter Elek (February 2022)

# CHAPTER 13
# Used cars

# ONLY FROM THE CLEANED DATA SET

# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())
install.packages(c('tidyverse', 'lmtest', 'sandwich','haven','stargazer','caret','grid'))
# import libraries
library(tidyverse)
library(lmtest)
library(sandwich)
library(haven)
library(stargazer)
library(caret)
library(grid)

###############################################################################
# FROM CLEAN DATA

# directory should be changed
data <- read.csv(paste0("C:/ElekP/Oktatas/Machine_Learning/Data/usedcars_work.csv"), stringsAsFactors = FALSE)
data <- read.csv(paste0("usedcars_work.csv"),stringsAsFactors = FALSE)
data <- read.csv(paste0(""),stringsAsFactors = FALSE)

# Frequency tables
# area
data %>%
  group_by(area) %>%
  dplyr::summarize(frequency=n(), mean=mean(price))

# focus only on Chicago
data <- data %>%
  filter(area=="chicago")

# group by price using condition and also get mean price
data %>%
  group_by(condition) %>%
  dplyr::summarize(frequency=n(), mean=mean(price))

# drive
data %>%
  group_by(drive) %>%
  dplyr::summarize(frequency=n(), mean=mean(price))

# dealer
data %>%
  group_by(dealer) %>%
  dplyr::summarize(frequency=n(), mean=mean(price))


# data summary
data %>%
  dplyr::select(age, odometer, LE, XLE, SE, cond_likenew, cond_excellent, cond_good, cylind6) %>%
    summary()

Hmisc::describe(data$age)

# Histograms not in textbook
hist(data$price)
# price
F13_h_price_R <- ggplot(data=data, aes(x=price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 1000, boundary=0,
                 size = 0.25, alpha = 0.8,  show.legend=F, na.rm=TRUE) +
  coord_cartesian(xlim = c(0, 20000)) +
  labs(x = "Price (US dollars)",y = "Percent")+
  expand_limits(x = 0.01, y = 0.01) +
  scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(expand = c(0.01,0.01),breaks = seq(0,20000, 2500))
F13_h_price_R

# lnprice 
F13_h_lnprice_R <- ggplot(data=data, aes(x=lnprice)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.2, boundary=0,
  size = 0.25, alpha = 0.8,  show.legend=F, na.rm=TRUE) +
  coord_cartesian(xlim = c(6, 10)) +
  labs(x = "log price (log US dollars)",y = "Percent")+
  expand_limits(x = 0.01, y = 0.01) +
  scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(expand = c(0.01,0.01),breaks = seq(6,10,1))
F13_h_lnprice_R

###############################################################################
# REGRESSION ANALYSIS


# lowess (locally weighted scatterplot smoothing)
Ch13_p_age_lowess_R <- ggplot(data = data, aes(x=age, y=price)) +
  geom_point( size = 1,  shape = 16, alpha = 0.8, show.legend=F, na.rm = TRUE) + 
  geom_smooth(method="loess", se=F, size=1, span=0.9) +
  labs(x = "Age (years)",y = "Price (US dollars)") +
  expand_limits(x = 0.01, y = 0.01) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,20000), breaks = seq(0,20000, 5000)) +
  scale_x_continuous(expand = c(0.01,0.01),limits = c(0,30), breaks = seq(0,30, 5))
Ch13_p_age_lowess_R

###################################
# Linear regressions

# Model 1: Linear regression on age
model1 <- as.formula(price ~ age + agesq)
# Models 2-5: Multiple linear regressions
# note: condition - missing will be baseline for regs
model2 <- as.formula(price ~ age + agesq + odometer)
model3 <- as.formula(price ~ age + agesq + odometer + odometersq + LE + cond_excellent + cond_good + dealer)
model4 <- as.formula(price ~ age + agesq + odometer + odometersq + LE + XLE + SE + cond_likenew +
                       cond_excellent + cond_good + cylind6 + dealer)
model5 <- as.formula(price ~ age + agesq + agecu + odometer + odometersq + LE*age + XLE*age + SE*age +
                       cond_likenew*age + cond_excellent*age + cond_good*age + cylind6*age + odometer*age + dealer*age)


reg1 <- lm(model1, data=data)
reg2 <- lm(model2, data=data)
reg3 <- lm(model3, data=data)
reg4 <- lm(model4, data=data)
reg5 <- lm(model5, data=data)

#residuals of reg 1 
#a residual is the difference between the actual value and the value predicted by the model (y-ŷ) for any given point.
summary(reg1)
hist(reg1$residuals)

summary(reg5)
# evaluation of the models

models <- c("reg1", "reg2","reg3", "reg4", "reg5")
AIC <- c()
BIC <- c()
RMSE <- c()
RSquared <- c()
regr <- c()
k <- c()
#by calculating BIC and AIC, we could know which model is better 
#creating loop for calculating BIC and AIC
for ( i in 1:length(models)){
  AIC[i] <- AIC(get(models[i]))
  BIC[i] <- BIC(get(models[i]))
  RMSE[i] <-sqrt( mean( ( predict(get(models[i])) - get(models[i])$model$price )^2 ) )
  RSquared[i] <-summary(get(models[i]))$r.squared
  regr[[i]] <- coeftest(get(models[i]), vcov = sandwich)
  k[i] <- get(models[i])$rank -1
}

############################################################
# Linear regression evaluation

# Model 1
stargazer(regr[[1]], type="text", digits=2, float = F, no.space = T)

# Lowess vs. quadratic (reg1) regression
Ch13_p_age_quad_vs_lowess_R <- ggplot(data = data, aes(x=age)) +
  geom_smooth(aes(y=price), method="loess", se=F, size=1) +
  geom_line(aes(y=predict(reg1)), size=1,lty=2) +
  labs(x = "Age (years)",y = "Price (US dollars)") +
  scale_x_continuous(limits = c(0,30), breaks = seq(0,30, 5)) +
  scale_y_continuous(limits = c(0,20000), breaks = seq(0,20000, 5000)) +
  theme(legend.position = c(0.7,0.7),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.box.background = element_rect(color = "white"))
Ch13_p_age_quad_vs_lowess_R

# All models
eval <- data.frame(models, k, RSquared, RMSE, BIC)
eval <- eval %>%
  mutate(models = paste0("(",gsub("reg","",models),")")) %>%
  rename(Model = models, "R-squared" = RSquared, "Training RMSE" = RMSE, "N predictors" = k)
stargazer(eval, type="text", summary = F, digits=2, float = F, no.space = T)

# models 1-4 only, 5 too large, it can be used for alaining together.
stargazer(reg1, reg2, reg3, reg4 , align = T,   digits=2, dep.var.caption = "Dep. var: price", keep.stat = c("rsq","n"),
            type="text", title = "Cars - regression", no.space = T)

#################################################################
# Cross-validation

# set number of folds
k <- 4

set.seed(13505)
cv1 <- train(model1, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(13505)
cv2 <- train(model2, data, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(13505)
cv3 <- train(model3, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(13505)
cv4 <- train(model4, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(13505)
cv5 <- train(model5, data, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")

# calculate average rmse
cv <- c("cv1", "cv2", "cv3", "cv4", "cv5")
rmse_cv <- c()

for(i in 1:length(cv)){
  rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                       get(cv[i])$resample[[1]][2]^2 +
                       get(cv[i])$resample[[1]][3]^2 +
                       get(cv[i])$resample[[1]][4]^2)/4)
}

# summarize results
cv_mat <- data.frame(rbind(cv1$resample[4], "Average"),
           rbind(cv1$resample[1], rmse_cv[1]),
           rbind(cv2$resample[1], rmse_cv[2]),
           rbind(cv3$resample[1], rmse_cv[3]),
           rbind(cv4$resample[1], rmse_cv[4]),
           rbind(cv5$resample[1], rmse_cv[5])
           )

colnames(cv_mat)<-c("Resample","Model1", "Model2", "Model3", "Model4", "Model5")
cv_mat

stargazer(cv_mat, summary = F, digits=0, float=F, type="text")

# NB THIS IS SLIGHTLY DIFFERENT TO ONE IN TEXTBOOK (DIFFERENT SEED)

###############################################################################
# Prediction

data <- data %>% dplyr::select(age, agesq, odometer, odometersq, SE, LE, XLE, cond_likenew,
                        cond_excellent, cond_good, dealer,price, cylind6)

# Add new observation(add it mamually)
new <- list(age=10, agesq=10^2,odometer=12,odometersq=12^2,SE=0,XLE=0, LE=1, 
            cond_likenew=0,cond_excellent=1,cond_good=0, 
            dealer=0, cylind6=0, price=NA)

# Predict price with all predictors (Model1)
reg1 <- lm(model1, data=data)
# Standard error of residuals
p1 <- predict(reg1, data)
resid_p1 <- p1-data$price
summary(resid_p1)

# predict value for newly added obs
pred1_new <- predict(reg1, newdata = new,se.fit = TRUE, interval = "prediction")
p1<- pred1_new$fit

# Predict price with all predictors (Model3)
reg3 <- lm(model3, data=data)
# Standard error of residuals
p2 <- predict(reg3, data)
resid_p2 <- p2-data$price
summary(resid_p2)
# predict value for newly added obs
pred2_new <- predict(reg3, newdata = new,se.fit = TRUE, interval = "prediction")
p2<- pred2_new$fit
pred2_new 

# Result summary
sum1 <- cbind(t(p1), t(p2))
colnames(sum1) <- c('Model1', 'Model3')
rownames(sum1) <- c('Predicted', 'PI_low (95%)', 'PI_high (95%)')
sum1

stargazer(sum1, summary = F, digits=0, float=F, type="text")

# summary of predictions and PI 80% version
# predict value for newly added obs
pred1_new80 <- predict(reg1, newdata = new, se.fit=TRUE, interval = "prediction", level=0.8)
p180<- pred1_new80$fit
pred2_new80 <- predict(reg3, newdata = new,se.fit = TRUE, interval = "prediction", level=0.8)
p280<- pred2_new80$fit

# Result summary
sum2 <- cbind(t(p180), t(p280))
colnames(sum2) <- c('Model1', 'Model3')
rownames(sum2) <- c('Predicted', 'PI_low (80%)', 'PI_high (80%)')
sum2

stargazer(sum2, summary = F, digits=0, float=F, type="text")

# in book sum1 and sum2 are combined in Table 3
