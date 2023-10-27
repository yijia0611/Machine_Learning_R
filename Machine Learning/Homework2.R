data<-data(happiness,package="wooldridge")
library(haven)
library(glmnet)
library(purrr)
library(margins)
library(skimr)
library(kableExtra)
library(Hmisc)
library(cowplot)
library(gmodels) 
library(lspline)
library(sandwich)
library(modelsummary)

library(rattle)
library(caret)
library(pROC)
library(ranger)
library(rpart)
library(partykit)
library(rpart.plot)
library(tidyverse)

#seperate into train and test
smp_size <- floor(0.2 * nrow(happiness))
#create ids
test_ids <- sample(seq_len(nrow(happiness)), size = smp_size)
happiness$test <- 0
happiness$test[test_ids] <- 1
#test set
happiness_test <- happiness %>% filter(test == 1)
#Working data set
happiness_train <- happiness %>% filter(test == 0)

#1.baseline model
model1<-glm(vhappy~prestige+divorce+babies+preteen+teens,data=happiness_train,family = "binomial")