# Chapter 8 Lab: Decision Trees 
# 8.3.2 - 8.3.4. (Regression trees, bagging, random forests, boosting)

# for explanations see Ch. 8 of ISLR textbook

rm(list=ls())
library(tree)
library(ISLR)

# 8.3.2
# Fitting Regression Trees

library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
tree.boston2=tree(medv~.,Boston,subset=train, control=tree.control(nobs=length(train),mincut=2, minsize=4))
summary(tree.boston2)
plot(tree.boston)
text(tree.boston,pretty=0)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)
sqrt(mean((yhat-boston.test)^2))

# 8.3.3
# Bagging and Random Forests

library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,importance=TRUE)
bag.boston
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
sqrt(mean((yhat.bag-boston.test)^2))
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

#RMSE can be calculated as :
sqrt(mean((yhat.bag-boston.test)^2))
set.seed(1)
#can change the mtry value , and then has different RMSE,by add tree number 
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE,ntree=500)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
sqrt(mean((yhat.rf-boston.test)^2))
importance(rf.boston)
varImpPlot(rf.boston)

# 8.3.4
# Boosting
#
library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
sqrt(mean((yhat.boost-boston.test)^2))
#should also include shrink parameter
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
sqrt(mean((yhat.boost-boston.test)^2))
#randome and boosting can be generated into casual analysis