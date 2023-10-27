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
#d  fit a random forest on the traning set 
 
rf <- randomForest(sat ~., data = gpa2_train,mtry=ncol(gpa2_train), ntree = 25)

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
mse_rf

predtree<-predict(pruned.tree,newdata = gpa2_test)
mse_tree<-mean((gpa2_test$sat - predtree)^2)
mse_tree
plot(predtree)
rpart.plot(predtree)
