# Importing the dataset
as.data = read.csv('C:/Users/Sam Koshy/Desktop/MSA - Fall/502/Fall 3/Machine Learning/HW/allstate_train.csv')
as.test=read.csv('C:/Users/Sam Koshy/Desktop/MSA - Fall/502/Fall 3/Machine Learning/HW/allstate_test.csv')

# Splitting the dataset into the Training set and Validation set
install.packages('caTools')
library(caTools)
set.seed(051015)
split = sample.split(as.data, SplitRatio = 0.7)
as.train = subset(as.data, split == TRUE)
as.val = subset(as.data, split == FALSE)

install.packages("Matrix")
library(Matrix)
sparse_train = sparse.model.matrix(loss ~ . -loss, data=as.train)
sparse_val = sparse.model.matrix(loss ~ . -loss, data=as.val)
histogram(as.train$loss, breaks=10000)
loss=log(as.train$loss)
histogram(loss, breaks=10000)

# Fitting XGBoost to the Training set
install.packages('xgboost')
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
Sevierity = xgboost(data = sparse_train, label = loss, booster= "gbtree", objective="reg:linear", eta = 0.03, gamma = 0, nround=200, max_depth = 8, subsample = 1, colsample_bytree = 1, verbose = 0, eval_metric = "mae")
ptrain = predict(Sevierity, sparse_train)
error.train=MAE(exp(ptrain),as.train$loss)
pvalid = predict(Sevierity, sparse_val)
error.val=MAE(exp(pvalid),as.val$loss)
