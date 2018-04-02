#Red Ventures PD Modeling
library(caret)
library(caTools)
library(Matrix)
library(xgboost)
library(readr)
library(stringr)
library(car)
#reading data and replacing NA
data=read.csv("C:\\Users\\Sam Koshy\\Desktop\\Red Venture\\RVDS_technical_assessment\\train.csv", header=T)
data=replace(data, is.na(data), -99) 
test=read.csv("C:\\Users\\Sam Koshy\\Desktop\\Red Venture\\RVDS_technical_assessment\\test.csv", header=T)
test=replace(test, is.na(test), -99) 

#replacing yes and no
data$default_oct = ifelse(data$default_oct == "yes",1,0)
data$default_oct[data$default_oct=="yes"]=1
set.seed(12)

#splitting data into training and validation
split = sample.split(data, SplitRatio = 0.8)
train = subset(data, split == TRUE)
val = subset(data, split == FALSE)

#converting to sparse data matrix for XGB
s_train = model.matrix(default_oct ~ . -1, data=train, row.names=F, drop.unused.levels=F, sparse=T)
s_val = model.matrix(default_oct ~ . -1, data=val, row.names=F, drop.unused.levels=F, sparse=T)
tdef=as.matrix(train$default_oct)
vdef=as.matrix(val$default_oct)

#XGB
def_class = xgboost(data = s_train, label = tdef, booster= "gbtree", objective="binary:logistic", eta = .08, silent=0, lamda=.9,
                    gamma = 0, nround=2, max_depth = 5, subsample = .5, colsample_bytree = 1, verbose = 0, eval_metric = "error")
#predictions
ptrain = predict(def_class, s_train)
pvalid = predict(def_class, s_val)

#Logloss evaluation
LogLossBinary = function(actual, predicted, eps = 1e-15) {
   predicted = pmin(pmax(predicted, eps), 1-eps)
   - (sum(actual * log(predicted) + (1 - actual) * log(1 - predicted))) / length(actual)
}
LogLossBinary(tdef,ptrain)
LogLossBinary(vdef,pvalid)

#library(ROCR)
#library(InformationValue)
#vd<-as.factor(vdef)
#pv<-as.factor(pvalid)
#rc <- roc(vd, pvalid)
#roc$auc
#plot(1-rc$specificities,rc$sensitivities)
#optCutOff <- optimalCutoff(val$default_oct, pvalid)[1] 
#optCutOff

#test data prediction
test$default_oct=rep(1,5999)
s_test = model.matrix(default_oct ~ . -1,data=test, row.names=F, drop.unused.levels=F, sparse=T)
ptest= predict(def_class, s_test)
pred=as.data.frame(ptest)
dim(pred)
test$pr_y=pred
Final=cbind(test$customer_id,test$pr_y)
write.table(Final,file="C:/Users/Sam Koshy/Desktop/samkoshy_predictions.csv",col.names = c("Customer_ID", "pr_y"),sep = ",",row.names = F)
