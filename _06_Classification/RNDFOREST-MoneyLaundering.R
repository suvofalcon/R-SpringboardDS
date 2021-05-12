# ******************************************************************************************************
# Random Forest - Detecting Money Laundering
#
# A big bank wants to detect money laundering threat in credit card transactions
#
# Data Sets:
# 1. Transaction Data of its credit card users
# 2. Small subset of data with money laundering alert (indicator is 1, if an alert has been issued, else 0)
#
# Aims to create a process to generate alert on new transactions
# ******************************************************************************************************

rm(list = ls()) # clear all runtime variables and functions in environment
options(scipen = 999) # No automatic round offs of numbers

# Use of external libraries
library(randomForest)
library(Boruta)

# load the dataset
data <- read.csv("Z:\\CodeMagic\\Data Files\\Jigsaw\\RF.csv")
# Check the data load
head(data)
str(data)
# change the dependent variable to factor
data$Alert <- as.factor(data$Alert)
# check for missing values
sum(is.na(data)) # No missing values

# We will use tuneRF function to determine the best number of variables at each split for minimum OOB error
set.seed(1)
mtry <- tuneRF(data[,-1],data[,1], mtryStart = 1, stepFactor = 2, ntreeTry = 500, improve = 0.01)
best.m <- mtry[mtry[,2] == min(mtry[,2]),1]

# Run the Random Forest
set.seed(1)
rf <- randomForest(Alert~.,data=data,mtry = best.m, ntree = 500, importance = TRUE,do.trace = TRUE)
rf
# Lets find out which variables contributes most to the model accuracy
varImpPlot(rf)
# we see sum of transaction contributes most to the model accuracy and ML_Scenario_3 contributes least
# to find out the relative importance of each of the variables
rf$importance
plot(rf)
##################################################################################
# Feature selection using Boruta
set.seed(1)
Boruta.fit <- Boruta(Alert~.,data = data,doTrace = 2, ntree = 500)
Boruta.fit
getSelectedAttributes(Boruta.fit)
attStats(Boruta.fit)
m <- data.frame(attStats(Boruta.fit))
plot(Boruta.fit)
##################################################################################
# Feature selection using varSelRF
library(varSelRF)
set.seed(1)
rf.vsl <- varSelRF(data[,2:7],data$Alert,vars.drop.frac = 0.2)
rf.vsl #this shows only 4 variables which are important, we may run RF model again using these 4 variables

##################################################################################
# Prediction and ROC Curve
smp_size <- floor(0.75 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind,]
test <- data[-train_ind,]
train$Alert <- as.factor(train$Alert)
test$Alert <- as.factor(test$Alert)
# Run Random forest on train dataset
set.seed(1)
fit.rf <- randomForest(Alert~.,data=train,importance = TRUE, do.trace = TRUE)
fit.rf
library(ROCR)
library(caret)
# make predictions
test.forest <- predict(fit.rf,type = "prob",newdata = test)
forestpred <- prediction(test.forest[,2],test$Alert)
forestperf <- performance(forestpred, "tpr","fpr")
plot(forestperf)
auc.tmp <- performance(forestpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc
