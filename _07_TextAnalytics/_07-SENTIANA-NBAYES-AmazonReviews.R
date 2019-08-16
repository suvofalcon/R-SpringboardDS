# *************************************************************************************************************
# Text Mining - Implementation of Naive Bayes for Sentiment Classification
#
# A sample dataset containing reviews about Sony watches from Amazon
#
# This code has been developed using R version 3.2.X (hence few new function calls has been made)
# 
# **************************************************************************************************************
rm(list = ls()) # Clear the environment runtime variables
options(scipen = 999) # No automatic roundoffs 

# Use of external libraries
library(tm)
library(RTextTools)
library(e1071)
library(caret)
library(gmodels)

# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems
switch(Sys.info() [['sysname']],
       Windows = {analysis <- read.csv("//SUVOS-TIME-CAPS/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/review_sentiment.csv", header = TRUE, stringsAsFactors = FALSE)},
       Linux   = {analysis <- read.csv("//SUVOS-TIME-CAPS/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/review_sentiment.csv", header = TRUE, stringsAsFactors = FALSE)},
       Darwin  = {analysis <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/review_sentiment.csv", header = TRUE, stringsAsFactors = FALSE)})

# Split the data into training and test
set.seed(2000)
sampling <- sort(sample(nrow(analysis), nrow(analysis) * 0.7))
length(sampling)
head(analysis)

names(analysis)
train_tweets <- analysis[sampling, ]
test_tweets <- analysis[-sampling, ]

# if we want to see the distribution of the three classes in both the training and test dataset
prop.table(table(train_tweets$sentiment))
prop.table(table(test_tweets$sentiment))

# ********** Naive Bayes Algorithm **************************************************** #

# create individual matrices for training and testing datasets
mtrain <- as.matrix(train_tweets)
mtest <- as.matrix(test_tweets)

# building document term matrices for training and testing data
train_matrix <- create_matrix(mtrain[,2], language = "english", removeNumbers = TRUE, removePunctuation = TRUE,
                              removeStopwords = TRUE, stripWhitespace = TRUE, toLower = TRUE)
test_matrix <- create_matrix(mtest[,2], language = "english", removeNumbers = TRUE, removePunctuation = TRUE,
                             removeStopwords = TRUE, stripWhitespace = TRUE, toLower = TRUE)

# Input to a naive bayes algorithm has to be matrix with categorical values, and not numeric

# Convert the document term matrix to a 1/0 matrix 
# essentially all the frequencies in the document term matrix will be converted to presence
# I am not interested as to how many times the word has occured, rather whether it has occured(1) or not(0)
conversion <- function(A){
  A <- ifelse(A > 0, 1, 0)
  A <- factor(A, levels = c(0,1), labels = c("NO","YES"))
  return (A)
}
# Now lets apply this function to the training matrix
mat_train <- apply(train_matrix, MARGIN = 2, conversion)
# lets see first few rows
head(mat_train)

# lets also apply the same to the test matrix
mat_test <- apply(test_matrix, MARGIN = 2, conversion)
# lets see the first few rows
head(mat_test)

# Now lets run the Naive Bayes algorithm
classifier <- naiveBayes(mat_train, as.factor(mtrain[,3]))

# Validation
predicted <- predict(classifier, mat_test)

# Model performance metrics

# Confusion Matrix
confusionMatrix(predicted, mtest[,3])
# Another way of confusion matrix
CrossTable(predicted, mtest[, 3], prop.chisq = FALSE, prop.t = FALSE, dnn = c("predicted","actual"))

# Kappa value in the confusionMatrix - is a measure of how much of the predictions are occuring by 
# random chance.. The value lies between 0 and 1 .. and closer the value is towards 1, it means the 
# model is quite good in predicting the values and the predictions are not occuring by chance

# here our Kappa value is pretty low, which may be because the dataset is very small