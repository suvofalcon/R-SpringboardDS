# ************************************************************************************************************************
# Feature Section using the Boruta Package
#
# Boruta is a feature selection algorithm. Precisely, it works as a wrapper algorithm around 
# Random Forest. This package derive its name from a demon in Slavic mythology who dwelled in 
# pine forests.
#
# How does it work?
# ------------------------------
# Firstly, it adds randomness to the given data set by creating shuffled copies of all features 
# (which are called shadow features).
# Then, it trains a random forest classifier on the extended data set and applies a feature
# importance measure (the default is Mean Decrease Accuracy) to evaluate the importance of each 
# feature where higher means more important.
# At every iteration, it checks whether a real feature has a higher importance than the best of 
# its shadow features (i.e. whether the feature has a higher Z score than the maximum Z score of 
# its shadow features) and constantly removes features which are deemed highly unimportant.
# Finally, the algorithm stops either when all features gets confirmed or rejected or it reaches 
# a specified limit of random forest runs.
# ************************************************************************************************************************
options(scipen = 999)
rm(list = ls()) # clear all environment variables

# Use of external libraries
library(Boruta)

# we will load the survival_unemployment.csv 
# we will load the dataset (this is from SUVOS-TIME-CAPS)
switch(Sys.info() [['sysname']],
       Windows = {traindata <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LoanPredTrain.csv", header = TRUE, stringsAsFactors = FALSE)},
       Linux   = {traindata <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LoanPredTrain.csv", header = TRUE,stringsAsFactors = FALSE)},
       Darwin  = {traindata <- read.csv("//Volumes/Data/CodeMagic/Data Files/LoanPredTrain.csv", header = TRUE,stringsAsFactors = FALSE)})

# Lets check the data load
dim(traindata)
head(traindata)
str(traindata)

# Check for missing values
colSums(is.na(traindata))

# The dataset has not only missing values but also blank values.. we will subsitute all those with
# with NA and then treat all the NA together
traindata[traindata == ""] <- NA

# Now we will use the simplest method of missing value treatment - called the list wise deletion
# we will use the complete.cases method - which returns a logical vector with no missing values
traindata <- traindata[complete.cases(traindata),]

# Now some of the attributes needs to be converted to a factor data type
traindata[,c(2:6, 12:13)] <- data.frame(apply(traindata[,c(2:6, 12:13)], 2, as.factor))
# check the conversion
str(traindata)

# Now lets implement the boruta package. We will implement it in all features except LoanID
traindata <- traindata[,-1]
boruta.train <- Boruta(Loan_Status ~., data = traindata, doTrace = 2)
# Lets examine the summary of boruta training object
print(boruta.train)

# Boruta shows which attributes are confirmed important and which are confirmed non important.
# It also marks few attrtibutes as tentative, because the Z score is very close to their best shadow
# attributes and hence Boruta is not able to make a decision

# Now lets plot the Boruta variable importance chart
plot(boruta.train)
# Blue boxplots correspond to minimal, average and maximum Z score of a shadow attribute. Red, yellow 
# and green boxplots represent Z scores of rejected, tentative and confirmed attributes respectively.

# Now time to take decision on tentative attributes
# The tentative attributes will be classified as confirmed or rejected by comparing the median Z score
# of the attributes with the median Z score of the best shadow attribute

final.boruta <- TentativeRoughFix(boruta.train)
# see the result
print(final.boruta)
# see the plot after classification of tentative attributes
plot(final.boruta)

# So the list of the confirmed attributes
getSelectedAttributes(final.boruta, withTentative = FALSE)

# Lets create a data frame of the final result derived from Boruta
boruta.df <- attStats(final.boruta)
class(boruta.df)
print(boruta.df)

# Let’s understand the parameters used in Boruta as follows:
#   
# maxRuns: maximal number of random forest runs. You can consider increasing this parameter if 
# tentative attributes are left. Default is 100.
# 
# doTrace: It refers to verbosity level. 0 means no tracing. 1 means reporting attribute decision as 
# soon as it is cleared. 2 means all of 1 plus additionally reporting each iteration. Default is 0.
# 
# holdHistory: The full history of importance runs is stored if set to TRUE (Default). 
# Gives a plot of Classifier run vs. Importance when the plotImpHistory function is called.
