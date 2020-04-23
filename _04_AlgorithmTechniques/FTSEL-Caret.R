# ************************************************************************************************************************
# Feature Section using the Caret Package
#
# Selecting the right features in your data can mean the difference between 
# mediocre performance with long training times and great performance with short training times.
#
# The caret R package provides tools automatically report on the relevance and importance of 
# attributes in your data and even select the most important features for you.
# In this code We will discover the feature selection tools in the Caret R package with 
# standalone recipes in R.
#
# How to remove redundant features from your dataset.
# How to rank features in your dataset by their importance.
# How to select features from your dataset using the Recursive Feature Elimination method.
# ************************************************************************************************************************

# No Automatic roundoffs
options(scipen = 999)
rm(list = ls()) # Clear all environment variables

# Use of external libraries
library(mlbench)
library(caret)
library(pROC)

# ********************* REMOVE REDUNDANT FEATURES **********************************#

# Data can contain attributes that are highly correlated with each other. Many methods perform 
# better if highly correlated attributes are removed.
# The Caret R package provides the findCorrelation which will analyze a correlation matrix of your 
# data’s attributes report on attributes that can be removed.
# A correlation matrix is created from these attributes and highly correlated attributes are identified, 
# in this case the age attribute is remove as it correlates highly with the pregnant attribute.

# Generally, you want to remove attributes with an absolute correlation of 0.75 or higher.

# The following code loads the Pima Indians Diabetes dataset that contains a number of 
# biological attributes from medical reports.

# load the data
data("PimaIndiansDiabetes")
# Check the data load
head(PimaIndiansDiabetes)
# Check the structure of the dataset
str(PimaIndiansDiabetes)
dim(PimaIndiansDiabetes)
# Check for missing values
colSums(is.na(PimaIndiansDiabetes))

# Calculate the correlation matrix (except for the response variable column - diabetes)
correlationMatrix <- cor(PimaIndiansDiabetes[,1:8])
# Lets examine the correlationMatrix
print(correlationMatrix)
# lets find attributes that are highly correlated (ideally > 0.5)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.5)
# Lets print indexes of highly correlated attributes
print(highlyCorrelated)
# Lets find the column name
colnames(PimaIndiansDiabetes[highlyCorrelated])
# we see it is "age" variable - and the same is dropped from any modelling... 


# ********************* RANK FEATURES BY IMPORTANCE **********************************#

# Here features are ranked based on importance. In this methodology a "Learning Vector Quantization" model
# is built (Special case of neural networks), then variable importance is derived from there

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model <- train(diabetes ~., data = PimaIndiansDiabetes, method = "lvq", preProcess = "scale", trControl = control)
# once the training on LVQ is done, estimate the variable importance
importance <- varImp(model, scale = FALSE)
# summarize the importance
print(importance)
# plot the importanct
plot(importance)

# We see that Glucose, Mass and Age are the three most important variables


# ********************* FEATURE SELECTION **********************************#

# A technique of Recursive Feature elimination is used for automatic feature selection.
# Here a Random Forest algorithm is used on each iteration to evaluate the model. The algorithm is used to
# explore all possible subsets of attributes...

# Lets define the control using a random forest selection function
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
# Run the RFE algorithm
results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes = c(1:8), rfeControl = control)
# Lets summarize the results
print(results)
# List the chosen features
predictors(results)
# Lets plot the results
plot(results, type = c("g","o"))
# From the plot we see the five is the optimum number of variables, beyond which any 
# increase in the number of variable, doesnt cause significant increase in accuracy