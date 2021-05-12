# ******************************************************************************************************
# Decision Tree - Dataset used is iris
# 
# Based on Sepal.Length, Sepal.Width, Petal.Length, and Petal.Width
# We would predict the classification of our response Variable - Species
# Which species of the flower is - based on Sepal and Petal length and width
# ******************************************************************************************************
rm(list = ls())
options(scipen = 999)

# use of external libraries
library(class)
library(ggplot2)
library(plyr)
library(caret)
library(rpart)
library(rpart.plot)

# We will use the inbuilt iris dataset
data(iris)
# look at the structure of the dataset
str(iris)

# Lets look into the frequency distribution of our response variable species
table(iris$Species)
# Quick look on the data - initial few rows
head(iris, 10)
# Looking at the data rows, we see the rows are arranged / grouped by species in an orderly manner. This would not work for KNN 
# classification and we need to randomize the order.. In essence we need to shuffle our dataset 

# ********** Data Exploration and Data Preparation Block *************************************** #

# Check for missing data
sum(is.na(iris)) # There are no missing values

# Before we prepare the data, lets do some visual analysis

# Studying each of the species by their Sepal.Length
ggplot(iris, aes(x = seq_along(Sepal.Length), y = Sepal.Length, fill = Species)) + 
  geom_point(aes(color = Species), pch = 19, cex = 0.75) + xlab("Counts") + ylab("Sepal Length of Flowers")
# On an average the Sepal length of Setosa species is much smaller and Virginica is the highest
# Visualize a Bar Graph
aggSet <- ddply(iris, "Species", summarise, meanval = mean(Sepal.Length))
ggplot(aggSet, aes(x = Species, y = meanval, fill = Species)) + geom_bar(stat = "identity", col = "black") +
  ylab("Average Sepal Length") + geom_text(aes(label = meanval), vjust = 1.5, colour = "white")

# Studying each of the species by their Sepal.Width
ggplot(iris, aes(x = seq_along(Sepal.Width), y = Sepal.Width, fill = Species)) + 
  geom_point(aes(color = Species), pch = 19, cex = 0.75) + xlab("Counts") + ylab("Sepal Width of Flowers")
# Sepal Width of Setosa is largest and Versicolor is the smallest, however Virginica is very close though
# Visualize a Bar Graph
aggSet <- ddply(iris, "Species", summarise, meanval = mean(Sepal.Width))
ggplot(aggSet, aes(x = Species, y = meanval, fill = Species)) + geom_bar(stat = "identity", col = "black") +
  ylab("Average Sepal Width") + geom_text(aes(label = meanval), vjust = 1.5, colour = "white")

# Studying each of the species by Petal.Length
ggplot(iris, aes(x = seq_along(Petal.Length), y = Petal.Length, fill = Species)) + 
  geom_point(aes(color = Species), pch = 19, cex = 0.75) + xlab("Counts") + ylab("Petal Length of Flowers")
# setosa Petal length is the smallest and Virginica is the highest
# Visualize a Bar Graph
aggSet <- ddply(iris, "Species", summarise, meanval = mean(Petal.Length))
ggplot(aggSet, aes(x = Species, y = meanval, fill = Species)) + geom_bar(stat = "identity", col = "black") +
  ylab("Average Petal Length") + geom_text(aes(label = meanval), vjust = 1.5, colour = "white")

# Studying each of the species by Petal.Width
ggplot(iris, aes(x = seq_along(Petal.Width), y = Petal.Width, fill = Species)) + 
  geom_point(aes(color = Species), pch = 19, cex = 0.75) + xlab("Counts") + ylab("Petal Width of Flowers")
# Visualize a Bar Graph
aggSet <- ddply(iris, "Species", summarise, meanval = mean(Petal.Width))
ggplot(aggSet, aes(x = Species, y = meanval, fill = Species)) + geom_bar(stat = "identity", col = "black") +
  ylab("Average Petal Width") + geom_text(aes(label = meanval), vjust = 1.5, colour = "white")

# Let us shuffle the iris dataset - orderly arrangement will not work for any classification
set.seed(9850)
# This is a number of R's random number generator. The major advantage of setting a seed is that we can get the same sequence of 
# random numbers whenever we supply the same seed in the random number generator.
# we would create a group of random numbers as equal to size of the dataset
gp <- runif(nrow(iris))
# I will now order my iris dataset  - put the rows in the order of the random number set gp and keep all the colums
iris_shuffled <- iris[order(gp),]
# now lets verify the arrangement -- see the first 10 observations
head(iris_shuffled, 10)

# Check the summary of the numeric variables excluding the response /factor variable (column - 5)
summary(iris_shuffled[-5])
# We see for every variable the scale is different - Min, max, mean and even sd
sapply(iris_shuffled[-5],sd)
# Now we will split iris_shuffled_n into a training and test dataset - decision tree will be applied on 
# training dataset to learn and then verified against test dataset

# # we decide to keep 130 rows in training dataset and remaining 20 rows in test dataset - since the normalized dataset is already 
# # randomized, we take 130 rows and 20 rows from the beginning
# iris_train <- iris_shuffled[1:130,]
# iris_test <- iris_shuffled[131:150,]

# # we isolate the response variable again into train and test from the original shuffled dataset
# iris_train_target <- iris_shuffled[1:130, 5]
# iris_test_target <- iris_shuffled[131:150, 5]

# An alternate approach of sampling using caTools packages
library(caTools)
set.seed(101)
iris_shuffled$flag <- sample.split(iris_shuffled,SplitRatio = 0.75)
# Check the head
head(iris_shuffled)
iris_train <- subset(iris_shuffled[,-6],iris_shuffled$flag == TRUE)
iris_test <- subset(iris_shuffled[,-6],iris_shuffled$flag == FALSE)
head(iris_train)

# ********** Decision Tree **************************************************** #

# we will now build the first iteration of the decision tree
# We will use the method class because we are predicting a categorical variable
tree_model <- rpart(Species ~., data = iris_train, method = "class")

# to inspect the model
tree_model

# plot and visualize the tree
plot(tree_model)
text(tree_model)
# to find the effectiveness of the model, inspect the cost complexity
printcp(tree_model)

# lets increase the depth of the tree in an attempt to reduce the classification error
tree_model <- rpart(Species ~., data = iris_train, method = "class", 
                    control = rpart.control(minsplit = 5, cp = 0.0001))
# inspect the model
tree_model
# The depth of the tree does not go much with the control parameter and hence we dont see a situation for pruning
# check on the cost complexity
printcp(tree_model)

# let us visualize the ttee
prp(tree_model)

# we will use this tree_model for predictions
predictions <- predict(tree_model, newdata = iris_test[,-5], type = "class")
# build the confusion matrix to guage the accuracy
confusionMatrix(predictions, iris_test[,5])
