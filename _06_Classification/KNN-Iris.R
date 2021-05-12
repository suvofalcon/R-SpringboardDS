# ******************************************************************************************************
# K-Nearest Neighbor Classification - Dataset used is iris
# 
# Based on Sepal.Length, Sepal.Width, Petal.Length, and Petal.Width
# We would predict the classification of our response Variable - Species
# Which species of the flower is - based on Sepal and Petal length and width
# ******************************************************************************************************
options(scipen = 999)

# use of external libraries
library(class)
library(ggplot2)
library(plyr)
library(caret)

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

# Let us shuffle the iris dataset - orderly arrangement will not work for KNN classification
set.seed(1234)
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

# As a part of data preparation, we first normalize the values, where we transform all the values to a common scale
# we write our own normalize function
normalize <- function(colVariable) {
  numerator <- colVariable - min(colVariable)
  denominator <- max(colVariable) - min(colVariable)
  return (numerator/denominator)
}
# We will now use the normalize function and scale the iris_shuffled dataset
iris_shuffled_n <- as.data.frame(lapply(iris_shuffled[1:4], normalize))
# if we see the numerical column summary for our normalized dataset
summary(iris_shuffled_n)
# All min and max values of the variables are same and hence it is now scaled

# Now we will split iris_shuffled_n into a training and test dataset - knn will be applied on training dataset to learn and then verified 
# against test dataset

# we decide to keep 130 rows in training dataset and remaining 20 rows in test dataset - since the normalized dataset is already 
# randomized, we take 130 rows and 20 rows from the beginning
iris_train <- iris_shuffled_n[1:130,]
iris_test <- iris_shuffled_n[131:150,]

# we isolate the response variable again into train and test from the original shuffled dataset
iris_train_target <- iris_shuffled[1:130, 5]
iris_test_target <- iris_shuffled[131:150, 5]


# ********** Classification Model **************************************************** #

# To run the k-nearest neighbor algorithm we need to use the training dataset, test dataset and use the training target variable

# Now k is a user defined value and the rule of thumb is k = sqrt(number of observations)
# k means - how many nearest neighbors we want the algorithm to calculate and it is always benefecial to use a odd number, because since
# it takes majority votes .. in case of even number we may end up with a tie in which case the resolution will be random

# Lets use k = 13 (approx sqrt(150))

model_classification <- knn(train = iris_train, test = iris_test, cl = iris_train_target, k = 13)

# Now model_classification has the prediction of classification of all the values in the test dataset
confusionMatrix(model_classification, iris_test_target)

# Confusion Matrix shows an accuracy of 95% correct predictions...
