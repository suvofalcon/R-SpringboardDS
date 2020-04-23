# ******************************************************************************************************
# Linear Discriminant Analysos - Dataset used is iris
# 
# Based on Sepal.Length, Sepal.Width, Petal.Length, and Petal.Width
# We would predict the classification of our response Variable - Species
# Which species of the flower is - based on Sepal and Petal length and width
# ******************************************************************************************************
rm(list = ls()) # Clear all runtime variables, functions in the environment
options(scipen = 999) # No Automatic roundoffs

# Use of external libraries
library(MASS)
library(ggplot2)
library(plyr)
library(caret)

# We will use the inbuilt iris dataset
data(iris)
# look at the structure of the dataset
str(iris)
# Lets look into the frequency distribution of our response variable species
table(iris$Species) # Equal distribution of data

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


head(iris, 10)
# Looking at the data rows, we see the rows are arranged / grouped by species in an orderly manner. 
# Let us shuffle the iris dataset - orderly arrangement is not proper for predictive algorithm
# We need to have a proper mix of all different groups in training and test dataset
set.seed(4321) 
# This is a number of R's random number generator. The major advantage of setting a seed is that we can get the same sequence of 
# random numbers whenever we supply the same seed in the random number generator.
# we would create a group of random numbers as equal to size of the dataset
gp <- runif(nrow(iris))
# I will now order my iris dataset  - put the rows in the order of the random number set gp and keep all the colums
iris_shuffled <- iris[order(gp),]
# now lets verify the arrangement -- see the first 10 observations
head(iris_shuffled, 10)

# Now we will split iris_shuffled into a training and test dataset - LDA will be applied on training dataset 
# to learn and then verified against test dataset

# we decide to keep 120 rows in training dataset and remaining 30 rows in test dataset - since the normalized dataset is already 
# randomized, we take 120 rows and 30 rows from the beginning
iris_train <- iris_shuffled[1:120,]
iris_test <- iris_shuffled[121:150,]

# ********** Linear Discriminant Analysis **************************************************** #

# We run the lda
lda_model <- lda(Species~., data = iris_train)
# Check the model
lda_model

# We will use this model to run the prediction on test dataset
lda_prediction <- predict(lda_model, iris_test)

# With this prediction, we will build the confusionMatrix to check the accuracy
confusionMatrix(lda_prediction$class, iris_test$Species)

# The confusionMatrix, shows it has predicted with 96% accuracy. Only in one case, where the versicolor, it has
# predicted virginica
