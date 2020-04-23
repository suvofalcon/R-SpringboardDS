# ******************************************************************************************************
# Naive Bayes Classification - Dataset used is iris
# 
# Based on Sepal.Length, Sepal.Width, Petal.Length, and Petal.Width
# We would predict the classification of our response Variable - Species
# Which species of the flower is - based on Sepal and Petal length and width
# ******************************************************************************************************
options(scipen = 999) # No automatic roundoffs
rm(list = ls()) # clear existing environment variables

# Use of external libraries
library(e1071)
library(caret)
library(plyr)

# We will use the inbuilt iris dataset
data(iris)
# look at the structure of the dataset
str(iris)

# Lets look into the frequency distribution of our response variable species
table(iris$Species)
# Quick look on the data - initial few rows
head(iris, 10)
# Looking at the data rows, we see the rows are arranged / grouped by species in an orderly manner. 
# This would not work for Naive Bayes classification and we need to randomize the order.. 
# In essence we need to shuffle our dataset 

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

# Let us shuffle the iris dataset - orderly arrangement will not work for Naive Bayes classification
# We need to have a proper mix of all different groups in training and test dataset
set.seed(1234)
# This is a number of R's random number generator. The major advantage of setting a seed is that we can get the same sequence of 
# random numbers whenever we supply the same seed in the random number generator.
# we would create a group of random numbers as equal to size of the dataset
gp <- runif(nrow(iris))
# I will now order my iris dataset  - put the rows in the order of the random number set gp and keep all the colums
iris_shuffled <- iris[order(gp),]
# now lets verify the arrangement -- see the first 10 observations
head(iris_shuffled, 10)

# Now we will split iris_shuffled into a training and test dataset - naive bayes
# will be applied on training dataset to learn and then verified against test dataset

# we decide to keep 130 rows in training dataset and remaining 20 rows in test dataset - since the normalized dataset is already 
# randomized, we take 130 rows and 20 rows from the beginning
iris_train <- iris_shuffled[1:130, -5]
iris_test <- iris_shuffled[131:150, -5]

# we isolate the response variable again into train and test from the original shuffled dataset
iris_train_target <- iris_shuffled[1:130, 5]
iris_test_target <- iris_shuffled[131:150, 5]

# ********** Classification Model **************************************************** #

# We run the naive bayes classifier

bayes_classification <- naiveBayes(iris_train, iris_train_target)
# Once the classification is done, we would apply this classification in training dataset to predict the results
pred <- predict(bayes_classification,iris_test)
# We will generate a confusion Matrix to verify
confusionMatrix(pred,iris_test_target)

# Confusion Matrix shows an accuracy of 95% correct predictions...
