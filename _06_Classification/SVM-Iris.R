# ******************************************************************************************************
# Support Vector Machines - Dataset used is iris
# 
# Based on Sepal.Length, Sepal.Width, Petal.Length, and Petal.Width
# We would predict the classification of our response Variable - Species
# Which species of the flower is - based on Sepal and Petal length and width
# ******************************************************************************************************
rm(list = ls()) # Clear all runtime variables, functions in the environment
options(scipen = 999) # No Automatic roundoffs

# Use of external libraries
library(e1071)
library(ggplot2)
library(plyr)

# We will use the inbuilt iris dataset
data(iris)
# look at the structure of the dataset
str(iris)
# Lets look into the frequency distribution of our response variable species
table(iris$Species) # Equal distribution of data

# ********** Data Exploration and Data Preparation Block *************************************** #

# Check for missing data
sum(is.na(iris)) # There are no missing values

# summary statistics
summary(iris[,-5])

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

# Lets do a sampling now and create a training and test sample

# This is a number of R's random number generator. The major advantage of setting a seed is that we can get the same sequence of 
# random numbers whenever we supply the same seed in the random number generator.
set.seed(4321)
sampleIndex <- sample(nrow(iris), 0.7*nrow(iris))
# Create train and test samples
iris_train <- iris[sampleIndex,]
iris_test <- iris[-sampleIndex,]

# ********** Support Vector Machine Classification **************************************************** #

# we run the SVM classification
svmfit <- svm(Species ~., data = iris_train, kernel = "linear", cost = 0.1, scale = FALSE)

# Lets plot the model - We will use two variables at a time and the response variable
plot(svmfit, iris_train, Sepal.Length~Sepal.Width)
plot(svmfit, iris_train, Petal.Length~Petal.Width)
# From the above plots, we see that Petal.Length and Petal.Width is able to seggregate the classes quite well
# Whereas there is a considerable overlap when we try to arrive at a classification with only Sepal.Length and 
# Sepal.Width -- This is also observed during data exploration as well

# What are our support vectors (support vectors are data points which kind of supports the hyperplane on
# both sides)
svmfit$index
# Number of support vectors
length(svmfit$index) # There are 50 support vectors

# see the summary
summary(svmfit)
# This shows number of support vectors are 50. Out of which 24 observations are support vectors from "setosa" class
# 21 observations are support vectors from "versicolor" class and 5 observations are support vectors from
# "virginica" class

# Now we would want wider margins (maximize the distance between the hyperplane and nearest support vector)
# To have wider margin possible, we would need an optimum cost parameter..

# We will tune the svm model, to get the optimum cost parameter
# cross-validation using tune
set.seed(4321)
tune.out <- tune(svm, Species ~., data = iris_train, kernel = "linear", ranges = 
                   list(cost = c(0.001, 0.01, 0.1, 1, 5, 10,100)))
summary(tune.out) # we will get the cost which is giving us the lowest cross-validation error rate

svmfit.bestmodel <- tune.out$best.model
summary(svmfit.bestmodel)

# Now its time for predictions
predictions <- predict(svmfit.bestmodel, iris_test[,1:4], type = "class")
plot(predictions)

# Build the confusion Matrix
confusionMatrix(predictions, iris_test$Species) # ConfusionMatrix shows the accuracy of 91%

##########################################################################################################
# Bonus content - Demonstration of Non-linear SVM's - using Radial or Polynomial
# kernel = "radial" -> uses radial kernel, we need to set gamma
# kernel = "polynomial" uses polynomial kernels
# for this, we need to use degree component to fix it

set.seed(1234)
x <- matrix(rnorm(400), ncol = 2)
# generate 400 random numbers and create a matrix with 200 rows and 2 columns
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))
df <- data.frame(x = x, y = as.factor(y))
plot(x, col = y) # Looking at this plot, we see there cannot be any hyperplane which can linearly separate the
# two classes -- and this implies, we need to go for either radial or polynomial transformation

# lets create the samples
sampleIndex <- sample(200, 100)
train <- df[sampleIndex, ]
test <- df[-sampleIndex, ]

# Run the SVM classification
svmfit <- svm(y ~., data = df, kernel = "radial", gamma = 1, cost = 1)
# Now lets plot and visualize the hyperplane
plot(svmfit, train)
# Check the summary of the svmfit
summary(svmfit)

# to find out the optimized cost and gamma parameter, we would use the tune method
# using the cross validation technique
set.seed(1234)
tune.out <- tune(svm, y ~., data = train, kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100, 10000)),
                 gamma = c(0.5, 1, 2, 3, 4))
summary(tune.out)
# to find the best model

svmfit.bestmodel <- tune.out$best.model
summary(svmfit.bestmodel)

# Now we will visualize the hyperplane with the bestmodel
plot(svmfit.bestmodel, train)

# do the predictions with the bestmodel
predictions <- predict(svmfit.bestmodel, test[,1:2], type = "class")
plot(predictions)
# to see the accuracy, lets build the confusionMatrix
confusionMatrix(predictions, test$y)