# **************************************************************************************************************************************
# Jigsaw BigData Assignment
# Class 11 - Data Manipulation
# **************************************************************************************************************************************

# Using the dataset iris
library(datasets)
data("iris")

# Take a look at the iris dataset, along with look at the structure and initial rows
?iris
head(iris)
str(iris)

# **************************************************************************************************************************************
# 1 - In the iris dataset, what is the mean of 'Sepal.Length' for the Species Virginica?

# mean in the dataset is computed as
# We can group the Sepal.Length for the Species Virginica
SepalLength.Species <- aggregate(iris$Sepal.Length, list(iris$Species), mean)
names(SepalLength.Species) <- c("Species","MeanSepalLength")
# The above data frame has Mean Sepal Length for all Species
# We can subset the data frame to get the Mean Sepal Length for virginica
MeanSepalLength.Virginica <- subset.default(SepalLength.Species$MeanSepalLength, SepalLength.Species$Species == 'virginica')
MeanSepalLength.Virginica

# **************************************************************************************************************************************
# 2 - Again using the 'iris' dataset, what R code needs to be written for returning a vector of the means
# of the variables 'Sepal.Length', 'Sepal.Width', 'Petal.Length', and 'Petal.Width'?

# creating a vector by finding out the mean of each of the columns needed
vec_means <- c(mean(iris$Sepal.Length), mean(iris$Sepal.Width), mean(iris$Petal.Length), mean(iris$Petal.Width))
vec_means

# **************************************************************************************************************************************
# 3 - Using the ‘mtcars’ dataset, how can you calculate the average miles per gallon (mpg) by number of cylinders in the car (cyl)?

data("mtcars")
?mtcars
head(mtcars)
str(mtcars)
# We will use aggregate function to calculate the avg miles per gallon by cylinder
AvgMpg.Cylinder <- aggregate(mtcars$mpg, list(mtcars$cyl), mean)
names(AvgMpg.Cylinder) <- c("Cylinder", "Average MPG")
AvgMpg.Cylinder

# **************************************************************************************************************************************
# 4 - Continuing with the 'mtcars' dataset, what is the absolute difference between the average horsepower of 4-cylinder cars and the 
# average horsepower of 8-cylinder cars?

Abs.difference <- abs(subset.default(AvgMpg.Cylinder$`Average MPG`, AvgMpg.Cylinder$Cylinder == 4)) - abs(subset.default(AvgMpg.Cylinder$`Average MPG`, 
                                                                                                                         AvgMpg.Cylinder$Cylinder == 8))
Abs.difference

# **************************************************************************************************************************************
# 5 - File named “retail_sales-class11.csv” should be used for this problem.
# You will need to import this dataset into R and then write appropriate code for identifying which
# month has generated maximum revenue amongst this data. Please save the entire code and share
# it with us for evaluation.

# Lets load the dataset (//SUVOS-TIME-CAPS)
sales.ds <- read.csv("//Volumes/Data/CodeMagic/Data Files/retail_sales-class11.csv", header = TRUE)

# Check the data load, columns and their respective data types
head(sales.ds)
str(sales.ds)

# There are two ways of doing it

# Method -1 - Using aggregate
#------------------------------------

# aggregation will happen on Revenue Column, by the factor Month, which needs to be given as list
result.ds <- aggregate(sales.ds$Revenue, list(sales.ds$Month), sum)
colnames(result.ds) <- c("Month", "Total Revenue")
# to return which month has generated the max revenue, use the subset function
maxRevenueMonth <- subset.default(result.ds$Month, result.ds$`Total Revenue` == max(result.ds$`Total Revenue`))
as.character(maxRevenueMonth) # Since the return is a factor type, we convert it to character

# Method -2 - using tapply
#------------------------------------

# Unlike aggregate, tapply takes the second argument as factor
result.ds <- tapply(sales.ds$Revenue, sales.ds$Month, sum)
# The return of tapply is an array, so we convert it to data frame by converting it to a table
result.ds <- as.data.frame(as.table(result.ds))
colnames(result.ds) <- c("Month", "Total Revenue")
# to return which month has generated the max revenue, use the subset function
maxRevenueMonth <- subset.default(result.ds$Month, result.ds$`Total Revenue` == max(result.ds$`Total Revenue`))
as.character(maxRevenueMonth) # Since the return is a factor type, we convert it to character
