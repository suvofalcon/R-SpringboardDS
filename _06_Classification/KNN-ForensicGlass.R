# ******************************************************************************************************
# K-Nearest Neighbor Classification - Forensic Glass Dataset
# 
# We analyze the data set of 214 glass shards of six possible glass types
# based on the available measurements on shard characteristics including Refractive index (RI)
# Which group of glass does the sample belong to..?
# ******************************************************************************************************

options(scipen = 999)

# use of external libraries
library(class)
library(MASS) # The forensic glass dataset is inbuilt in this package
library(ggplot2)
library(plyr)
library(caret)

# Load the dataset
data(fgl)
# Check the data load
str(fgl)

# Check on the frequency distribution of response variable type
table(fgl$type) # Proportions are very uneven

head(fgl, 10)
# Looking at the data rows, we see the rows are arranged / grouped by type in an orderly manner. 
# This would not work for KNN classification and we need to randomize the order.. 
# In essence we need to shuffle our dataset 

# ********** Data Exploration and Data Preparation Block *************************************** #

# Check for missing values
sum(is.na(fgl)) # There are no missing values in the dataset

# We will do some visualizations and exploration on the dataset

# Studying each of the type vs RI (Refractive Index)
ggplot(fgl, aes(x = type, y = RI)) + geom_boxplot(aes(color = type))
ggplot(fgl, aes(x = seq_along(RI), y = RI, fill = type)) + 
  geom_point(aes(color = type), pch = 19, cex = 0.75) + xlab("Counts") + ylab("Refractive Index")
# The refractive index falls within very distinctive range for each of the types of glass
# visualizing a bar plot with average RI by type
aggSet <- ddply(fgl, "type", summarise, meanval = mean(RI))
ggplot(aggSet, aes(x = type, y = meanval, fill = type)) + 
  geom_bar(stat = "identity", position = "identity", col = "black") +
  ylab("Avg Refractive Index")
# The average Refractive Index for various types are very different than each other...

# Studying each of the type vs Na (Amount of Sodium)
ggplot(fgl, aes(x = type, y = Na)) + geom_boxplot(aes(color = type))
ggplot(fgl, aes(x = seq_along(Na), y = Na, fill = type)) + 
  geom_point(aes(color = type), pch = 19, cex = 0.75) + xlab("Counts") + ylab("Sodium Presence") 
# The presence of Na falls within very distinctive range for each of the types of glass
# visualizing a bar plot with average Na by type
aggSet <- ddply(fgl, "type", summarise, meanval = mean(Na))
ggplot(aggSet, aes(x = type, y = meanval, fill = type)) + 
  geom_bar(stat = "identity", position = "identity", col = "black") +
  ylab("Avg Na (Sodium)")
# The average Sodium presence for all types of glass is relatively high and for Tabl and Head glass
# it is particularly higher than the rest...

# Studying each of the type vs Mg (Amount of Magnesium)
ggplot(fgl, aes(x = type, y = Mg)) + geom_boxplot(aes(color = type))
ggplot(fgl, aes(x = seq_along(Mg), y = Mg, fill = type)) + 
  geom_point(aes(color = type), pch = 19, cex = 0.75) + xlab("Counts") + ylab("Magnesium Presence") 
# Except for categories WinF and winNF the magnesium contents are different across other glass types
# visualizing a bar plot with average Mg by type
aggSet <- ddply(fgl, "type", summarise, meanval = mean(Mg))
ggplot(aggSet, aes(x = type, y = meanval, fill = type)) + 
  geom_bar(stat = "identity", position = "identity", col = "black") +
  ylab("Avg Mg (Magnesium)")
# The average Sodium presence for all types of glass is relatively high and for Tabl and Head glass
# it is particularly higher than the rest...

# Studying each of the type vs Al (Amount of Aluminium)
ggplot(fgl, aes(x = type, y = Al)) + geom_boxplot(aes(color = type))
ggplot(fgl, aes(x = seq_along(Al), y = Al, fill = type)) + 
  geom_point(aes(color = type), pch = 19, cex = 0.75) + xlab("Counts") + ylab("Aluminium Presence") 
# visualizing a bar plot with average Al by type
aggSet <- ddply(fgl, "type", summarise, meanval = mean(Al))
ggplot(aggSet, aes(x = type, y = meanval, fill = type)) + 
  geom_bar(stat = "identity", position = "identity", col = "black") +
  ylab("Avg Al (Aluminium)")
# Presence of Aluminium is also different across various types

# Studying each of the type vs Si (Amount of Silicon)
ggplot(fgl, aes(x = type, y = Si)) + geom_boxplot(aes(color = type))
ggplot(fgl, aes(x = seq_along(Si), y = Si, fill = type)) + 
  geom_point(aes(color = type), pch = 19, cex = 0.75) + xlab("Counts") + ylab("Silicon Presence") 
# visualizing a bar plot with average Si by type
aggSet <- ddply(fgl, "type", summarise, meanval = mean(Si))
ggplot(aggSet, aes(x = type, y = meanval, fill = type)) + 
  geom_bar(stat = "identity", position = "identity", col = "black") +
  ylab("Avg Si (Silicon)")
# Silicon presence in various types of glass are uniform and seems to be not having a statistical significance
# in classifying a glass type

# Studying each of the type vs K (Amount of Potassium)
ggplot(fgl, aes(x = type, y = K)) + geom_boxplot(aes(color = type))
ggplot(fgl, aes(x = seq_along(K), y = K, fill = type)) + 
  geom_point(aes(color = type), pch = 19, cex = 0.75) + xlab("Counts") + ylab("Potassium Presence") 
# visualizing a bar plot with average K by type
aggSet <- ddply(fgl, "type", summarise, meanval = mean(K))
ggplot(aggSet, aes(x = type, y = meanval, fill = type)) + 
  geom_bar(stat = "identity", position = "identity", col = "black") +
  ylab("Avg K (Potassium)")
# Glass type Con has a distinctive higher Potassium content than any other glass type

# Studying each of the type vs Ca (Amount of Calcium)
ggplot(fgl, aes(x = type, y = Ca)) + geom_boxplot(aes(color = type))
ggplot(fgl, aes(x = seq_along(Ca), y = Ca, fill = type)) + 
  geom_point(aes(color = type), pch = 19, cex = 0.75) + xlab("Counts") + ylab("Calcium Presence") 
# visualizing a bar plot with average Ca by type
aggSet <- ddply(fgl, "type", summarise, meanval = mean(Ca))
ggplot(aggSet, aes(x = type, y = meanval, fill = type)) + 
  geom_bar(stat = "identity", position = "identity", col = "black") +
  ylab("Avg Ca (Calcium)")
# Calcium content is relatively higher across all types and specifically for type con

# Studying each of the type vs Ba (Amount of Barium)
ggplot(fgl, aes(x = type, y = Ba)) + geom_boxplot(aes(color = type))
ggplot(fgl, aes(x = seq_along(Ba), y = Ba, fill = type)) + 
  geom_point(aes(color = type), pch = 19, cex = 0.75) + xlab("Counts") + ylab("Barium Presence") 
# visualizing a bar plot with average Ba by type
aggSet <- ddply(fgl, "type", summarise, meanval = mean(Ba))
ggplot(aggSet, aes(x = type, y = meanval, fill = type)) + 
  geom_bar(stat = "identity", position = "identity", col = "black") +
  ylab("Avg Ba (Barium)")
# Other than glass type Head - all others have relatively very low barium content

# Studying each of the type vs Fe (Amount of Fe)
ggplot(fgl, aes(x = type, y = Fe)) + geom_boxplot(aes(color = type))
ggplot(fgl, aes(x = seq_along(Fe), y = Fe, fill = type)) + 
  geom_point(aes(color = type), pch = 19, cex = 0.75) + xlab("Counts") + ylab("Iron Presence") 
# visualizing a bar plot with average Fe by type
aggSet <- ddply(fgl, "type", summarise, meanval = mean(Fe))
ggplot(aggSet, aes(x = type, y = meanval, fill = type)) + 
  geom_bar(stat = "identity", position = "identity", col = "black") +
  ylab("Avg Fe (Iron)")
# Glass type Tabl and Head has a very low Fe content than all other types

# Let us shuffle the iris dataset - orderly arrangement will not work for KNN classification
set.seed(1234)
# This is a number of R's random number generator. The major advantage of setting a seed is that we can get the same sequence of 
# random numbers whenever we supply the same seed in the random number generator.
# we would create a group of random numbers as equal to size of the dataset
gp <- runif(nrow(fgl))
# We will now shuffle the fgl dataset.. arrange the rows in the order of the random number gp and keep all
# the columns
fgl_shuffled <- fgl[order(gp),]
# Lets verify the arrangement, by checking the first 10 observations
head(fgl_shuffled, 10)

# Lets check the summary of all numerical variables except the response variable
summary(fgl_shuffled[-10])
# We see for every variable the scale is different

# As a part of data preparation, we first normalize the values, where we transform all the values to a common scale
# we write our own normalize function
normalize <- function(colVariable) {
  numerator <- colVariable - min(colVariable)
  denominator <- max(colVariable) - min(colVariable)
  return (numerator/denominator)
}
# We will now use the normalize function and scale the fgl_shuffled dataset
fgl_shuffled_n <- as.data.frame(lapply(fgl_shuffled[1:9], normalize))
# if we see the numerical column summary for our normalized dataset
summary(fgl_shuffled_n)

# Now we will split fgl_shuffled_n into a training and test dataset - 
# knn will be applied on training dataset to learn and then verified against test dataset
fgl_train <- fgl_shuffled_n[1:180,]
fgl_test <- fgl_shuffled_n[181:214,]

# we isolate the response variable again into train and test from the original shuffled dataset
fgl_train_target <- fgl_shuffled[1:180,10]
fgl_test_target <- fgl_shuffled[181:214,10]

# ********** Classification Model **************************************************** #

# To run the k-nearest neighbor algorithm we need to use the training dataset, 
# test dataset and use the training target variable

# Now k is a user defined value and the rule of thumb is k = sqrt(number of observations)
# k means - how many nearest neighbors we want the algorithm to calculate and it is 
# always benefecial to use a odd number, because since it takes majority votes .. 
# in case of even number we may end up with a tie in which case the resolution will be random

# Lets use k = 15 (approx sqrt(214))

knn_classification <- knn(train = fgl_train, test = fgl_test, cl = fgl_train_target, k = 11)

# Now knn_classification has the prediction of classification of all the values in the test dataset
confusionMatrix(knn_classification, fgl_test_target)

# ConfusionMatrix shows a prediction accuracy of 67.65%