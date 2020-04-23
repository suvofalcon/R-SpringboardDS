# ******************************************************************************************************
# Random Forest - Forensic Glass Dataset
# 
# We analyze the data set of 214 glass shards of six possible glass types
# based on the available measurements on shard characteristics including Refractive index (RI)
# Which group of glass does the sample belong to..?
# ******************************************************************************************************
rm(list = ls()) # clear all runtime variables and functions in environment
options(scipen = 999) # No automatic round offs of numbers

# Use of external libraries
library(MASS)
library(caret)
library(plyr)
library(ggplot2)
library(randomForest)

# Load the dataset
data(fgl)
# Check the data load
str(fgl)
# Check on the frequency distribution of response variable type
table(fgl$type) # Proportions are very uneven
# Look at intial 10 rows
head(fgl, 10)
# Looking at the data rows, we see the rows are arranged / grouped by type in an orderly manner. 
# This would not work for any classification and we need to randomize the order.. 
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

# ********** Data Sampling **************************************************** #

# Lets check the distribution of the various types
table(fgl$type)

# Since the proportion of the types is very uneven, we have to ensure that all types are present in the same proportion as original in the training as well as 
# test dataset

# So we will sample in each individual types in the ratio of 70:30 (attempting a stratified sampling)

# First we will create the training and test dataset in the same proportion of types

subset_data <- subset(fgl, fgl$type == "WinF")
smp_size <- floor(0.75 * nrow(subset_data))
train_ind <- sample(seq_len(nrow(subset_data)), size = smp_size)
fgl_train <- subset_data[train_ind,]
fgl_test <- subset_data[-train_ind,]

subset_data <- subset(fgl, fgl$type == "WinNF")
smp_size <- floor(0.75 * nrow(subset_data))
train_ind <- sample(seq_len(nrow(subset_data)), size = smp_size)
fgl_train <- rbind(fgl_train,subset_data[train_ind,])
fgl_test <- rbind(fgl_test, subset_data[-train_ind,])

subset_data <- subset(fgl, fgl$type == "Veh")
smp_size <- floor(0.75 * nrow(subset_data))
train_ind <- sample(seq_len(nrow(subset_data)), size = smp_size)
fgl_train <- rbind(fgl_train,subset_data[train_ind,])
fgl_test <- rbind(fgl_test, subset_data[-train_ind,])

subset_data <- subset(fgl, fgl$type == "Con")
smp_size <- floor(0.75 * nrow(subset_data))
train_ind <- sample(seq_len(nrow(subset_data)), size = smp_size)
fgl_train <- rbind(fgl_train,subset_data[train_ind,])
fgl_test <- rbind(fgl_test, subset_data[-train_ind,])

subset_data <- subset(fgl, fgl$type == "Tabl")
smp_size <- floor(0.75 * nrow(subset_data))
train_ind <- sample(seq_len(nrow(subset_data)), size = smp_size)
fgl_train <- rbind(fgl_train,subset_data[train_ind,])
fgl_test <- rbind(fgl_test, subset_data[-train_ind,])

subset_data <- subset(fgl, fgl$type == "Head")
smp_size <- floor(0.75 * nrow(subset_data))
train_ind <- sample(seq_len(nrow(subset_data)), size = smp_size)
fgl_train <- rbind(fgl_train,subset_data[train_ind,])
fgl_test <- rbind(fgl_test, subset_data[-train_ind,])

# Once it is done, we will randomise the order, since orderly arrangement will not work for any Classification
randomiseDataSet <- function (ds){
  set.seed(5678)
  gp <- runif(nrow(ds))
  ds <- ds[order(gp),]
  return (ds)
}
# randomise the training and test dataset
fgl_train <- randomiseDataSet(fgl_train)
fgl_test <- randomiseDataSet(fgl_test)

# ********** Build the Random Forest **************************************************** #

# lets build the random forest using the training dataset
# We want the importance of the variables to be assessed, so that we can plot them later
# and we want to try 2 variables at each split (default is 3 and this can be changed, based on OOB estimate of error rate)
# to correctly estimate mtry (number of variables for each split), we may use tuneRF function
tuneRF(fgl_train[,-10], fgl_train[,10], stepFactor = 1.5) # we see mtry as 2 is a better candidate with least OOB error

forest_model <- randomForest(type ~., data = fgl_train, importance = TRUE, do.trace = TRUE, mtry = 2)

# lets inspect the model
forest_model # this also shows the confusion matrix and overall error rate

# now lets try some plots to visualize the model better
varImpPlot(forest_model)
# the above shows the mean decrease in accuracy and mean decrease in gini score as a function of each of the variables of the model 

# we see that Mg, RI and Al contributes max in increase the accuracy of the model and Fe contributes the least
# similarly Mg, AL and RI contributes max in increase of the Gini coefficients and Fe contributes the least ..

# Hence we may drop the Fe and rebuild the tree and see if there is any reduction in error rate or change

# lets look at relative importance of each of the independent variables in classifying each of the particular type of class
forest_model$importance
# We can see from the table that for float glass (WinF) RI has 17.9% importance and Mg has 12.1% importance

# if we plot the forest_model itself, we see
plot(forest_model)
# The above tells how the error in classification increase or decrease as we increase the number of trees

# now if we want to use this forest_model on our test dataset
predictions <- predict(forest_model, newdata = fgl_test[,-10], type = "class")
# build the confusion matrix to see how well it has performed on the test dataset
confusionMatrix(predictions, fgl_test[,10])
