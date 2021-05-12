# ******************************************************************************************************
# Decision Trees - Body Fat Measurement
# 
# DXA - Dual Energy X-ray absorptiometry - measures body fat percentage (& bone density) accurately.
# However setup requirement, cost and exposure considerations mean that a good easy predictor would
# be a boon in the hands of a common medical practiotioner.. The data
#
# age - in years
# DEXfat - body fat measured by DXA, response variable
# waistcirc - waist circumference
# hipcirc - hip circumference
# elbowbreadth - breadth of the elbow
# kneebreadth - breadth of the knee
# anthro(3a, b, c, 4) - sum of lograthim of three antropometric measurements
#
# The objective is to build a decision tree for estimating DEXfat using rpart package
# 
# ******************************************************************************************************
rm(list = ls()) # clear all runtime environment variables
options(scipen = 999) # No automatic roundoffs

# Use of external libraries
library(TH.data) # This will have the dataset
library(rpart)
library(rpart.plot)

data("bodyfat") # use the data bodyfat

# ********** Data Exploration and Data Preparation Block **************************************************** #

# Check for missing values
sum(is.na(bodyfat)) # there are no missing values

# Obtain Summary Statistics
summary(bodyfat)
# Check on the standard deviations
sapply(bodyfat, sd)

# we will study the dependent variable
boxplot(bodyfat$DEXfat, main = "Box Plot for DEXfat", xlab = "DEXfat")
hist(bodyfat$DEXfat, labels = TRUE, col = "dark green", xlab = "DEXfat", ylab = "Frequency", main = "Histogram for DEXfat")
plot(bodyfat$DEXfat, xlab = "DEXfat", ylab = "Frequencies", main = "Scatter Plot for DEXfat", pch = 19, col = "blue", cex = 0.75)
# looking at the above plots, we see two observations on the extreme..
subset.data.frame(bodyfat, bodyfat$DEXfat >= 60)
# Seem to be obese data, cannot be treated as outlier

# Lets see the correlations between DEXfat and other variables
cor(bodyfat)
# also an estimate on possible relationships
plot(bodyfat)
# Looking at the plots, we see DEXfat has strong relationships with waistcirc, hipcirc, kneebreadth
# and all four anthro variables
# Taking a detailed look at
# Vs. waistcirc
plot(bodyfat$DEXfat ~ bodyfat$waistcirc, main = "DEXfat vs waistcirc", xlab = "Waist Circumference", ylab = "DEXfat", col = "dark green", cex = 0.75, pch = 19)
# Vs. hipcirc
plot(bodyfat$DEXfat ~ bodyfat$hipcirc, main = "DEXfat vs hipcirc", xlab = "Hip Circumference", ylab = "DEXfat", col = "dark green", cex = 0.75, pch = 19)
# Vs. kneebreadth
plot(bodyfat$DEXfat ~ bodyfat$kneebreadth, main = "DEXfat vs Knee Breadth", xlab = "Knee Breadth", ylab = "DEXfat", col = "dark green", cex = 0.75, pch = 19)
# Vs. anthro4
plot(bodyfat$DEXfat ~ bodyfat$anthro4, main = "DEXfat vs anthro4", xlab = "anthro4", ylab = "DEXfat", col = "dark blue", cex = 0.75, pch = 19)
# Vs. anthro 3a
plot(bodyfat$DEXfat ~ bodyfat$anthro3a, main = "DEXfat vs anthro3a", xlab = "anthro3a", ylab = "DEXfat", col = "dark blue", cex = 0.75, pch = 19)
# Vs. anthro 3c
plot(bodyfat$DEXfat ~ bodyfat$anthro3c, main = "DEXfat vs anthro3c", xlab = "anthro3c", ylab = "DEXfat", col = "dark blue", cex = 0.75, pch = 19)

# Looking at the above analysis, it is apparent that a regression algorithm can be used as well
# to predict the value of DEXfat. However, we will use decision tree to predict an average 
# value of DEXfat which will depict the underlying rules as well.

# We will not use the anthro* variables as they are hard to understand and log transformations
# more scientific measurements and end user correlation is difficult

# dropping this variable for the decision tree
modifiedbodyfat <- bodyfat[,-c(7,8,9,10)]
# check the modified dataset
head(modifiedbodyfat)

# ********** Decision Tree **************************************************** #

# we will now build the first iteration of the decision tree
# We will use the method anova because we are predicting a continuous variable
tree_model <- rpart(DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth,
                    data = modifiedbodyfat, method = "anova")

# Now lets inspect the model
tree_model
# Lets plot tree_model and visualize the tree
plot(tree_model) # This shows the skeleton tree
text(tree_model) # This shows the tree with labels

# The decision tree predicts the average values of the DEXfat and tells and audience by 
# looking at the average which category the patient might fall in...
# This is the jist of partitioning by regression trees

# to find the effectiveness of the tree model, lets look at the cost complexity of the tree
printcp(tree_model)

# The above tells us, which variables are used to construct a tree and at each split, what 
# is the relative error, classification error(xerror) and std error(xstd)
# What is more important to us is the classification error
# We see that the minimum classification error is 0.27255..

# Now we dont know, whether this classification error is minimum or we can further build the
# tree.. (tree should be built at a point, where there is minimum classfication error)

# To find what would be an allowable classification error for this case, we re-run the tree
# specifying extreme conditions.. (using control parameters)

tree_model <- rpart(DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth,
                    data = modifiedbodyfat, method = "anova", 
                    control = rpart.control(minsplit = 5, cp = 0.00001))
# minsplit = 5 means, to apply the splitting algorithm there has to be atleast 5 cases in a split
# otherwise the splitting algorithm cannot be applied and cp is the control parameter limit

# now to inspect the model
tree_model
# Now the tree gets much deeper, which can be visualized as well
plot(tree_model)
text(tree_model) # obviously this is hard to read

# now lets guage the effectiveness again
printcp(tree_model)
# Now if we inspect the cp table , we see the classification error (xerror) comes down to
# 0.16909 and then post which it starts increasing again.. So at level 0.16909 is the level,
# where we have minimum classification error

# hence we will prune the tree at this level (at this cost complexity value)
# so lets get this cost complexity value for this minimum xerror

# first lets find the min xerror
min(tree_model$cptable[,"xerror"])
# correspoding  CP value is obtained as
minCP <- tree_model$cptable[which.min(tree_model$cptable[,"xerror"]),"CP"]
# Now we will use this min cp value and prune the tree
prune_tree <- prune(tree_model, cp = minCP) 
# The tree is pruned either by depth or by complexity parameter.. here we use CP

# now lets inspect the prune tree
prune_tree
# Now visualize
plot(prune_tree)
text(prune_tree) # since this is also hard to read

# For better visualization of the tree
prp(prune_tree)
# The above visualization shows all rules involving all predictor variables
# Please note as the depth of the tree increases, more and more variables are brought in 
# to build the tree

# to get predictions from our model
predictions <- predict(prune_tree, newdata = modifiedbodyfat)
head(predictions) # This is going to give us the mean values which the tree has predicted
# to plot our predictions vs actual
plot(predictions ~ modifiedbodyfat$DEXfat)

# we see our predictions are very close to the actual