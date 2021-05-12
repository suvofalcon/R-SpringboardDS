# ******************************************************************************************************
# PCA - Predictive Algorithm using Principal Component Analysis for dimensionality reduction

# Data Used - BigMartSales-3
# ******************************************************************************************************

rm(list = ls()) # Clear the existing environment variables
options(scipen = 999) # No automatic roundoffs

# Use of external libraries
library(dummies)
library(rpart)
library(rpart.plot)


# we will load the BigMartSales-3 dataset (load both the train and test datasets)
# we will load the dataset (this is from SUVOS-TIME-CAPS)
switch(Sys.info() [['sysname']],
       Windows = {train <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/ExternalDataSets/BigMartSales-3/Train_UWu5bXk.csv", header = TRUE)},
       Linux   = {train <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/ExternalDataSets/BigMartSales-3/Train_UWu5bXk.csv", header = TRUE)},
       Darwin  = {train <- read.csv("//Volumes/Data/CodeMagic/Data Files/ExternalDataSets/BigMartSales-3/Train_UWu5bXk.csv", header = TRUE)})
#load the test dataset
switch(Sys.info() [['sysname']],
       Windows = {test <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/ExternalDataSets/BigMartSales-3/Test_u94Q5KV.csv", header = TRUE)},
       Linux   = {test <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/ExternalDataSets/BigMartSales-3/Test_u94Q5KV.csv", header = TRUE)},
       Darwin  = {test <- read.csv("//Volumes/Data/CodeMagic/Data Files/ExternalDataSets/BigMartSales-3/Test_u94Q5KV.csv", header = TRUE)})

# Test dataset would have one column less and hence adding that column with value which is insignificant
test$Item_Outlet_Sales <- 1

# Now as both the datasets are equal, lets combine the datasets
combined <- rbind(train, test)

# Now lets check the dimension and structure of the dataset
dim(combined)
str(combined)

# Check for missing values
colSums(is.na(combined)) # There are 2439 missing values in "Item_Weight" variable
# Check for summary
summary(combined, na.rm = TRUE)
# We see the variable "Item_Visibility" (The % of total display area of all products in a store 
# allocated to the particular product) has minimum value of 0 -- which is not possible and hence needs to be corrected

# ********** Data Preparation Block **************************************************** #

# We will impute missing values with median for the "Item_Weight" variable
combined$Item_Weight[is.na(combined$Item_Weight)] <- median(combined$Item_Weight, na.rm = TRUE)

# For Item_Visibility , we will again median for values only where item visibility is 0
combined$Item_Visibility <- ifelse(combined$Item_Visibility == 0, median(combined$Item_Visibility), combined$Item_Visibility)

"Lets check the levels of the Outlet size variable"
levels(combined$Outlet_Size)
# We see one level is "" -- which we need to replace with "Other"
levels(combined$Outlet_Size)[1] <- "Other"

# Till here, we’ve imputed missing values. Now we are left with removing the dependent (response) variable 
# and other identifier variables( if any). As we said above, we are practicing an unsupervised learning technique, 
# hence response variable must be removed.

subset_data <- subset(combined, select = -c(Item_Outlet_Sales, Item_Identifier, Outlet_Identifier))

# Now lets check the available variables
colnames(subset_data)
head(subset_data)

# Now since PCA works on numeric variables, we have to see if we have any variable which are non-numeric and if there is
# we need to do numerical transformations
str(subset_data)
# We see there are five factor variables

# We will convert all these categorical variables into numeric variable using one hot coding using the dummies package
subset_data <- dummy.data.frame(subset_data, names = c("Item_Fat_Content","Item_Type","Outlet_Size","Outlet_Location_Type",
                                                       "Outlet_Type"))
# Now lets check the data set
str(subset_data)

# ********************************* PCA Analysis *********************************************

# Now we are ready to do PCA.. divide the data into train and test
pca.train <- subset_data[1:nrow(train),]
pca.test <- subset_data[-(1:nrow(train)),]

# The base R function prcomp() is used to perform PCA. By default, it centers the variable to have mean equals to zero. 
# With parameter scale. = T, we normalize the variables to have standard deviation equals to 1.
prin_comp <- prcomp(pca.train, scale. = TRUE)

# prin_comp results in five useful measures
names(prin_comp)

# 1. center and scale refers to the respective mean and standard deviation of the variables that are used for normalization prior to
# the PCA
# outputs the mean of variables
prin_comp$center
# outputs the standard deviation of the variables
prin_comp$sdev

# 2. The rotation measure provides the principal component loading. Each column of the rotation matrix contains the principal
# component loading vector for the respective independent variable
prin_comp$rotation
# to see the first 5 principal component and first 5 rows
prin_comp$rotation[1:5, 1:5]

# plot the principal components
biplot(prin_comp, scale = 0)
# the parameter scale = 0 ensures that the arrows are scaled to represent the loadings

# to compute standard deviation for each principal components
std_dev <- prin_comp$sdev
# compute variance
pr_var <- std_dev^2

# We aim to find the components which explain the maximum variance. This is because, 
# we want to retain as much information as possible using these components. 
# So, higher is the explained variance, higher will be the information contained in those components.
prop_varex <- pr_var/sum(pr_var)
# to check the proportion of variance explained by the first 10 components
prop_varex[1:10]
# We see the first principal component explains a variance of 10.2%, the second one explains about 6.82% and so on

# So how do we decide, how many components, should we select for scree plot
# A scree plot is used to access components or factors which explains the most of variability in the data. 
# It represents values in descending order.

# draw the scree plot
plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b")
# cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
# From the plots we see that around 28-30 principal components explains 98% variation in data...
# Beyond this more addition of components, doesnt give us any increase in coverage for variation.
# Hence using PCA we have reduced 44 components into 30

# ****************************Predictive Modelling using PCA Components ***********************************

# The first step is to prepare a new training dataset with principal components
train.data <- data.frame(Item_Outlet_Sales = train$Item_Outlet_Sales, prin_comp$x)

# We would just consider the first 30 principal components
train.data <- train.data[,1:31]

# We would now try to do the predictions using decision tree
rpart.model <- rpart(Item_Outlet_Sales ~., data = train.data, method = "anova")
rpart.model

# Now prepare the test data using the principal components
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)

# Select the first 30 components
test.data <- test.data[,1:30]

# Now make predictions on test data
predictions <- predict(rpart.model, test.data)

# This is for fun
#For fun, finally check your score of leaderboard
sample <- read.csv("SampleSubmission_TmnO39y.csv")
final.sub <- data.frame(Item_Identifier = sample$Item_Identifier, 
                          Outlet_Identifier = sample$Outlet_Identifier, Item_Outlet_Sales = rpart.prediction)
write.csv(final.sub, "pca.csv",row.names = F)

