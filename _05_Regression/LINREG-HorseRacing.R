# ******************************************************************************************************
# THIS CODE IS INCOMPLETE - MAY BE REGRESSION IS NOT THE RIGHT APPROACH -- WOULD REVISIT THIS LATER

# Regression Analysis on Horse Racing Dataset... to find the indepedent variables affecting our variable
# of interest - handle
# A way to predict the handle money, depending on some paramters known for the race
#
# Data Used - "horse_racing-class10.csv"
# ******************************************************************************************************
options (scipen = 999)

# Use of external libraries
library(car)
library(robustbase)

# Lets load the dataset
switch(Sys.info() [['sysname']],
       Windows = {horseracing <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/horse_racing-class10.csv", header = TRUE, colClasses = c("factor","factor","factor","numeric","factor","factor","numeric","character","factor","factor","numeric","factor","factor","numeric","numeric","numeric","character","factor","numeric"))},
       Linux   = {horseracing <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/horse_racing-class10.csv", header = TRUE, colClasses = c("factor","factor","factor","numeric","factor","factor","numeric","character","factor","factor","numeric","factor","factor","numeric","numeric","numeric","character","factor","numeric"))},
       Darwin  = {horseracing <- read.csv("//Volumes/Data/CodeMagic/Data Files/horse_racing-class10.csv", header = TRUE, colClasses = c("factor","factor","factor","numeric","factor","factor","numeric","character","factor","factor","numeric","factor","factor","numeric","numeric","numeric","character","factor","numeric"))})

# Check the data load
head(horseracing)
str(horseracing) # There are 4222 observations and 19 variables

# some change in the dataset
horseracing$distance_unit <- as.factor(horseracing$distance_unit)
horseracing$track_type <- as.factor(horseracing$track_type)

# ***********************************************************************************************
# Data Exploration and Data Preparation Block 
# ***********************************************************************************************

# Check for missing values
sum(is.na(horseracing)) # There are no missing values in the dataset

# We will generate additional variables for Month and Year, just to analyze how handle money is distributed
# by months and by year
race_date_string <- as.character(horseracing$race_date)
race_month <- months.Date(as.Date(race_date_string, format = "%d-%B-%y"))
race_year <- format((as.Date(race_date_string, format = "%d-%B-%y")), "%Y")
# add the columns
horseracing <- data.frame(horseracing, race_month = race_month, race_year = race_year)
str(horseracing)

# we will generate a variable claim_price_range as
horseracing$claim_price_range <- horseracing$maximum_claim_price - horseracing$minimum_claim_price

# Generate Summary Statistics
summary(horseracing)

# We will take a look at the target variable closely
hist(horseracing$handle, col = "darkgreen", xlab = "Handle Money", ylab = "Frequencies", main = "Histogram for Handle Money", labels = TRUE)
adjbox(horseracing$handle)
plot(horseracing$handle, xlab = "Frequencies", ylab = "Handle Money", main = "Scatter Plot for Handle Money", pch = 19, col = "blue")
# Looking at the above plots, we would take a look at the observations where handle money is > 150,0000
nrow(subset.data.frame(horseracing, horseracing$handle > 1500000))
# There are 90 observations out of 4222, which is responsible for skewing the distribution of handle money
# a detailed look at those observations
highValueHorseracing <- subset.data.frame(horseracing, horseracing$handle > 1500000)
# Looking at the observations, it is clear that these are high value race(s). 
# They are skewing the distribution, but looking at the dataset, there is no reason to believe that they
# are outliers

# Tabulations and Cross Tabulations

# Handle Money by Track ID
tab.ds <- aggregate(horseracing$handle, list(horseracing$track_id), mean)
colnames(tab.ds) <- c("TrackID", "Handle Money")
tab.ds
tab<- tapply(horseracing$handle, horseracing$track_id, mean)
barplot(sort(tab, decreasing = TRUE), col = c("blue", "green"), ylab = "Average Handle", xlab = "Track ID", main = "Average Handle by Track ID")

# Handle Money by Race month
tab.ds <- aggregate(horseracing$handle, list(horseracing$race_month), mean)
colnames(tab.ds) <- c("Race Month", "Handle Money")
tab.ds
tab <- tapply(horseracing$handle, horseracing$race_month, mean)
barplot(sort(tab, decreasing = TRUE), col = c("violet","cyan","blue","green","yellow","orange","red"), ylab = "Average Handle", xlab = "Race Month", main = "Average Handle Money by Month")

# Handle Money by Race year
tab.ds <- aggregate(horseracing$handle, list(horseracing$race_year), mean)
colnames(tab.ds) <- c("Race Year", "Handle Money")
tab.ds
tab <- tapply(horseracing$handle, horseracing$race_year, mean)
barplot(sort(tab, decreasing = TRUE), col = c("violet","cyan","blue","green","yellow","orange","red"), ylab = "Average Handle", xlab = "Race Year", main = "Average Handle Money by Year")

# Handle Money by Race Type
tab.ds <- aggregate(horseracing$handle, list(horseracing$race_type), mean)
colnames(tab.ds) <- c("Race Type", "Handle Money")
tab.ds
tab <- tapply(horseracing$handle, horseracing$race_type, mean)
barplot(sort(tab, decreasing = TRUE), col = c("violet","cyan","blue","green","yellow","orange","red"), ylab = "Average Handle", xlab = "Race Type", main = "Average Handle Money by Race Type")

# Handle Money by Surface
tab.ds <- aggregate(horseracing$handle, list(horseracing$surface), mean)
colnames(tab.ds) <- c("Surface", "Handle Money")
tab.ds
tab <- tapply(horseracing$handle, horseracing$surface, mean)
barplot(sort(tab, decreasing = TRUE), col = c("violet","cyan","blue","green","yellow","orange","red"), ylab = "Average Handle", xlab = "Race Surface", main = "Average Handle Money by Race Surface")

# Handle Money by Course Type
tab.ds <- aggregate(horseracing$handle, list(horseracing$course_type), mean)
colnames(tab.ds) <- c("Course Type", "Handle Money")
tab.ds
tab <- tapply(horseracing$handle, horseracing$course_type, mean)
barplot(sort(tab, decreasing = TRUE), col = c("violet","cyan","blue","green","yellow","orange","red"), ylab = "Average Handle", xlab = "Course Type", main = "Average Handle Money by Course Type")

# Handle Money by Track Condition
tab.ds <- aggregate(horseracing$handle, list(horseracing$track_condition), mean)
colnames(tab.ds) <- c("Track Condition", "Handle Money")
tab.ds
tab <- tapply(horseracing$handle, horseracing$track_condition, mean)
barplot(sort(tab, decreasing = TRUE), col = c("violet","cyan","blue","green","yellow","orange","red"), ylab = "Average Handle", xlab = "Track Condition", main = "Average Handle Money by Track Condition")

# Handle Money by Weather
tab.ds <- aggregate(horseracing$handle, list(horseracing$weather), mean)
colnames(tab.ds) <- c("Weather", "Handle Money")
tab.ds
tab <- tapply(horseracing$handle, horseracing$weather, mean)
barplot(sort(tab, decreasing = TRUE), col = c("violet","cyan","blue","green","yellow","orange","red"), ylab = "Average Handle", xlab = "Weather", main = "Average Handle Money by Weather")

# Handle Money by State
tab.ds <- aggregate(horseracing$handle, list(horseracing$state), mean)
colnames(tab.ds) <- c("State", "Handle Money")
tab.ds
tab <- tapply(horseracing$handle, horseracing$state, mean)
barplot(sort(tab, decreasing = TRUE), col = c("violet","cyan","blue","green","yellow","orange","red"), ylab = "Average Handle", xlab = "State", main = "Average Handle Money by State")

# Handle Money by Race Number and Track ID
tab.ds <- aggregate(horseracing$handle, list(horseracing$race_number, horseracing$track_id), mean)
colnames(tab.ds) <- c("Race Number","Track ID", "Handle Money")
tab.ds
plot(tab.ds$`Race Number`[tab.ds$`Track ID` == "AP"], tab.ds$`Handle Money`[tab.ds$`Track ID` == "AP"], type = "b", lwd = 2, col = "blue", xlab = "Race Number", ylab = "Handle Money", main = "Average Handle Money with Race Number and Track ID")
lines(tab.ds$`Race Number`[tab.ds$`Track ID` == "CD"], tab.ds$`Handle Money`[tab.ds$`Track ID` == "CD"], type = "b", lwd = 2, col = "red")
legend(x = "topleft", c("AP","CD"), lty = c(1,1), lwd = c(2.5,2.5), col = c("blue", "red"))

# Handle Money by Race Number and Race Type
tab.ds <- aggregate(horseracing$handle, list(horseracing$race_number, horseracing$race_type), mean)
colnames(tab.ds) <- c("Race Number", "Race Type", "Handle Money")
tab.ds
coplot(tab.ds$`Handle Money` ~ tab.ds$`Race Number` | tab.ds$`Race Type`, type = "o", lwd = 1, col = "dark green", xlab = "Race Number", ylab = "Handle Money", main = "Handle Money by Race Number by Race Type")

# ***********************************************************************************************
# Creation of Samples
# ***********************************************************************************************

# We will create two samples - Training and Validation
# We will model it against the training dataset and verify it against the validation dataset

# Training data set will have 70% of the rows of the main dataset selected random
# We will set the seed to make the partitions reproducible
set.seed(1)
samplingIndex <- sort(sample(nrow(horseracing), nrow(horseracing)*0.7))
horseracing.train <- horseracing[samplingIndex,]
horseracing.test <- horseracing[-samplingIndex,]
# quickly verify the number of rows in training and test dataset
nrow(horseracing.train)
nrow(horseracing.test)

# We will look at the target variable distribution in training and test dataset
# We will look at both of these in the same plotting area
par(mfrow = c(1, 2))
hist(horseracing.train$handle, labels = TRUE, xlab = "Handle Money", ylab = "Frequency", col = "blue", main = "Handle Money-Train")
hist(horseracing.test$handle, labels = TRUE, xlab = "Handle Money", ylab = "Frequency", col = "green", main = "Handle Money-Test")

# Looking at the plots, we see that the sampling has happened correctly

# ***********************************************************************************************
# Modelling Block
# ***********************************************************************************************

# We will use the approach of 1 IDV at a time to come up with the final model by iterations
str(horseracing.train)

# Iteration 1 
#------------------------------------------------
# With track_id
Reg_model <- lm(horseracing.train$handle ~ horseracing.train$track_id)
summary(Reg_model) # Track_id is statistically significant..

# Iteration 2
#------------------------------------------------
# With race_number
Reg_model <- lm(horseracing.train$handle ~ horseracing.train$track_id + horseracing.train$race_number)
summary(Reg_model) # Race_number is statistically significant

# Iteration 3
#------------------------------------------------
# With distance_id
Reg_model <- lm(horseracing.train$handle ~ horseracing.train$track_id + horseracing.train$race_number + horseracing.train$distance_id)
summary(Reg_model) # distance_id is not statistically significant
# With surface and course_type
Reg_model <- lm(horseracing.train$handle ~ horseracing.train$track_id + horseracing.train$race_number + horseracing.train$surface + horseracing.train$course_type)
summary(Reg_model) # dropping course_type as it is not statistically significant and because of singularities
Reg_model <- lm(handle ~ track_id + race_number + surface, data = horseracing.train)
summary(Reg_model)
# With Prize
Reg_model <- lm(handle ~ track_id + race_number + surface + prize, data = horseracing.train)
summary(Reg_model) # Prize is statistically significant
# With Claim Price variables
Reg_model <- lm(handle ~ track_id + race_number + surface + prize + claim_price_range, data = horseracing.train)
summary(Reg_model) # We see this to be statistically significant

# Iteration 4
#------------------------------------------------
# With state and weather
Reg_model <- lm(handle ~ track_id + race_number + surface + prize + claim_price_range + state + weather, data = horseracing.train)
summary(Reg_model) # We see weather is not significant except Weather "o". Dropping Weather for now and also state for singularities
# With distance_id
Reg_model <- lm(handle ~ track_id + race_number + surface + prize + claim_price_range + distance_id, data = horseracing.train)
summary(Reg_model) # we will keep distance_id for now

# with number of runners
Reg_model <- lm(handle ~ track_id + race_number + surface + prize + claim_price_range + distance_id + number_of_runners , data = horseracing.train)
summary(Reg_model) # we see number of runners is statistically significant and also we see per significance, we will have to
# drop the variables distance_id and claim_price_range and surface
Reg_model <- lm(handle ~ track_id + race_number + prize + number_of_runners, data = horseracing.train)
summary(Reg_model)

# with track_condition
Reg_model <- lm(handle ~ track_id + race_number + prize + number_of_runners + track_condition, data = horseracing.train)
summary(Reg_model) # dropping track condition

# With race_type
Reg_model <- lm(handle ~ track_id + race_number + prize + number_of_runners + race_month , data = horseracing.train)
summary(Reg_model)

# We will consider the above as the final model
Reg_final <- lm(handle ~ track_id + race_number + prize + number_of_runners + race_month, data = horseracing.train)
summary(Reg_final)

# ***********************************************************************************************
# Model Diagnostics
# ***********************************************************************************************


# ***********************************************************************************************
# Conclusions
# ***********************************************************************************************