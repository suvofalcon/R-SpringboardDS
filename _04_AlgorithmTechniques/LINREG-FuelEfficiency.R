# ******************************************************************************************************
# Regression Analysis - Predictive Algorithm using multiple linear regression to arrive an explanation
# for gallon per 100 miles variable for various cars in the fuel efficiency database
# 
# We try to model the fuel efficiency measured in GPM (gallons per 100 miles) as a function of various
# variables. We analyze GPM instead of usual EPA fuel efficiency measure (miles per gallon - MPG)
# because reciprocal transformation GPM = 100/MPG leads to approximate linear relationships between
# response and predictors
# ******************************************************************************************************
options(scipen = 999) # preventing automatic roundoffs

library(car) # using external libraries
library(robustbase)

# loading the dataset from SUVOS-TIME-CAPS
switch(Sys.info() [['sysname']],
       Windows = {fueleff <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/FuelEfficiency.csv", header = TRUE)},
       Linux   = {fueleff <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/FuelEfficiency.csv", header = TRUE)},
       Darwin  = {fueleff <- read.csv("//Volumes/Data/CodeMagic/Data Files/LedolterDatasets/DataText/FuelEfficiency.csv", header = TRUE)})

# Check the data load
head(fueleff)
dim(fueleff) # 38 rows and 8 columns
str(fueleff)
names(fueleff)
# MPG - Miles Per Gallon
# GPM - Gallon per 100 miles (Variable of Interest)
# WT - Weight of the car
# DISC - Cubic Displacement of the engine
# NC - Number of Cylinders
# HP - Horsepower of the engine
# ACC - Accelaration in seconds from 0 to 60 mph
# ET - Engine Type (1 for straight and 0 for V-type)

# ********** Data Exploration and Data Preparation Block **************************************************** #

# Check for Missing values
sum(is.na(fueleff)) # looks like there are no missing value in the dataset

# We run summary statistics on the variable of interest - GPM
summary(fueleff$GPM)
sd(fueleff$GPM)

# some graphical analysis
adjbox(fueleff$GPM, main = "Box Plot for GPM variable")
plot(fueleff$GPM, main = "Plot for GPM Variable", xlab = "Index", ylab = "GPM Variable")
# looking at the boxplot and plot graphs, we conclude there are no definite outliers on our dependent variable.

# Qualitative to Quantitative Transformations
# We do not need Qualitative to Quantitative Transformations

# Generate Derived Variables
# We are not creating any derived variables for now

# general distribution of the DV - GPM
hist(fueleff$GPM, xlab = "Gallons Per 100 Mile", col = "blue", main = "Histogram for GPM Variable", labels = TRUE)
# We see there are no values for GPM variable between 4 and 4.5 in this sample

# Looking at some relationships - variable WT
plot(fueleff$WT, fueleff$GPM, main = "GPM variable Vs Weight of the Vehicle", xlab = "Weight of the Vehicle", 
     ylab = "GPM", col = "blue", pch = 19, cex = 0.6)
# there is a clear trend that with the increase of the weight of the variable, Gallons per 100 Mile increases
# Check on the correlation
cor(fueleff$GPM, fueleff$WT)
# There is a very high positive correlation - 92.6% of the time, when there is a increase in weight, gallons per 100 mile increases
# Quick check on the distribution of the weight variable
hist(fueleff$WT, xlab = "Weight of the Car", col = "darkgreen", main = "histogram for WT Variable", labels = TRUE)
plot(fueleff$WT, main = "Plot for the Weight of the Car(s)", ylab = "Weight", pch = 3, col = "darkgreen")

# Looking at some relationships - variable DIS (engine displacement in cc)
plot(fueleff$DIS, fueleff$GPM, main = "GPM Variable vs Engine Displacement(DISC)", xlab = "Engine Displacement", 
     ylab = "GPM", col = "blue", pch = 19, cex = 0.6)
# We see the trend that with the increase of engine displacement, Gallons per 100 mile increases
# Check on correlation
cor(fueleff$GPM, fueleff$DIS)
# There is high positive correlation - 82.3% of the time, when there is an increase in engine displacement, GPM increases.
# Quick check on the distribution of the engine displacement valyes
hist(fueleff$DIS, xlab = "Engine Displacement", col = "darkgreen", main = "histogram for DIS Variable", labels = TRUE)
plot(fueleff$DIS, main = "Plot for the Engine Displacement", ylab = "Engine Displacement", pch = 3, col = "darkgreen")

# Looking at some relationships - variable NC (Number of cylinders)
# Number of cylinders is a categorical variable
GPMByClinder <- aggregate(fueleff$GPM, list(fueleff$NC), mean)
colnames(GPMByClinder) <- c("NumberOfCylinders", "GallonsPer100Mile")
barplot(tapply(fueleff$GPM, fueleff$NC, mean), col = "Navy Blue", xlab = "Number of Cylinders", ylab = "Average GPM",
        main = "Average GPM by Number of Cylinders")
# So we see as the Number of cylinders increases the Average GPM also increases
# Check on correlation
cor(fueleff$GPM, fueleff$NC)
# There is high positive correlation - 84.1% of time, when there is increase in Number of cylinders, GPM increases

# Looking at some relationships - variable HP (horsepower)
plot(fueleff$HP, fueleff$GPM, main = "GPM vs Horsepower", xlab = "Engine Horsepower", ylab = "GPM Variable", col = "blue",
     pch = 19, cex = 0.6)
# we see a trend that increase in Horsepower, increases the GPM of the vehicle
# Check on the correlation
cor(fueleff$GPM, fueleff$HP)
# There is a high positive correlation - 88.8% of the time, when there is increase in engine HP, GPM increases
# Quick check on the engine HP distribution
hist(fueleff$HP, xlab = "Engine Horsepower", col = "darkgreen", main = "histogram for Engine Horsepower Variable", labels = TRUE)
plot(fueleff$HP, main = "Plot for the Engine Horsepower", ylab = "Engine Horsepower", pch = 3, col = "darkgreen")

# Looking at some relationships - variable ACC (Accelaration)
plot(fueleff$ACC, fueleff$GPM, main = "GPM vs Accelaration", xlab = "Engine Accelaration", ylab = "GPM Variable", col = "blue",
     pch = 19, cex = 0.6)
# There doesnt seem to be much relationship between engine accelaration and GPM variable
# Quick check on the correlation
cor(fueleff$GPM, fueleff$ACC)
# The correlation is very minimal

# Looking at some relationships - variable ET (Engine Type)
# Engine Type is categorical variable
GPMByET <- aggregate(fueleff$GPM, list(fueleff$ET), mean)
colnames(GPMByET) <- c("EngineType", "GallonsPer100Mile")
barplot(tapply(fueleff$GPM, fueleff$ET, mean), col = "Navy Blue", xlab = "Engine Type", ylab = "Average GPM",
        main = "Average GPM by Engine Type")
# So we see the average GPM is higher if the engine Type is 1
# Check on correlation
cor(fueleff$GPM, fueleff$ET)
# There is positive correlation - 52% of time, if the Engine Type is straight, GPM increases

# ********** Regression Model **************************************************** #

# We work on approach, wherein we add all the variables at a time. We will drop the ones, which will hve no statistical
# significance, or multicollinearity issues

# we will remove the MPG variable because GPM is derived from MPG and presence of MPG will give multicollinearity

Reg <- lm(fueleff$GPM ~ fueleff$WT + fueleff$DIS + fueleff$NC + fueleff$HP + fueleff$ACC + fueleff$ACC + 
            fueleff$ET)
summary(Reg) # We see the Adjusted R-squared is 92.67% and ACC IDV is not statistically significant and so is WT and DISC

# Dropping the insignificant variables and re-run the iteration

Reg <- lm(fueleff$GPM ~ factor(fueleff$NC) + fueleff$HP + factor(fueleff$ET))
summary(Reg) # With Adjusted R-squared of 90.4% we see all IDVs are statistically significant

# Final Model
# Based on the iterations so far, Number of Cylinders, Horsepower and Engine Type seems to be the 
# IDVs contributing maximum to the changes in the GPM variable

Reg_final <- lm(fueleff$GPM ~ factor(fueleff$NC) + fueleff$HP + factor(fueleff$ET))
summary(Reg_final)

# ********** Model Diagnostics **************************************************** #

# we will predict a set of GPM values based on our finalized model
predicted_gpm <- predict(Reg_final)
# The actual GPM values in the variable
actual_gpm <- fueleff$GPM
# We will now check the errors or the residual values
residual_error_gpm <- resid(Reg_final)
# Create the diagnostic data frame
diagnostic_dataFrame <- data.frame(actual_gpm, predicted_gpm, residual_error_gpm)
colnames(diagnostic_dataFrame) <- c("Actual-GPM", "Predicted-GPM", "Residual-Errors")

# We will plot the actual vs predicted
plot(diagnostic_dataFrame$`Actual-GPM`, type = "l", col = "blue", main = "Actual vs Predicted GPM", ylab = "Gallons Per 100 Mile")
lines(diagnostic_dataFrame$`Predicted-GPM`, type = "l", col = "red")
legend(x = "topright", c("Actual", "Predicted"), lty = c(1, 1), lwd = c(1, 1), col = c("blue", "red"))

# we will now check for multicollinearity within our IDV... For a good model, multicollinearity should be less than 10
# multicollinearity is checked using the function vif() - or variable inflation factor
vif(Reg_final)

# we will now check for heteroscedasticity
# to check heteroscedasticity, we plot, predicted vs residual
plot(predicted_gpm, residual_error_gpm, abline(0, 0), main = "Check for Heteroscedasticity", xlab = "Residual Error", ylab = "Predicted GPM", pch = 3, col = "blue")
# looking at the above plot, we cannot see any heterscedasticity. The variances are at random and there is no pattern emerging from the plot

# The sum of all residual errors should be zero
round(sum(Reg_final$residuals), digits = 6)

# ********** Conclusion **************************************************** #

# We see with very minimal Multicollinearity, no heteroscedasticity and a good Adjusted R-Squared of 90.4% explainability, Reg_final can be accepted as a good
# predictive model to predict GPM as a function of IDVs
summary(Reg_final)
