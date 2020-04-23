# ******************************************************************************************************
# Regression Analysis - Predictive Algorithm using multiple linear regression to arrive an explanation
# for the DEXfat variable as a function of various perdictors

# Data Used - bodyfat
# Package - TH.data
# ******************************************************************************************************

# Use of external libraries
library(TH.data)
library(car)

# Now we will use the inbuilt dataset bodyfat
data(bodyfat)
head(bodyfat)
str(bodyfat)

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

# ********** Regression Model **************************************************** #

# We work on approach, wherein we add all the variables at a time. We will drop the ones, which will hve no statistical
# significance, or multicollinearity issues
head(bodyfat)

Reg <- lm(bodyfat$DEXfat ~ bodyfat$age + bodyfat$waistcirc + bodyfat$hipcirc + bodyfat$elbowbreadth + bodyfat$kneebreadth + bodyfat$anthro3a + bodyfat$anthro3b + bodyfat$anthro3c + bodyfat$anthro4)
summary(Reg)

# We will drop the statistically insignificant IDV's and re-run the iterations
Reg <- lm(bodyfat$DEXfat ~ bodyfat$waistcirc + bodyfat$hipcirc + bodyfat$anthro3a)
summary(Reg)

# With all the above IDV are statistically significant even at 5% significance level and Adjusted R-squared 90.4%
# 90.4% of the data can be explained alone with these IDV's, we will treat this as the final model

# Final Model
Reg_final <- lm(bodyfat$DEXfat ~ bodyfat$waistcirc + bodyfat$hipcirc + bodyfat$anthro3a)

# ********** Model Diagnostics **************************************************** #

# we are going to predict a set of values (DEXfat) using our model
predicted_values <- predict(Reg_final)
# The actual values
actual_values <- bodyfat$DEXfat
# we will check the errors or the residual values
residual_values <- resid(Reg_final)
# create the diagnostic data frame
diagnostic_dataFrame <- data.frame(actual_values, predicted_values, residual_values)
head(diagnostic_dataFrame)

# We will plot the actual vs predicted
plot(diagnostic_dataFrame$actual_values, type = "l", col = "blue", main = "Actual vs Predicted", xlab = "DEXfat")
lines(diagnostic_dataFrame$predicted_values, type = "l", col = "red")
legend(1,65,legend = c("Actual","Predicted"),col = c("Blue","Red"),lty = 1)

# we will now check for multicollinearity within our IDV... For a good model, multicollinearity should be less than 10
# multicollinearity is checked using the function vif() - or variable inflation factor
vif(Reg_final)

# we will now check for heteroscedasticity
# to check heteroscedasticity, we plot, predicted vs residual
plot(predicted_values, residual_values, abline(0, 0), main = "Check for Heteroscedasticity", xlab = "Predicted DEXfat", ylab = "Residual Error", col = "blue", cex = 0.75, pch = 19)
# looking at the above plot, we cannot see any heterscedasticity. The variances are at random and there is no pattern emerging from the plot

# The sum of all residual errors should be zero
round(sum(Reg_final$residuals), digits = 4)

# ********** Conclusion **************************************************** #
summary(Reg_final)
# We see with very minimal multicollinearity, no heteroscedasticity and an explainability of 90.4%, Reg_final can be 
# accepted as a good model for predicting DEXfat as a function of the IDV's

