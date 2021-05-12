# ******************************************************************************************************
# Regression Analysis - Predictive Algorithm using multiple linear regression to arrive an explanation
# for the fatalities variable as a function of various perdictors

# Data Used - highway_deaths.csv

# This is done in the jigsaw class and may not be exhaustive and explanatory as the others
# ******************************************************************************************************

# we will load the highway deaths data set
# we will load the dataset (this is from SUVOS-TIME-CAPS)
switch(Sys.info() [['sysname']],
       Windows = {hdeaths <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/highway_deaths.csv", header = TRUE)},
       Linux   = {hdeaths <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/highway_deaths.csv", header = TRUE)},
       Darwin  = {hdeaths <- read.csv("//Volumes/Data/CodeMagic/Data Files/highway_deaths.csv", header = TRUE)})

# Check the data load
str(hdeaths)
head(hdeaths)
# MPH 65 is a factor variable -- it says whether the speed limit has been enforced(1) or it has not been enforced (0)
hdeaths$MPH65 <- as.factor(hdeaths$MPH65)
names(hdeaths) # shows the column names of all variables

# Now we if hypothesesize that as the drivers increases (number of licensed drivers), the fatalities also increases. So we run a linear regression
# which is also called as simple linear regression - Effect of one independent variable on the dependent variable
# fatalities is our variable of interest - dependent variable 

# trying to predict fatalities as a function of drivers
Reg_dataset <- lm(data = hdeaths, fatalities ~ drivers)
Reg_dataset
summary(Reg_dataset)

# how do we read it.. 
# Residuals are errors
# we get the intercept and drivers. Intercept 0.226021 is positive, which means as the number of drivers increases, fatalities also increases
# The p value <2e-16 is also less than 0.05 (5%^ significance level) - this shows that the relationship between drivers and fatalities is not due to random chance
# but is of statistical significance

# Multiple R-squared is also 0.9399 (94%) - which means 94% of variability in our dependent variable fatalities can be explained by just one variable drivers

# The drivers 0.226021 is interpreted as - since drivers is on a scale of 1000's in the data set, we can conclude that for every 10,0000 increase in number of
# drivers, there is a 2.26021 increase in number of fatalities

# Lets move on to a case, where we expect multiple variables to have an impact on a particular dependent variable

# we will run multiple linear regression as

Reg_multi_dataset <- lm(data = hdeaths, fatalities ~ drivers + congest + MPH65)
Reg_multi_dataset
summary(Reg_multi_dataset)

#if we check the estimate and P values we see drivers and MPH651 (1 - because the factor value 1, as it says the MPH65 speed limit has been enforced)
# With a 5% significance level, the p values are very significant for drivers and MPH65
# however the variable congest is not significant and we can hence drop that variable from the linear model and go for one more level of iteration

Reg_final <- lm(data = hdeaths, fatalities ~ drivers + MPH65)
Reg_final
summary(Reg_final)

# we see the R squared has not changed much and this model with 2 independent variable is explaining 95% of variability in our depdenent variable - fatalities

# so how do we interpret the results, 
# for every 100,000 increase in drivers (since drivers is expressed in thousands) our fatalities will increase by 22.6
# When Speed limit is enforced the number of fatalities is 251.1 more than when speed limit is not enforced. When speed limit is enforced, we can expect
# on an average 251 more fatalities as opposed to when speed limit is not enforced.
# (does this make sense? Intuitively, every individual thinks the other is following the speed limit and hence driving fast, when speed limit is not
# enforced, everybody is more careful)

# broader sense - what is to assess
#   Signs on each coefficient - are they expected?
#   p-values and significance, - are they all significant at the 5% or the 10% significance level

# now we will check the heteroscedasticity

# we will now get the predicted values in a data frame
predicted_hdeaths <- predict(Reg_final)
predicted_hdeaths 
# the above calculates the value of fatalities based on the equation formed from the final model - Reg_final
# Reg_final equation is = -1.372e+02 + 2.264e-01 * drivers + 2.511e+02 * MPH65

# to get the residual values (errors) - we use
residual_hdeaths <- resid(Reg_final)
residual_hdeaths
# the above will give the errors for the correspoding predictions in prediction_hdeaths

# next we plot the residual vs predicted to check for the heteroscedasticity
plot(predicted_hdeaths, residual_hdeaths, abline(0,0))
# looking at the above plot, we cannot see any heterscedasticity. The variances are constant and
# there is no pattern emerging from the plot

# the sum of all residual errors should be zero
round(sum(Reg_final$residuals), digits = 4)


# next we would want to check for multicollinearity, we will use the package 'car'
library(car)
vif(Reg_final)
# looking at the values, they are well less than 5 and we can assume that there is no multicollinearity problem.

# So there is no heteroscedasticy, no multicollinearity, RSquared value is also high, we should be good to go
# with this model

# the final thing, we will do is to plot between the actuals and predicted values

# getting the actuals
fatalities <- hdeaths$fatalities
# we will plot the fatalities actual and predicted
range(fatalities)
plot(fatalities, col = "blue", type = "l", ylim = c(0, 5000)) # actual plot
lines(predicted_hdeaths, col = "red", type = "l")

# if we see the plot, we would see that the actual and predicted are more or less overlapping and hence we can 
# conclude that our model is more or less doing a good job in predicting fatalities variable.

