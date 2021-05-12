# ************************************************************************************************************************
# Polynomial Regression in R - NOx exhaust emissions data
# 
# We will demonstrate a polynomial regression in R by using the NOx exhaust emissions
# The NOx exhaust emissions depend on two predictor variables, the fuel-air equivalence ratio
# (E) and the compression ratio (R) of the engine.
# ************************************************************************************************************************
options(scipen = 999)

# external libraries
library(car)
library(lmtest)
library(ggplot2)

# load the dataset from (SUVOS-TIME-CAPS)
switch(Sys.info() [['sysname']],
       Windows = {ethanol <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/ethanol.csv", header = TRUE)},
       Linux   = {ethanol <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/ethanol.csv", header = TRUE)},
       Darwin  = {ethanol <- read.csv("//Volumes/Data/CodeMagic/Data Files/LedolterDatasets/DataText/ethanol.csv", header = TRUE)})

# check the data load
dim(ethanol)
head(ethanol)
str(ethanol)

# ********** Data Exploration and Data Preparation Block **************************************************** #

# check for missing values
sum(is.na(ethanol)) # there are no missing values

# Lets analyse the Variable of interest
summary(ethanol$NOx)
sd(ethanol$NOx)
# visualise the variable
boxplot(ethanol$NOx) # There seems to be no outliers
hist(ethanol$NOx, main = "Histogram for NOx", xlab = "Exhaust Emission (NOx)", col = "Blue", labels = TRUE)
plot(ethanol$NOx, main = "Scatter Plot for NOx", ylab = "Exhaust Emission (NOx)", col = "blue", pch = 13)
# There is no pattern emerging out and data seems to be everywhere

# Lets start looking at some possible relationship
# With variable - CompRatio
plot(ethanol$CompRatio, ethanol$NOx, main = "Exhaust Emission vs Engine Comperssion Ratio", pch = 19, cex = 0.6, col = "blue",
     xlab = "Comp Ratio", ylab = "Exhaust Emission")
# check on the correlation
cor(ethanol$NOx, ethanol$CompRatio) # seems to be no correlation at all...

# Relationship with variable - EquivRatio
ggplot(ethanol, aes(x = EquivRatio, y = NOx)) + geom_point(col = "blue", pch = 19) + xlab("Air Equivalence Ratio") + 
  ylab("Exhaust Emission") + ggtitle("Exhaust Emission vs Air Equivalence Ratio")
# suggests a strong non linear relationship for which we need to approximate using a polynomial regression model

# ********** Regression Model **************************************************** #

# We work on approach, wherein we add all the variables at a time. We will drop the ones, which will hve no statistical
# significance, or multicollinearity issues

# lets assume we will fit the model for polynomial of degree 2 and degree 3

Reg <- lm(ethanol$NOx ~ ethanol$CompRatio + ethanol$EquivRatio + I(ethanol$EquivRatio^2) + I(ethanol$EquivRatio^3))
summary(Reg)

# The polynomial of degree 3 is not statistically significant and hence dropping the degree 3 and re-run the model
Reg <- lm(ethanol$NOx ~ ethanol$CompRatio + ethanol$EquivRatio + I(ethanol$EquivRatio^2))
summary(Reg) # With adjusted R-Squared of 81.74%, we see all IDV are statistically significant

# We finalize the below model
Reg_final <- lm(ethanol$NOx ~ ethanol$CompRatio + ethanol$EquivRatio + I(ethanol$EquivRatio^2))
summary(Reg_final)

# ********** Model Diagnostics **************************************************** #

# We will predict a set of exhaust emission values from our model
predicted_emissions <- predict(Reg_final)
# actual emission values
actual_emissions <- ethanol$NOx
# residual errors
residual_emissions <- resid(Reg_final)
# create the diagnostic data frame
diagnostic_dataFrame <- data.frame(actual_emissions, predicted_emissions, residual_emissions)

# Lets plot the actual vs Predicted
plot(diagnostic_dataFrame$actual_emissions, type = "l", col = "blue", main = "Actual vs Predicted", ylab = "Exhaust Emissions")
lines(diagnostic_dataFrame$predicted_emissions, type = "l", col = "red")

# we will now check for multicollinearity within our IDV... For a good model, multicollinearity should be less than 10
# multicollinearity is checked using the function vif() - or variable inflation factor
vif(Reg_final)
# We can ignore high collinearity between EquivRatio and EquivRatio^2 because they are essentially the same variable.

# we will now check for heteroscedasticity
# to check heteroscedasticity, we plot, predicted vs residual
plot(diagnostic_dataFrame$predicted_emissions, diagnostic_dataFrame$residual_emissions, abline(0,0), main = "Check for Heteroscedasticity",
     xlab = "Predicted Emuissions", ylab = "Residual Error", pch = 3, col = "blue")
# We see some indication of heteroscedasticity
# Confirming using Breush-Pagan Test
bptest(Reg_final)
# With positive and high p-value we conclude there is no presence of heteroscedasticity

# Sum of all residual errors should be zero
round(sum(Reg_final$residuals), digits = 6)

# ********** Conclusion **************************************************** #

# With no heteroscedasticity and a good Adjusted R-Squared of 
# 81.74% explainability, Reg_final can be accepted as a good predictive model to predict Exhaust Emission as a function of 
# IDVs
summary(Reg_final)
