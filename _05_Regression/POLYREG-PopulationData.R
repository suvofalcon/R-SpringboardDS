# ************************************************************************************************************************
# Polynomial Regression in R - Population Data
# 
# We will demonstrate a polynomial regression in R by creating a population dataset
# 
# ************************************************************************************************************************
options(scipen = 999)

# external libraries
library(car)
library(lmtest)

# Create the dataset
# We create a Year and Population variable
Year <- c(1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969)
Population <- c(4835, 4970, 5085, 5160, 5310, 5260, 5235, 5255, 5235, 5210, 5175)
# Now we create the data frame
PopulationData <- data.frame(Year, Population)
# we check the variable types
str(PopulationData)

# ********** Data Exploration and Data Preparation Block **************************************************** #

# For convenience, we smoothen out the data by taking the median of the year variable and setting it to zero and other values as
# negihbourhood of zero and adding a new variable for smoothened Year
PopulationData$YearX <- PopulationData$Year - median(PopulationData$Year)

# Lets visualize the relationship
plot(PopulationData$YearX, PopulationData$Population, xlab = "Year Reference to 1964", ylab = "Population", col = "blue",
     main = "Population vs Reference Year")
# Clearly this is not a linear model and we have to devise a polynomial model that better approximates our data

# ********** Regression Model **************************************************** #

# At what degree of the polynomial stop? Depends on the degree of precision that we seek. 
# The greater the degree of the polynomial, the greater the accuracy of the model, but the greater the risk of overfitting

# lets assume we will fit the model for polynomial of degree 2 and degree 3

Reg <- lm(PopulationData$Population ~ PopulationData$YearX + I(PopulationData$YearX^2) + I(PopulationData$YearX^3))
summary(Reg)

# We see with 94.6% explainability the polynomial with degree 3 is not statistically significant.. 
# Dropping the variables and re-run the model

Reg <- lm(PopulationData$Population ~ PopulationData$YearX + I(PopulationData$YearX^2))
summary(Reg) # With 92.6% explainability and all Polynomials are statistically significant

# The final model
Reg_final <- lm(PopulationData$Population ~ PopulationData$YearX + I(PopulationData$YearX^2))
summary(Reg_final)

# ********** Model Diagnostics **************************************************** #

# We will predict a set of population values using our model
predicted_Population <- predict(Reg_final)
# actual population values
actual_population <- PopulationData$Population
residual_errors <- resid(Reg_final)
diagnostic_dataFrame <- data.frame(actual_population, predicted_Population, residual_errors)

# Lets plot the actual vs predicted
plot(diagnostic_dataFrame$actual_population, type = "l", col = "blue", main = "Actual vs Predicted", ylab = "Population")
lines(diagnostic_dataFrame$predicted_Population, type = "l", col = "red")

# we will now check for multicollinearity within our IDV... For a good model, multicollinearity should be less than 10
# multicollinearity is checked using the function vif() - or variable inflation factor
vif(Reg_final)

# we will now check for heteroscedasticity
# to check heteroscedasticity, we plot, predicted vs residual
plot(diagnostic_dataFrame$predicted_Population, diagnostic_dataFrame$residual_errors, abline(0,0), main = "Check for Heteroscedasticity",
     xlab = "Predicted Population", ylab = "Residual Error", pch = 3, col = "blue")
# The variances are random and there are no strong pattern emerging from the plot
# To confirm absence of heteroscedasticity, we run the Bruesh-Pagen Test
bptest(Reg_final)
# Positive and p-value not statistically significant, confirms the absence of heteroscedasticity

# Sum of all residual errors should be zero
round(sum(Reg_final$residuals), digits = 6)

# ********** Conclusion **************************************************** #

# We see with very minimal Multicollinearity, no heteroscedasticity and a good Adjusted R-Squared of 
# 92.6% explainability, Reg_final can be accepted as a good predictive model to predict Population as a function of 
# IDVs
summary(Reg_final)
