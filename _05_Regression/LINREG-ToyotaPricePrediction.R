# ******************************************************************************************************
# Regression Analysis - The dataset includes sale prices and vehicle characteristics of 1436
# used Toyota Corollas.
#
# The Objective here is to predict the sale price of a used automobile
# ******************************************************************************************************
options (scipen = 999)

# Use of external libraries
library(robustbase)
library(car)
library(lmtest)

# load the dataset from (SUVOS-TIME-CAPS)
switch(Sys.info() [['sysname']],
       Windows = {toyota <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/ToyotaCorolla.csv", header = TRUE)},
       Linux   = {toyota <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/ToyotaCorolla.csv", header = TRUE)},
       Darwin  = {toyota <- read.csv("//Volumes/Data/CodeMagic/Data Files/LedolterDatasets/DataText/ToyotaCorolla.csv", header = TRUE)})

# Check the dataload
dim(toyota)
head(toyota)
str(toyota)

# ********** Data Exploration and Data Preparation Block **************************************************** #

# Check for missing values
sum(is.na(toyota)) # looks like there are no missing values
#we will obtain summary statistics and do some basic data sanity checks, -ve price, impossible values etc
nrow(subset(toyota, toyota$Price <= 0)) # -ve price
nrow(subset(toyota, toyota$KM <= 0)) # 0 or less than 0 mileage

# Lets analyse the KM variable
summary(toyota$KM)
# Looks like there is a Minimum KM reading of 1... lets examine those records
subset(toyota, toyota$KM == 1)
# so we see cars which are more than 1 month old and has run only 1 KM... This strongly seems to be some
# kind of erroneous data - we will remove all these rows
toyota <- toyota[!(toyota$KM == 1),]

# Lets analyse the variable of interest
summary(toyota$Price)
sd(toyota$Price)
plot(toyota$Price, main = "Scatter Plot for Price", xlab = "Index", pch = 11)
boxplot(toyota$Price)
# We will examine the prices greater than 30,000 Euros in detail
subset(toyota, toyota$Price >= 30000)
# They seem to be cars of the same type.... cannot be considered as outliers
# Lets visualize the data further
hist(toyota$Price, xlab = "Price of the Cars", col = "blue", main = "Histogram for the Price Variable", labels = TRUE)
# its a highly positvely skewed distribution

# Looking at some relationships - variable Age
plot(toyota$Age, toyota$Price, main = "Price Variable vs Age", xlab = "Age of the Vehicle", ylab = "Price",
     col = "blue", pch = 19, cex = 0.6)
# There is a clear trend that as the age of the vehicle increase price falls down
# check on correlation
cor(toyota$Price, toyota$Age)
# High negative correlation - 87.7% of the times, price of the vehicle decreases with increase in Age of the vehicle

# Looking at some relationships - variable KM
plot(toyota$KM, toyota$Price, main = "Price Variable vs KM", xlab = "KM of the Vehicle", ylab = "Price",
     col = "blue", pch = 19, cex = 0.6)
# There is a trend that as the KM run of the vehicle increase, price steeply falls down
# however beyond some point the price remains constant even if KM increases
# Check on the correlation
cor(toyota$Price, toyota$KM)
# There is a negative correlation - 56.8% of the time, price of the vehicle decreases with increase in KM run

# Looking at some relationships - variable Fuel Type
# Variable Fuel Type is a factor variable - check the levels
levels(toyota$FuelType)
# lets aggregate the price by Fuel Type
toyotaPriceByFuelType <- aggregate(toyota$Price, list(toyota$FuelType), mean, na.rm = TRUE)
colnames(toyotaPriceByFuelType) <- c("FuelType", "AveragePrice")
# Lets visualize
barplot(tapply(toyota$Price, toyota$FuelType, mean), col = "DarkGreen", xlab = "Fuel Type", ylab = "Average Price",
        main = "Average Price by Fuel Type")
# With the fluctuations in average price by fuel type we can conclude there is some impact on the price 
# based on the type of fuel used in the car

# Looking at some relationships - variable HP
plot(toyota$HP, toyota$Price, main = "Price Variable vs HP", xlab = "HP of the Engine", ylab = "Price",
     col = "blue", pch = 19, cex = 0.6)
# there is a very small indication that increase in engine HP rating causes increase in price
# check on the correlation
cor(toyota$Price, toyota$HP)
# Shows that 31.5% of the time, when engine HP rating increases, Price of the car increases

# Looking at some relationships - variable MetColor
# We will treat MetColor variable as a factor variable with 1 is the car has metallic color
# Average Price by presence of metallic color
toyotaPriceByMetColor <- aggregate(toyota$Price, list(toyota$MetColor), mean, na.rm = TRUE)
colnames(toyotaPriceByMetColor) <- c("MetallicColor", "AveragePrice")
# Lets visualize
barplot(tapply(toyota$Price, toyota$MetColor, mean), col = "DarkGreen", xlab = "Metallic Color", ylab = "Average Price",
        main = "Average Price by Metallic Color")
# There is a small difference in price by the presence of metallic color in the car
# This may not be a significant contributing factor in impacting price

# Looking at some relationships - variable Automatic (Presence of Automatic Transmission)
# We will treat Automatic variable as a factor variable with 1 is the car has Automatic Transmission
# Average Price by presence of Automatic
toyotaPriceByAuto <- aggregate(toyota$Price, list(toyota$Automatic), mean, na.rm = TRUE)
colnames(toyotaPriceByAuto) <- c("Automatic", "AveragePrice")
# Lets visualize
barplot(tapply(toyota$Price, toyota$Automatic, mean), col = "DarkGreen", xlab = "Automatic", ylab = "Average Price",
        main = "Average Price by Automatic")
# There is a small difference in price by the presence of Automatic Transmission in the car
# This may not be a significant contributing factor in impacting price

# Looking at some relationships - variable CC (Engine Displacement)
plot(toyota$CC, toyota$Price, main = "Price Variable vs CC", xlab = "Engine CC", ylab = "Price",
     col = "blue", pch = 19, cex = 0.6)
# there is a very small indication that increase in engine CC rating causes increase in price
# check on the correlation
cor(toyota$Price, toyota$CC)
# Shows that 16.2% of the time, when engine CC rating increases, Price of the car increases
# may not have any impact on the model

# Looking at some relationships - variable Doors
plot(toyota$Doors, toyota$Price, main = "Price Variable vs Doors", xlab = "Engine CC", ylab = "Price",
     col = "blue", pch = 19, cex = 0.6)
# Visualize the average price by number of doors
barplot(tapply(toyota$Price, toyota$Doors, mean, na.rm = TRUE), col = "DarkGreen", xlab = "Number of Doors",
        ylab = "Average Price", main = "Average Price by Doors")
# there is a very small indication that increase in price has any impact on number of doors
# check on the correlation
cor(toyota$Price, toyota$Doors)
# Shows that 18.4% of the time, when number of doors increases, Price of the car increases
# may not have any impact on the model

# Looking at some relationships - variable Weight
plot(toyota$Weight, toyota$Price, main = "Price Variable vs Weight", xlab = "Weight", ylab = "Price",
     col = "blue", pch = 19, cex = 0.6)
# There is a strong indication that increase in weight causes increase in price
# Check on the correlation
cor(toyota$Price, toyota$Weight)
# 57.6% of the times, when the weight of the car increases the price increases as well

# We will do some cross tabulations ----------------------- #


# ********** Regression Model **************************************************** #

# We work on approach, wherein we add all the variables at a time. We will drop the ones, which will hve no statistical
# significance, or multicollinearity issues

Reg <- lm(Price ~ ., data = toyota)
summary(Reg)

# We will drop the variable MetColor and Doors because of no statistical significance and re-run the model

Reg <- lm(toyota$Price ~ toyota$Age + toyota$KM + factor(toyota$FuelType) + toyota$HP + factor(toyota$Automatic)
          + toyota$CC + toyota$Weight)
summary(Reg) # With 86.92% explainability....

# We will drop the Automatic variable as it is statistically significant at 5% as compared to other variables
# which have very high statistical significance
Reg <- lm(toyota$Price ~ toyota$Age + toyota$KM + factor(toyota$FuelType) + toyota$HP + toyota$CC + toyota$Weight)
summary(Reg) # With 86.89% explainability....

# Final Model
# Based on the iterations so far, we will finalize the model with 86.89% explainability
Reg_final <- lm(toyota$Price ~ toyota$Age + toyota$KM + factor(toyota$FuelType) + toyota$HP 
                + toyota$CC + toyota$Weight)
summary(Reg_final)

# ********** Model Diagnostics and Adjustments **************************************************** #

# we will now check for multicollinearity within our IDV... For a good model, multicollinearity should be less than 10
# multicollinearity is checked using the function vif() - or variable inflation factor
vif(Reg_final)

# We can ignore the high collinearity of the fuel type variable, because the factor has three levels and the 
# reference level is least in numbers and hence the other two shows collinearity ..

# But apparently there is an obvious correlation between HP and CC of an engine... Hence we will drop the CC
# variable and re-run the regression

Reg_final_2 <- lm(toyota$Price ~ toyota$Age + toyota$KM + factor(toyota$FuelType) + toyota$HP 
                  + toyota$Weight)
summary(Reg_final_2) # There is a negligible drop in explainability (adjusted r-squared) - 86.38%

# Looking at the p-value of Diesel and Petrol Fuel type, we will use an indicator variable for Petrol and
# re-run the iteration
toyota$PetrolFuel <- ifelse(toyota$FuelType == "Petrol", 1, 0)
Reg_final_2 <- lm(toyota$Price ~ toyota$Age + toyota$KM + toyota$PetrolFuel + toyota$HP 
                  + toyota$Weight)

summary(Reg_final_2) # This model has 86.36% explainability

# Looking at the p-values (which are significantly higher for other variables as compared to FuelType)
# re-run the regression model by completing eliminating the FuelType variable
Reg_final_2 <- lm(toyota$Price ~ toyota$Age + toyota$KM + toyota$HP 
                  + toyota$Weight)
summary(Reg_final_2) # This model has 86.25% explainability

# Re-run the Multicollinearity test
vif(Reg_final_2) # There is no presence of multicollinearity

# We will predict a set of prices using our model
predicted_prices <- predict(Reg_final_2)
# The actual prices from the dataset
actual_prices <- toyota$Price
# Check the error or the residual values
residual_error_prices <- resid(Reg_final_2)
# create the dignostic data frame
diagnostic_dataFrame <- data.frame(actual_prices, predicted_prices, residual_error_prices)
colnames(diagnostic_dataFrame) <- c("Actual-Prices","Predicted-Prices","Residual-Errors")

# We will plot the actual vs predicted
plot(diagnostic_dataFrame$`Actual-Prices`, type = "l", col = "blue", main = "Actual vs Predicted Prices", 
     ylab = "Prices")
lines(diagnostic_dataFrame$`Predicted-Prices`, type = "l", col = "red")
#legend(x = "topright", c("Actual", "Predicted"), lty = c(1, 1), lwd = c(1, 1), col = c("blue", "red"))

# we will now check for heteroscedasticity
# to check heteroscedasticity, we plot, predicted vs residual
plot(diagnostic_dataFrame$`Predicted-Prices`, diagnostic_dataFrame$`Residual-Errors`, abline(0, 0), main = "Check for Heteroscedasticity", xlab = "Residual Error", ylab = "Predicted GPM", pch = 3, col = "blue")
# looking at the above plot, we suspect presence of heteroskedasticity
bptest(Reg_final_2) # The significant p-value confirms the presence of heteroskedasticity
# Strongly suggests the presence of Heteroskedasticity - hence we will use White's heteroskedasticity-corrected 
# covariance matrix to make the inference
coeftest(Reg_final_2, vcov = hccm(Reg_final_2))
# We know heteroskedasticity does not bias the coefficients but the errors and We use this coeffieicient and error to make the inference

# The sum of all residual errors should be zero
round(sum(Reg_final_2$residuals), digits = 6)

# ********** Conclusion **************************************************** #

# We see with very minimal Multicollinearity, no heteroscedasticity and a good Adjusted R-Squared of 
# 86.25% explainability, Reg_final can be accepted as a good predictive model to predict GPM as a function of 
# IDVs
summary(Reg_final_2)
