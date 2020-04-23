# ******************************************************************************************************
# Regression Analysis on MMIX (market mix) Dataset... to find the indepedent variables affecting our variable
# of interest - NewVolumeSales
#
# Work on the Market Mix dataset
# *******************************************************************************************************
options(scipen = 999) # No automatic round off will happen

# uses of external libraries
library(robustbase)
library(car)

# we will load the highway deaths data set
# we will load the dataset (this is from SUVOS-TIME-CAPS)
switch(Sys.info() [['sysname']],
       Windows = {mmix.ds <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/MMix.csv", header = TRUE)},
       Linux   = {mmix.ds <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/MMix.csv", header = TRUE)},
       Darwin  = {mmix.ds <- read.csv("//Volumes/Data/CodeMagic/Data Files/MMix.csv", header = TRUE)})

# check the data load
head(mmix.ds)
names(mmix.ds)
str(mmix.ds)

# ********** Data Exploration and Data Preparation Block **************************************************** #

# we will obtain summary statistics and do some basic data sanity checks, -ve price, impossible values etc
# basic sanity checks reveals no wrong values in the dataset
head(mmix.ds)

# Check for Missing values
# check overall missing values in mmix.ds
colSums(is.na(mmix.ds)) # looks like there are no missing values in the data set

# Checks on outliers in the data
adjbox(mmix.ds$NewVolSales)
plot(mmix.ds$NewVolSales)
# looking at the boxplot and plot graphs, we conclude there are no definite outliers on our dependent variable.

# Qualitative to Quantitative Transformations

# We already have Quantitative Variables defined for NewspaperInserts, Website.Campaign (Facebook, Twitter, WebCamp, Online)

# Generate Derived Variables
# We are not creating any derived variables for now

# Graphical Analysis, correlations, tabulations etc - to select the potentially impacting IDV's

# general distribution of the DV - NewVolSales
hist(mmix.ds$NewVolSales, xlab = "NewVolumeSales", col = "blue", main = "Histogram of New Volume Sales", labels = TRUE)

# relationship between NewVolumeSales vs Base.Price
hist(mmix.ds$Base.Price, labels = TRUE, col = "blue", main = "Histogram for the Base Price", xlab = "Base Price")
summary(mmix.ds$Base.Price)
plot(mmix.ds$Base.Price, mmix.ds$NewVolSales, main = "NewVolumeSales vs Base Price", xlab = "Base Price", ylab = "NewVolumeSales", col = "blue", pch = 19, cex = 0.6)
# this makes sense as with the increase of Base Price, we see a declining trend of NewVolumeSales

# Check on correlations
cor(mmix.ds$NewVolSales, mmix.ds$Base.Price)
# denotes a negative correlation - 73% of times, when there is an increase in Base.Price, volume sales have gone down

# relationship between NewVolumeSales vs InStore discount
summary(mmix.ds$InStore)
hist(mmix.ds$InStore, labels = TRUE, col = "dark green", main = "Histogram for InStore discount", xlab = "InStore Discount")
plot(mmix.ds$InStore, mmix.ds$NewVolSales, col = "dark green", main = "NewVolumneSales vs InStore Discounts", xlab = "InStore Discounts", ylab = "NewVolumeSales", cex = 0.6, pch = 19)
# overall an increasing trend of as we increase the discount, we see an increase in sales
# quick check on correlation
cor(mmix.ds$NewVolSales, mmix.ds$InStore)
# a positive correlation, 44% of the times, when InStore discounts have increased, Sales have increased.

# some tabulations with Categorical Variable
plot(mmix.ds$Website.Campaign, mmix.ds$NewVolSales)
barplot(tapply(mmix.ds$NewVolSales, list(mmix.ds$Website.Campaign), mean), main = "Average Vol Sales by Website Campaigns", xlab = "Website Campaigns", ylab = "Volume Sales", col = "dark green")
AvgVolSales.WebsiteCampaign <- aggregate(mmix.ds$NewVolSales, list(mmix.ds$Website.Campaign), mean)
colnames(AvgVolSales.WebsiteCampaign) <- c("Website Campaign", "Average Volume Sales")
AvgVolSales.WebsiteCampaign

# How Volume Sales is looking like, as compared to marketing cost in Radio
plot(mmix.ds$Radio, mmix.ds$NewVolSales, main = "Effect of Radio on Volume Sales", xlab = "Radio Marketing Cost", ylab = "Volume Sales", pch = 5, col = "blue") # looks to be of no effect
cor(mmix.ds$NewVolSales, mmix.ds$Radio) # with no correlation
# highly unlikely that this variable will have any effect on the predictive model.

# Volume Sales vs TV
plot(mmix.ds$TV, mmix.ds$NewVolSales, main = "Effect of TV on Volume Sales", xlab = "TV Marketing Cost", ylab = "Volume Sales", pch = 5, col = "blue")
# looks TV Marketing cost has not much effect on Volume Sales
# quick check on correlation
cor(mmix.ds$NewVolSales, mmix.ds$TV) # with a very low correlation
# highly unlikely that this variable will have any effect on the predictive model. Even if there is
# effort is not significant

# ********** Regression Model **************************************************** #

# we will include 1 IDV at a time to comeup with the final model

# Iteration - 1 - With Base.Price
Reg1 <- lm(mmix.ds$NewVolSales ~ mmix.ds$Base.Price)
summary(Reg1) # We see Adjusted R-squared is 52% and the IDV is statistically very significant

# Iteration -2 - Will include InStore discount
# Will run a temporary regression with only InStore discount
RegTemp <- lm(mmix.ds$NewVolSales ~ mmix.ds$InStore)
summary(RegTemp) # We see Adjusted R-squared is 18.7% and InStore is statistically significant
# We will add InStore together with Base.Price
Reg2 <- lm(mmix.ds$NewVolSales ~ mmix.ds$Base.Price + mmix.ds$InStore)
summary(Reg2) # We see Adjusted R-squared is 60.3% and both IDV's are statistically significant

# Iteration - 3 - Will Check for TV and Radio (Just a check, even if we think, they may not be statistically significant)
Reg3 <- lm(mmix.ds$NewVolSales ~ mmix.ds$Base.Price + mmix.ds$InStore + mmix.ds$Radio + mmix.ds$TV)
summary(Reg3) # We see with 10% significance level TV marketing cost is statistically significant

# Iteration - 4 - We will drop the IDV Radio as that seems to have no effect on Volume Sales
Reg4 <- lm(mmix.ds$NewVolSales ~ mmix.ds$Base.Price + mmix.ds$InStore + mmix.ds$TV)
summary(Reg4) # with Adjusted R-squared 62.3% all are statistically significant at 5% significance level

# Iteration - 5 - We will check for the qualitative IDV, Website.Campaign
# Check for individual contribution
str(mmix.ds$Website.Campaign)
RegTemp <- lm(mmix.ds$NewVolSales ~ mmix.ds$Website.Campaign)
summary(RegTemp)

# We see Facebook and Twitter marketing cost has no effect, however Website Campaign is significant at 5
# 5% significance level
# We will use the dummy variable created only for Website.Campaign finally
# Including the whole variable on the till evolved model structure
Reg5 <- lm(mmix.ds$NewVolSales ~ mmix.ds$Base.Price + mmix.ds$InStore + mmix.ds$TV + mmix.ds$Website.Campaign)
summary(Reg5)
# When added to the model, we dont see TV to be significant anymore but Website Campaign is very  significant
# with Adjusted R-squared is 72%

# Iteration - 6 - We will drop TV IDV
Reg6 <- lm(mmix.ds$NewVolSales ~ mmix.ds$Base.Price + mmix.ds$InStore + mmix.ds$Website.Campaign)
summary(Reg6) # Facebook and Twitter Marketing cost is not significant
# Explainability is 71.6%

# Iteration - 7 - We will use the dummy variable for Website Campaign only
Reg7 <- lm(mmix.ds$NewVolSales ~ mmix.ds$Base.Price + mmix.ds$InStore + as.factor(mmix.ds$WebCamp))
summary(Reg7)

# Iteration - 8 - We will include Newspaper Inserts now and will check
Reg8 <- lm(mmix.ds$NewVolSales ~ mmix.ds$Base.Price + mmix.ds$InStore + as.factor(mmix.ds$WebCamp) + mmix.ds$NewspaperInserts)
summary(Reg8)
# Newspaper Insert is not significant and we will drop it in the final model

# Iteration - 9 - Final Model
Reg_final <- lm(mmix.ds$NewVolSales ~ mmix.ds$Base.Price + mmix.ds$InStore + mmix.ds$WebCamp)
summary(Reg_final)

# Final Linear Regression Model looks like
# NewVolSales = 50375.483 - 2045.716 * Base.Price + 37.637 * InStore - 2255.361 * WebSiteCampaign_Enabled

# ********** Model Diagnostics **************************************************** #

# we will predict a set of values using our reg_final model
predicted_sales <- predict(Reg_final)
# actual sales
actual_sales <- mmix.ds$NewVolSales
# Check for the errors or the residual values
residual_error_sales <- resid(Reg_final)
diagnostic_dataFrame <- data.frame(predicted_sales, actual_sales, residual_error_sales)

# we will plot the actual vs predicted
plot(diagnostic_dataFrame$actual_sales, type = "l", col = "blue", main = "Actual vs Predicted")
lines(diagnostic_dataFrame$predicted_sales, type = "l", col = "red")
legend(x = "topleft", c("Actual", "Predicted"), lty = c(1, 1), lwd = c(2, 2), col = c("blue", "red"))

# we will now check for multicollinearity within our IDV... For a good model, multicollinearity should be less than 10
# multicollinearity is checked using the function vif() - or variable inflation factor
vif(Reg_final)

# we will now check for heteroscedasticity
# to check heteroscedasticity, we plot, predicted vs residual
plot(predicted_sales, residual_error_sales, abline(0, 0), main = "Check for Heteroscedasticity", xlab = "Residual Error", ylab = "Predicted Sales", pch = 3, col = "blue")
# looking at the above plot, we cannot see any heterscedasticity. The variances are at random and there is no pattern emerging from the plot

# the sum of all residual errors should be 0
round(sum(Reg_final$residuals), digits = 6) # for greater accuracy, we will use a six digit precision decimal

# ********** Conclusion **************************************************** #

# We see with almost no Multicollinearity, no heteroscedasticity and a good Adjusted R-Squared of 71.21% explainability, Reg_final can be accepted as a good
# predictive model to predict NewVolSales as a function of IDV's

# NewVolSales = 50375.483 - 2045.716 * Base.Price + 37.637 * InStore - 2255.361 * (WebCamp?)

summary(Reg_final)


