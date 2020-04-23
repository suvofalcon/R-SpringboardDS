# ******************************************************************************************************
# Regression Analysis - Predictive Algorithm using multiple linear regression to arrive an explanation
# for the price of the car as a function of different attributes
# ******************************************************************************************************
options(scipen = 999)

# use of external libraries
library(car)

# load the cars retail price csv file and check for the file load
# we will load the dataset (this is from SUVOS-TIME-CAPS)
switch(Sys.info() [['sysname']],
       Windows = {cars.ds <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/cars_retail_price-class11.csv", header = TRUE)},
       Linux   = {cars.ds <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/cars_retail_price-class11.csv", header = TRUE)},
       Darwin  = {cars.ds <- read.csv("//Volumes/Data/CodeMagic/Data Files/cars_retail_price-class11.csv", header = TRUE)})

# check the data load
head(cars.ds)
names(cars.ds)
str(cars.ds)
nrow(cars.ds) # 804 rows are present in the dataset

# ********** Data Exploration and Data Preparation Block **************************************************** #

# Check for Missing values
# check overall missing values in cars.ds
sum(is.na(cars.ds))
colSums(is.na(cars.ds)) # looks like there are no missing values in the dataset

# we will obtain summary statistics and do some basic data sanity checks, -ve price, impossible values etc
nrow(subset.data.frame(cars.ds, cars.ds$Price <= 0)) # -ve price
nrow(subset.data.frame(cars.ds, cars.ds$Mileage <= 0)) # 0 or less than 0 mileage

# We will use Summary stats for the quantitative Columns
summary(cars.ds$Price)
sd(cars.ds$Price)
summary(cars.ds$Mileage)
sd(cars.ds$Mileage)
summary(cars.ds$Liter)
sd(cars.ds$Liter)

# For a qualitative or categorical like variables, we will check the distinct values or labels
levels(cars.ds$Make) # there are six levels
levels(cars.ds$Model) # There are 32 levels of Model
levels(cars.ds$Trim) # There are 47 levels of Trim
levels(cars.ds$Type) # There are five levels of Type
unique(cars.ds$Cylinder) # There are three cylinder variant types, 6, 8 and 4
unique(cars.ds$Doors) # 4 Doors and 2 Doors configuration
unique(cars.ds$Cruise) # Whether or Not Cruise Control enabled - 1 for Cruise control
unique(cars.ds$Sound) # Whether or not upgraded Sound Speakers - 1 for upgraded speakers
unique(cars.ds$Leather) # Whether or not leather upholstery - 1 for leather seats fitted

# Some graphical check for outliers - Variable Price
boxplot(cars.ds$Price, ylab = "Car Price(s)")
plot(cars.ds$Price, col = "blue", ylab = "Car Price(s)", main = "Scatter Plot for Car Prices")
hist(cars.ds$Price, col = "dark green", xlab = "Car Price(s)", main = "Histogram for Car Prices", labels = TRUE)

# looking at the above three plots, it suggests all cars priced above 50000 is different than the sample population
nrow(subset.data.frame(cars.ds, cars.ds$Price >= 50000))
# to investigate the 11 records further
subset.data.frame(cars.ds, cars.ds$Price >= 50000)
# looking at the data frame, we see these cars are very similar with same Make, Same Type and similar engine ratings, clearly tells the possibility of 
# high value vehicles. The differences in price also apparently seems dependent on the Mileage.
# we do not have enough reason to believe they are outliers - its just high value cars being included in the data set in a population where majority is within
# 10000 - 20000. Whether a separate model needs to be prepared for high value vehicles for price prediction is a business call now.
# We would be including the records in the dataset

# Looking at the variable Mileage
boxplot(cars.ds$Mileage, main = "Box Plot for Car Mileages", ylab = "Cars Mileage")
plot(cars.ds$Mileage, main = "Scatter Plot for Car Mileages", ylab = "Cars Mileage", col = "blue", pch = 3)
hist(cars.ds$Mileage, col = "dark green", xlab = "Cars Mileage", main = "Histogram for Cars Mileage", labels = TRUE)
# The scatter plot and histogram shows two points clearly differentiating from the rest of the population
subset.data.frame(cars.ds, cars.ds$Mileage >= 45000)
# looking at the two points, we see both the cars have driven similar mileages and the higher mileage run car has the lower price. They are of the same Make
# and almost similar model. They are just cars which has been driven quite a lot in one year. We need to consult business before we treat them as outliers
# and remove them from the dataset. Unless business wants to have a separate model for high utilized vehicles, we are retaining data rows in the model.

# Tablulations and Graphical Plots

# Price vs Mileage
plot(cars.ds$Mileage, cars.ds$Price, pch = 4, main = "Price vs Mileage", xlab = "Cars Mileage", ylab = "Cars Price", col = "blue")
# From the plot a clear defniite trend or relationship of Price vs Mileage is not emerging out
cor(cars.ds$Price, cars.ds$Mileage) # The correlation shows a negative relationship but not a strong one
# Lets create three buckets for Mileages - depending on how much the cars have run
# The intention is to observe, whether there is a definite relationship between Avg Price and Mileage in each of the buckets
cars.ds$Run_Bkt[cars.ds$Mileage < 20000] <- "Low Run"
cars.ds$Run_Bkt[cars.ds$Mileage >= 20000 & cars.ds$Mileage < 30000] <- "Normal Run"
cars.ds$Run_Bkt[cars.ds$Mileage >= 30000] <- "High Run"
cars.ds$Run_Bkt <- as.factor(cars.ds$Run_Bkt)
cars.ds.ByRunBkt <- aggregate(cars.ds$Price, list(cars.ds$Run_Bkt), mean)
colnames(cars.ds.ByRunBkt) <- c("Run Bracket", "Average Price")
cars.ds.ByRunBkt
barplot(tapply(cars.ds$Price, cars.ds$Run_Bkt, mean), col = "Dark Green", xlab = "Mileage Buckets", ylab = "Car Prices (Avg)", main = "Avg Prices By Mileage Run")
# Looks to be Low run Cars have higher average price, which intuitively makes sense, but other that, it is definite at this point of time
# that there are other factors influencing the Car Prices other than just Mileage.

# Price vs Make
cars.ds.ByMake <- aggregate(cars.ds$Price, list(cars.ds$Make), mean)
colnames(cars.ds.ByMake) <- c("Car Make", "Average Car Price")
cars.ds.ByMake
barplot(tapply(cars.ds$Price, cars.ds$Make, mean), col = "Dark Green", xlab = "Make of Cars", ylab = "Average Car Price", main = "Average Car Prices by Make")
lines(tapply(cars.ds$Mileage, cars.ds$Make, mean), col = "Purple", lwd = 2, type = "o")
# The above shows Average Car Prices are different across Make and when plotted against Mileages as compared to other brands for same 
# average Mileage Run, the prices are way high. Make has influence on the price.

# Price vs Model By Make 
coplot(cars.ds$Price ~ cars.ds$Model | cars.ds$Make, type = "p", col = "blue", main = "Price of Car vs Make By Models", ylab = "Car Prices", xlab = "Car Models by Make")
# Price vs Trim By Make
coplot(cars.ds$Price ~ cars.ds$Trim | cars.ds$Make, type = "p", col = "dark green", main = "Price of Car vs Trim by Models", xlab = "Car Trim by Models", ylab = "Car Prices")
# The above actually shows that Average Prices for every Make varies across its Models and Trim

# Just to check for the Buick Model, the average car prices also differ by Model and Trim
cars.ds.Buick <- subset.data.frame(cars.ds, cars.ds$Make == "Buick")
cars.ds.Buick$Make <- droplevels(cars.ds.Buick$Make)
cars.ds.Buick$Model <- droplevels(cars.ds.Buick$Model)
cars.ds.Buick$Trim <- droplevels(cars.ds.Buick$Trim)
cars.ds.Buick$Type <- droplevels(cars.ds.Buick$Type)
str(cars.ds.Buick)
barplot(tapply(cars.ds.Buick$Price, cars.ds.Buick$Model, mean), col = rainbow(nlevels(cars.ds.Buick$Model)), main = "Average Price by Model - Buick", xlab = "Models (Buick)", ylab = "Average Prices")
barplot(tapply(cars.ds.Buick$Price, cars.ds.Buick$Trim, mean), col = rainbow(nlevels(cars.ds.Buick$Trim)), main = "Average Price by Trim - Buick", xlab = "Trim (Buick)", ylab = "Average Prices")

# Price vs Type
cars.ds.ByType <- aggregate(cars.ds$Price, list(cars.ds$Type), mean)
colnames(cars.ds.ByType) <- c("Car Type", "Average Car Price")
cars.ds.ByType
barplot(tapply(cars.ds$Price, cars.ds$Type, mean), col = "Dark Green", xlab = "Car Types", ylab = "Average Car Prices", main = "Average Car Prices by Types")
# The above shows for various types of the vehicles the average prices vary, convertible being the highest and Hatchback being the lowest.

# Price vs Cylinder
cars.ds.ByCylinder <- aggregate(cars.ds$Price, list(cars.ds$Cylinder), mean)
colnames(cars.ds.ByCylinder) <- c("Cylinders", "Average Car Price")
cars.ds.ByCylinder
plot(cars.ds.ByCylinder$Cylinders, cars.ds.ByCylinder$`Average Car Price`, type = "b")
barplot(tapply(cars.ds$Price, cars.ds$Cylinder, mean), col = "Navy Blue", xlab = "Number of Cylinders", ylab = "Average Car Prices", main = "Average Car Prices By Number of Cylinders")
cor(cars.ds$Price, cars.ds$Cylinder) # also seems to be a moderate correlation between car prices and number of cylinders
# Clear trend that Average Price goes up as the number of Cylinders goes up..

# Price vs Engine Liters
plot(cars.ds$Liter, cars.ds$Price, type = "p")
cor(cars.ds$Price, cars.ds$Liter)
# We can create various categories for engine ratings and we can see whats the average car prices for each of the engine categories
cars.ds$EngineCC[cars.ds$Liter < 2.5] <- "Low Power"
cars.ds$EngineCC[cars.ds$Liter >= 2.5 & cars.ds$Liter < 4.5] <- "Avg Power"
cars.ds$EngineCC[cars.ds$Liter >= 4.5] <- "High Power"
cars.ds.byEngineCC <- aggregate(cars.ds$Price, list(cars.ds$EngineCC), mean)
colnames(cars.ds.byEngineCC) <- c("Engine Power", "Average Car Prices")
cars.ds.byEngineCC
barplot(tapply(cars.ds$Price, cars.ds$EngineCC, mean), xlab = "Engine CC Ratings", ylab = "Average Car Prices", main = "Average Car Prices by Engine CC", col = "Navy Blue")
# Clearly shows as the engine rating increases, the average car prices increases as well

# Price vs No of Doors
cars.ds.ByDoors <- aggregate(cars.ds$Price, list(cars.ds$Doors), mean)
colnames(cars.ds.ByDoors) <- c("Number of Doors", "Average Car Price")
cars.ds.ByDoors
barplot(tapply(cars.ds$Price, cars.ds$Doors, mean), col = "Dark Green", xlab = "Number of Doors", ylab = "Average Car Price", main = "Average Car Price vs Number of Car Doors")
# Looks to be on an average cars with 2 doors (Coupe etc) for this sample population has higher average prices than cars with 4 doors

# Price vs Cruise
cars.ds.ByCruise <- aggregate(cars.ds$Price, list(cars.ds$Cruise), mean)
colnames(cars.ds.ByCruise) <- c("Cruise Control", "Average Car Price")
cars.ds.ByCruise
barplot(tapply(cars.ds$Price, cars.ds$Cruise, mean), col = "Dark Green", xlab = "Presence of Cruise Control", ylab = "Average Car Price", main = "Average Car Price vs Presence of Cruise Control")
# Clearly shows the average car prices for cars fitted with cruise control is higher

# Price vs Sound
cars.ds.BySound <- aggregate(cars.ds$Price, list(cars.ds$Sound), mean)
colnames(cars.ds.BySound) <- c("Upgraded Speakers", "Average Car Price")
cars.ds.BySound
barplot(tapply(cars.ds$Price, cars.ds$Sound, mean), col = "Dark Green", xlab = "Upgraded Speaker System", ylab = "Average Car Price", main = "Average Car Price vs Upgraded Speaker System")
# looks to be in this sample population average car prices with cars fitted with Upgraded Speaker is lower than otherwise

# Price vs Leather
cars.ds.ByLeather <- aggregate(cars.ds$Price, list(cars.ds$Leather), mean)
colnames(cars.ds.ByLeather) <- c("Leather Seats", "Average Car Price")
cars.ds.ByLeather
barplot(tapply(cars.ds$Price, cars.ds$Leather, mean), col = "Dark Green", xlab = "Presence of Leather Seats", ylab = "Average Car Price", main = "Car Prices by Leather Seats")
# Clearly shows the average car prices are high when leather seats are present

# ********** Regression Model **************************************************** #

# We will use the approach of 1 IDV at a time to come up with the final model by iterations

# Iteration - 1 - With Mileage
Reg1 <- lm(cars.ds$Price ~ cars.ds$Mileage)
summary(Reg1) # We see Adjusted R-squared is 2% and the IDV is statistically significant

# Iteration - 2 - With Type
Reg2 <- lm(cars.ds$Price ~ cars.ds$Mileage + cars.ds$Type)
summary(Reg2) # We see all levels of Type is statistically significant and including the IDV Mileage, the adjusted R Square is now 33%

# Iteration - 3 - With Cylinder
Reg3 <- lm(cars.ds$Price ~ cars.ds$Mileage + cars.ds$Type + cars.ds$Cylinder)
summary(Reg3) # We see all the IDV's including the cyclinder variable is statistically significant. The adjusted R Square is 62.2%

# Iteration - 4 - With Engine Liter
Reg4 <- lm(cars.ds$Price ~ cars.ds$Mileage + cars.ds$Type + cars.ds$Cylinder + cars.ds$Liter)
summary(Reg4) # We see from this iteration, that Liter IDV is not statistically significant in deciding the car price
# We will use the categorical variable and see whether any ranges of Engine Liter is significant
Reg4 <- lm(cars.ds$Price ~ cars.ds$Mileage + cars.ds$Type + cars.ds$Cylinder + cars.ds$EngineCC)
summary(Reg4) # Now we see only Cars rated with High Engine Power is statistically significant.
# High Engine Power rated cars have Liters capacity as >= 4.5
# Hence if the Engine Liter displacement is >= 4.5, then the prices of car increases and that is statistically significant
# We will create a indicator variable for this level
cars.ds$EngineCC_HighPower <- ifelse(cars.ds$EngineCC == "High Power", 1, 0)
# Re-run the iteration
Reg4 <- lm(cars.ds$Price ~ cars.ds$Mileage + cars.ds$Type + cars.ds$Cylinder + cars.ds$EngineCC_HighPower)
summary(Reg4)

# Iteration - 5 - With Doors
Reg5 <- lm(cars.ds$Price ~ cars.ds$Mileage + cars.ds$Type + cars.ds$Cylinder + cars.ds$EngineCC_HighPower + cars.ds$Doors)
summary(Reg5) # We see there is a perfect collinearity present with the Doors variable with others.. We would be dropping Doors from the model
# Also running the Doors IDV indepdently we, see a very negligible impact on the price

# Iteration - 6 - with Cruise
Reg6 <- lm(cars.ds$Price ~ cars.ds$Mileage + cars.ds$Type + cars.ds$Cylinder + cars.ds$EngineCC_HighPower + cars.ds$Cruise)
summary(Reg6) # We see Cruise Control variable is statistically significant  with Adjusted R Squared value almost 73%

# Iteration - 7 - With Sound
Reg7 <- lm(cars.ds$Price ~ cars.ds$Mileage + cars.ds$Type + cars.ds$Cylinder + cars.ds$EngineCC_HighPower + cars.ds$Cruise + cars.ds$Sound)
summary(Reg7) # When put into the model, we dont see any statistical significance for a car having an upgraded Speaker System.
# Dropping the variable from the model

# Iteration - 8 - With Leather
Reg8 <- lm(cars.ds$Price ~ cars.ds$Mileage + cars.ds$Type + cars.ds$Cylinder + cars.ds$EngineCC_HighPower + cars.ds$Cruise + cars.ds$Leather)
summary(Reg8) # Leather Seats have statistical significance with the Price of the Car

# Iteration - 9 - with Make
Reg9 <- lm(cars.ds$Price ~ cars.ds$Mileage + cars.ds$Type + cars.ds$Cylinder + cars.ds$EngineCC_HighPower + cars.ds$Cruise + cars.ds$Leather + cars.ds$Make)
summary(Reg9) # We see all variants of the Make is statistically significant and when including in the overall model, the adjusted R square is very
# high with 91%.
# Also the above model, shows that with respect to the other coefficients, the statistical significance of curise and leather is very insignificant
# Re-run the model, dropping the IDV Cruise and Leather
Reg9 <- lm(cars.ds$Price ~ cars.ds$Mileage + cars.ds$Type + cars.ds$Cylinder + cars.ds$EngineCC_HighPower + cars.ds$Make)
summary(Reg9)

# Final Model

# We will not use the IDV Model and Trim. Since Model and Trim is dependent on Make and all Make variants are statistically significant.. 
# Introducing Model and Trim would introduce high collinearity in the model
# Also since only a particular engine capacity (4.0 litres and above has an influence on the price, dropping the variable to see impact on Adjusted R - Squared)

Reg_final <- lm(cars.ds$Price ~ cars.ds$Mileage + cars.ds$Type + cars.ds$Cylinder + cars.ds$Make)
summary(Reg_final)

# The Final Model 


# ********** Model Diagnostics **************************************************** #

# We will predict a set of Prices of cars using our finalized model
predicted_prices <- predict(Reg_final)
# The actual prices in a variable
actual_prices <- cars.ds$Price
# We will now check the errors or the residual values
residual_error_prices <- resid(Reg_final)
# Create the diagnostic data frame
diagnostic_dataFrame <- data.frame(actual_prices, predicted_prices, residual_error_prices)

# we will plot the actual vs predicted
plot(diagnostic_dataFrame$actual_prices, type = "l", col = "blue", main = "Actual vs Predicted", ylab = "Car Prices")
lines(diagnostic_dataFrame$predicted_prices, type = "l", col = "red")
legend(x = "topleft", c("Actual", "Predicted"), lty = c(1, 1), lwd = c(2, 2), col = c("blue", "red"))

# we will now check for multicollinearity within our IDV... For a good model, multicollinearity should be less than 10
# multicollinearity is checked using the function vif() - or variable inflation factor
vif(Reg_final)

# we will now check for heteroscedasticity
# to check heteroscedasticity, we plot, predicted vs residual
plot(predicted_prices, residual_error_prices, abline(0, 0), main = "Check for Heteroscedasticity", xlab = "Residual Error", ylab = "Predicted Sales", pch = 3, col = "blue")
# looking at the above plot, we cannot see any heterscedasticity. The variances are at random and there is no pattern emerging from the plot

# sum of all residual errors should be zero
round(sum(Reg_final$residuals), digits = 6) # for greater accuracy, we will use a six digit precision decimal

# ********** Conclusion **************************************************** #

# We see with very minimal Multicollinearity, no heteroscedasticity and a good Adjusted R-Squared of 91.21% explainability, Reg_final can be accepted as a good
# predictive model to predict Car Prices as a function of IDV's

# Price = 14704.9790 - 0.18387*Mileage - 10817.73587*TypeCoupe - 14195.27878*TypeHatchback - 12261.34849*TypeSedan - 7919.60329*TypeWagon + 3687.92011*Cylinder + 12777.28123*Cadillac - 1268.20463*Chevrolet - 2288.53778*Pontiac + 11632.41039*SAAB - 1187.95705*Saturn 
summary(Reg_final)
