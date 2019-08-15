# ************************************************************************************************************************
# Exploratory Data Analysis - Orange Juice data
# Analysis of the weekly sales data of refrigerated 64-ounce orange juice containers from 83 stores in the Chicago area.
# There are different brands, many stores and many time periods
# Recorded store sales in logarithms; logmove

# This makes extensive usage of lattice package
# ************************************************************************************************************************
options(scipen = 999) # preventing no automatic roundoffs

library(lattice)

# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems
switch(Sys.info() [['sysname']],
       Windows = {orangejuice <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/oj.csv", header = TRUE)},
       Linux =   {orangejuice <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/oj.csv", header = TRUE)},
       Darwin =  {orangejuice <- read.csv("//Volumes/Data/CodeMagic/Data Files/LedolterDatasets/DataText/oj.csv", header = TRUE)})

# Check the data load
dim(orangejuice)
head(orangejuice)
str(orangejuice)

# store should be a factor
orangejuice$store <- as.factor(orangejuice$store)

# check for missing values
sum(is.na(orangejuice)) # There are no missing values

# Average sales across brands
tapply(orangejuice$logmove, orangejuice$brand, mean, na.rm = TRUE)
barchart(tapply(orangejuice$logmove, orangejuice$brand, mean, na.rm = TRUE), horizontal = FALSE, col = "black") # They are almost same across brands

# Average sales across brands and across weeks
brandsales.weeks <- tapply(orangejuice$logmove, list(orangejuice$brand, orangejuice$week), mean, na.rm = TRUE)
# weekly sales for each of the brands
# For brand dominicks
plot(brandsales.weeks[1, ], type = "l", xlab = "Week(s)", ylab = "Sales (Dominicks)", ylim = c(7, 12))
# For brand Minute.Maid
plot(brandsales.weeks[2, ], type = "l", xlab = "Week(s)", ylab = "Sales (Minute.Maid)", ylim = c(7, 12))
# For brand Tropicana
plot(brandsales.weeks[3, ], type = "l", xlab = "Week(s)", ylab = "Sales (Tropicana)", ylim = c(7, 12))
# lets take a comprehensive view
xyplot(orangejuice$logmove ~ orangejuice$week|orangejuice$brand, type = "p", layout = c(1,3), col = "black")
boxplot(orangejuice$logmove ~ orangejuice$brand)
histogram(~orangejuice$logmove|orangejuice$brand, layout = c(1,3))
densityplot(~orangejuice$logmove|orangejuice$brand, layout = c(1,3), plot.points = FALSE)
densityplot(~orangejuice$logmove, groups = orangejuice$brand, plot.points = FALSE)

# analyse the sales data with respect to price
xyplot(orangejuice$logmove~orangejuice$price, col = "black")
# analyse the sames data with respect to price by brands
xyplot(orangejuice$logmove~orangejuice$price|orangejuice$brand, layout = c(1,3), col = "black")
smoothScatter(orangejuice$price, orangejuice$logmove)
# The above graphs shows that sales decrease with increasing price.

# analyse the sales data with respect to feat (feature advertisement)
densityplot(~orangejuice$logmove, groups = orangejuice$feat, plot.points = FALSE)
# analysis with respect to price by feature advertisement
xyplot(orangejuice$logmove~orangejuice$price, groups = orangejuice$feat)

# we will now analyse based on a particular store - lets say store 5 ( we can do this for all other as well ...)

# Time sequence plots of the sales of store 5 are shown for the three brands. Scatter plots of sales against price, separately
# for the three brands, are also shown; sales decrease with increasing price. Density histograms of sales and scatter plots of sales against price, 
# with weeks with and without feature advertisement coded in color, are shown for each of the threebrands. 
# Again, these graphs show very clearly that feature advertisement increases the sales.

# subset the data related to store 5
orangejuice_5 <- subset(orangejuice, orangejuice$store == 5)
# time series plot for sales in store 5 by brands for all weeks
xyplot(orangejuice_5$logmove~orangejuice_5$week|orangejuice_5$brand, type = "l", layout = c(1,3), col = "black")
# overall sales with price
xyplot(orangejuice_5$logmove~orangejuice_5$price, col = "black")
# dividing by brand
xyplot(orangejuice_5$logmove~orangejuice_5$price|orangejuice_5$brand, layout = c(1,3), col = "black")
# density plot
densityplot(~orangejuice_5$logmove|orangejuice_5$brand, groups = orangejuice_5$feat, plot.points = FALSE)
#sales vs price by brand by feature of advertisement
xyplot(orangejuice_5$logmove~orangejuice_5$price|orangejuice_5$brand, groups = orangejuice_5$feat)

# We may be interested in assessing whether the sensitivity (elasticity) of the sales to changes in the price
# depends on the income of the customers who live in the store’s neighborhood. We may expect that the price elasticity is largest in poorer 
# neighborhoods as poorer customers have to watch their spending budgets more closely. To follow up on this hypothesis, we look for the stores in 
# the wealthiest and the poorest neighborhoods.

# we will extract out store with highest average income and store with lowest average income
store_income <- tapply(orangejuice$INCOME, orangejuice$store, mean, na.rm = TRUE)
store_income[store_income==max(store_income)]
store_income[store_income==min(store_income)]
orangejuice_maxIncomeStore <- subset(orangejuice, orangejuice$store == 62)
orangejuice_minIncomeStore <- subset(orangejuice, orangejuice$store == 75)
orangejuicePriceElastic <- rbind(orangejuice_maxIncomeStore,orangejuice_minIncomeStore)
xyplot(orangejuicePriceElastic$logmove~orangejuicePriceElastic$price|orangejuicePriceElastic$store)
# we see store 62 is in the wealthiest area and the sales is unaffected by changes in price, whereas store 75 is in the poorest area and 
# sales is largely affected by increase in price
xyplot(orangejuicePriceElastic$logmove~orangejuicePriceElastic$price|orangejuicePriceElastic$store, groups = orangejuicePriceElastic$feat)

# we may continue this exploration for other stores as well

# We can use this data set to investigate clustering. We may want to learn whether it is possible to reduce the 83 stores to a smaller number of 
# homogeneous clusters.Furthermore, we may want to explain sales as a function of explanatory variables
# such as price, feature advertisements, and the characteristics of the store neighborhood. In particular, we may want to study whether 
# the effects of price changes and feature advertisements depend on demographic characteristics of the store neighborhood.