# *************************************************************************************************************
# R Fundamentals - This program explains how to work with XML using R
#
# Dataset Used - We will use the World Bank Data
# **************************************************************************************************************

rm(list = ls()) # We clear all runtime variables in the Environment

# Using external libraries
library(XML)

# The URL of the dataset being used
strURL <- "http://data.worldbank.org/indicator/SP.POP.TOTL"

# Now the URL page has table and we would use a function which will scrape the page and read the HTML tables
# the function will return a list of tables
tables <- readHTMLTable(strURL, stringsAsFactors = FALSE)
# Check the number of elements in the list
str(tables) # there are two tables in the web page

# Lets examine the first element in the list
tables[[1]]
# Lets examing the second element in the list
tables[[2]] # We do not need this table for our analysis

# We will create a data frame from the first element of our list
tableFrame <- data.frame(tables[[1]])
# examine the structure of the data frame
str(tableFrame)
# The last two columns are blank, removing it from the table
tableFrame <- tableFrame[, -c(7, 8)]
# re arranging the column names
colnames(tableFrame) <- c("Country", "2011", "2012", "2013", "2014", "2015")
# examine the first few rows the data frame
head(tableFrame)
# examine the structure of the data frame
str(tableFrame)

# The population figures should be numeric and converting by removing ','
tableFrame$`2011` <- as.numeric(gsub(",","",tableFrame$`2011`))
tableFrame$`2012` <- as.numeric(gsub(",","", tableFrame$`2012`))
tableFrame$`2013` <- as.numeric(gsub(",","", tableFrame$`2013`))
tableFrame$`2014` <- as.numeric(gsub(",","", tableFrame$`2014`))
tableFrame$`2015` <- as.numeric(gsub(",","", tableFrame$`2015`))

# Now we would use this data frame for finding out information
# To find the country having max population in 2011
maxPopCountry <- subset(tableFrame$Country, tableFrame$`2011` == max(tableFrame$`2011`))