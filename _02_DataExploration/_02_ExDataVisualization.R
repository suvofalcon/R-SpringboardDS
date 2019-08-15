# ************************************************************************************************************************
# Jigsaw Big Data Assignment
# Class 11 - Data Visualization
# ************************************************************************************************************************

# as per the study instructions, we will use the datasets library
library(datasets)
state <- data.frame(state.x77)

# 1. Try the commands and explain what you see

plot(state$Population)
# The above is a scatter plot for the population variable from the state dataset. It plots population on the Y-axis and 
# number(frequency) on the x-axis. For example - there are less than 10 states in the whole dataset having population 
# 20,000 and above

plot(state$Income, state$Murder)
# The above is again a scatter plot, but this time tying to depict, relationship between Income and Murder rate.
# Income is plotted on the x-axis and Murder rate on the Y-axis. We can see from the plots, that murder rates are highest on the
# lower income states - For example between the income range of 3500 - 4500, murder rates are very high

plot(log(state$Population), log(state$Area))
# The above is a scatter plot, depicting the log of State Population and log of State Area. The plot shows the relationship to be approximately flat.
# For the same log(state$Area), some states are more populated than others.. Some states are more congested than others, even if they are of same area.

# 2. Try and commands and explain what you see
hist(state$Area)
# Histogram - Frequency distribution of the area of the states. In the dataset, majority of the states have low land area. Only a very few have land area
# higher than 560,000. Positively skewed distribution with high kurtosis.

boxplot(state$Population)
# The above is a Box Whisker plot for the Population variable of the state data set. The State(s) haviing population equal to 20,000 and greater are excluded
# and shown as outliers. 95% of the states falls between the population of 1000 - 12,000 and that is what is being depicted as minimum and maximum by the
# whiskers. The median value from the plot ios approximately at 3000.

# 3. Draw a histogram for the income variable present in the state dataset, with appropriate labels, axes and titles. What R code would you write for this
# task?

hist = hist(state$Income, xlab = "State Income", ylab = "Number of States", col = "dark green", main = "Histogram of Income of States", labels = TRUE, ylim = c(0, 20))
# Shows maximum number of states falls within the income range of 4500 - 5000, with only 1 state with a very high income range

boxplot(state$Population, main = "Box Whisker Plot for Population", ylab = "State Population")
stem(state$Population)
# The above is a box-whisker plot for the population variable of the state data set. The State(s) having population equal to 20,000 and greater are excluded
# and shown as outliers. 95% of the states falls between the population of 1000 - 12000 and that is what is being depicted as minimum and maximum by the whiskers.
# The Median value of the plot is approximately 12000
# Minimum Value of State Population
min(state$Population)
# Maximum value of the State Population
max(state$Population) # However this value is treated as an outlier in the boxplot

# How to use SQL Statement within R using a separate library
library(sqldf)
test <- read.csv.sql("//Volumes/Data/CodeMagic/Data Files/NYSE_Stocks.txt", 
                     sql = "SELECT * FROM file WHERE V2 = 'ASP' AND V4 < 12.00",eol = "\n", header = FALSE, sep = ",")
head(test)