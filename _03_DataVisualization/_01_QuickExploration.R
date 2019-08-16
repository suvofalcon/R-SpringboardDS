# ************************************************************************************************************************
# Quick Data Exploration
# 
# This demonstrates the scatter plot, bar plot, line plot, histograms and box plots
# We would use ggplot2 package exhaustively throughout the code along with default R Base graphics capabilities
# This file will demonstrate basic how to's and the detail explanation will follow later.
# ************************************************************************************************************************

# use of external libraries
library(ggplot2)
library(gcookbook)

# Creating a Scatter Plot
# ***************************************************************************

# using dataset mtcars
data("mtcars")
# scatter plot using base R package
plot(mtcars$wt, mtcars$mpg)

# using ggplot2
qplot(mtcars$wt, mtcars$mpg)
# if the two variables are in the same data frame we can also use
qplot(wt, mpg, data = mtcars)
# This is equivalent to
ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() # This is more generic way of specifying

# Creating a Line Graph
# ***************************************************************************

# using dataset pressure
data("pressure")
# line graph using base R Package
plot(pressure$temperature, pressure$pressure, type = "l")
# to show the points, which are joined by the line
points(pressure$temperature, pressure$pressure)
# to add one more line graph in the same area
lines(pressure$temperature, pressure$pressure/2, col = "red")
points(pressure$temperature, pressure$pressure/2, col = "red")

# using ggplot2
qplot(pressure$temperature,pressure$pressure, geom = "line")
# The above is equivalent to
ggplot(pressure, aes(x = temperature, y = pressure)) + geom_line()
# We will plot the lines and points together
qplot(pressure$temperature, pressure$pressure, geom = c("line", "point"))
# The above is equivalent to
ggplot(pressure, aes(x = temperature, y = pressure)) + geom_line() + geom_point()

# Creating a Bar Graph
# ***************************************************************************

# using dataset mtcars
data("mtcars")
# to generate a graph of counts using base R package
barplot(table(mtcars$cyl))
# using ggplot2
qplot(mtcars$cyl, geom = "bar", stat = "identity") # here cyl has been treated as continuous variable
# to treat the cyl variable as discrete
qplot(factor(mtcars$cyl), geom = "bar", stat = "identity")
# The above is equivalent to
ggplot(mtcars, aes(x = factor(cyl))) + geom_bar()

# Creating a Histogram
# ***************************************************************************

# using dataset mtcars
data("mtcars")
# to create a histogram
hist(mtcars$mpg)
# to specify approximate number of bins
hist(mtcars$mpg, breaks =  10)
# using ggplot2
qplot(mtcars$mpg, binwidth = 10)
# The above is equivalent to
ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 5)
# Add some decorations to distinguish the bars
ggplot(mtcars, aes(x = mpg)) + geom_histogram(binwidth = 5, fill = "lightblue", colour = "black")

# Creating Box Plots
# ***************************************************************************

# When x-axis is a factor (as opposed to a numeric vector), it will automatically create a box plot
# using dataset ToothGrowth
data("ToothGrowth")
plot(ToothGrowth$supp, ToothGrowth$len)
# If we need to put interaction of two variables on the x-axis
boxplot(ToothGrowth$len ~ ToothGrowth$supp + ToothGrowth$dose)
# using ggplot2
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
# To show interaction
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
# The above is equivalent to
ggplot(ToothGrowth, aes(x = interaction(supp, dose), y = len)) + geom_boxplot()