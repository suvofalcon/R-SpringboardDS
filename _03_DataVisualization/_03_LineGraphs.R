# ************************************************************************************************************************
# Line Graphs
# 
# This demonstrates Line Graphs in detail using ggplot2
# Bar Graphs are typically used to visualize one continuous variable changes in relation to another continuous variable
# ************************************************************************************************************************
options(scipen = 999)

# use of external libraries
library(ggplot2)
library(gcookbook)
library(plyr)

# Basic line graph
ggplot(BOD, aes(x = Time, y = demand)) + geom_line()

# Line graphs can be made with discrete (categorical) or continuous (numeric) variables
# on the x-axis. When the x variable is a factor, you must also use aes(group=1) to ensure that ggplot() knows
# that the data points belong together and should be connected with a line

# Let's say in the BOD data, if we treat Time as factor
BOD1 <- BOD
BOD1$Time <- factor(BOD1$Time)
ggplot(BOD1, aes(x = Time, y = demand, group = 1)) + geom_line()

# The default y range is just enough to include the y-values in the data. If we need to have the range start from zero
ggplot(BOD, aes(x = Time, y = demand)) + geom_line() + expand_limits(y = 0)

# to add points to the graph
ggplot(BOD, aes(x = Time, y = demand)) + geom_line() + expand_limits(y = 0) + geom_point()

# using the worldpop dataset
ggplot(worldpop, aes(x = Year, y = Population)) + geom_line() + geom_point()
# since the intervals in the y-axis are unusually large and intervals are not consistent, we can use log transformations to smoothen out the data
ggplot(worldpop, aes(x = Year, y = Population)) + geom_line() + geom_point() + scale_y_log10()
# Now we can see the proportional change in the beginning 1000 years has been much slower than the recent times...


# Making a Line Graph with Multiple Lines

# Lets use the ToothGrowth dataset
head(ToothGrowth, 10)

# Lets summarise the data by supp and by dose
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length = mean(len))
# check the summarisation
head(tg, 10)
# Now the plotting
ggplot(tg, aes(x = dose, y = length, colour = supp)) + geom_line() + geom_point()
# If the variable dose is treated as categorical then
ggplot(tg, aes(x = factor(dose), y = length, colour = supp, group = supp)) + geom_line() + geom_point(size = 2) # to make the points little larger

# Changing the appearance of the lines
ggplot(BOD, aes(x = Time, y = demand)) + geom_line(linetype = "dashed", size = 1, colour = "blue")
ggplot(BOD, aes(x = Time, y = demand)) + geom_line(colour = "darkgreen", size = 1.5)

# Changing the Appearance of Points

ggplot(BOD, aes(x = Time, y = demand)) + geom_line(colour = "darkblue", size = 1) + geom_point(size = 4, shape = 22, colour = "red", fill = "pink")
# when there are two line graphs, we can make individual changes
ggplot(tg, aes(x = dose, y = length, fill = supp)) + geom_line(position = position_dodge(0.2)) + 
  geom_point(shape = 21, size = 3, position = position_dodge(0.2)) + scale_fill_manual(values = c("black", "white"))

# Making Graph with Shaded Area

# Convert the sunspot.year data set into data frame for this example
sunspotyear <- data.frame( Year = as.numeric(time(sunspot.year)), Sunspots = as.numeric(sunspot.year))
ggplot(sunspotyear, aes(x = Year, y = Sunspots)) + geom_area()
# to color the changes the aesthetics of the shaded area
ggplot(sunspotyear, aes(x = Year, y = Sunspots)) + geom_area(colour = "black", fill = "blue", alpha = 0.2)

# Making a stacked area graph
ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) + geom_area(colour = "black")
# The default levels are opposite and the legends are reversed as
ggplot(uspopage, aes(x = Year, y = Thousands, fill = AgeGroup)) + geom_area(colour = "black") + 
  scale_fill_brewer(palette = "Blues", breaks = rev(levels(uspopage$AgeGroup)))

# Making a confidence region
ggplot(climate, aes(x = Year, y = Anomaly10y)) + geom_ribbon(aes(ymin = Anomaly10y-Unc10y, ymax = Anomaly10y+Unc10y), alpha = 0.2) + geom_line()