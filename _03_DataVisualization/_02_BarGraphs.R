# ************************************************************************************************************************
# Bar Graphs
# 
# This demonstrates Bar Graphs in detail using ggplot2
# Bar Graphs are typically used to demonstrate numeric values for different categories
# ************************************************************************************************************************

# use of external libraries
library(ggplot2)
library(gcookbook)
library(plyr)

# Using dataset
data("pg_mean")
ggplot(pg_mean, aes(x = group, y = weight)) + geom_bar(stat = "identity")
# To put some decorations etc
# When x is a continuous (or numeric) variable, the bars behave a little differently. 
# Instead of having one bar at each actual x value, there is one bar at each possible x value 
# between the minimum and the maximum. 
# The conversion of continuous variable to discrete is done by using factor()
ggplot(BOD, aes(x = factor(Time), y = demand)) + geom_bar(stat = "identity")
# give some decorations
ggplot(pg_mean, aes(x = group, y = weight)) + geom_bar(stat = "identity", fill = "lightblue", colour = "black")

# When we want to group bars together
data("cabbage_exp")
head(cabbage_exp)
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity", position = "dodge")
# As with variables mapped to the x-axis of a bar graph, variables that are mapped to the fill color of 
# bars must be categorical rather than continuous variables.
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity", position = "dodge", colour = "black") + scale_fill_brewer(palette = "Pastel1")
# Note that if there are any missing combinations of the categorical variables, that bar will be missing, 
# and the neighboring bars will expand to fill that space.

# Making Bar Graph of counts
data("diamonds")
ggplot(diamonds, aes(x = cut)) + geom_bar() # The default is stat = "bin" which counts up the number of cases
ggplot(diamonds, aes(x = carat)) + geom_histogram(binwidth = 0.5, fill = "lightblue", colour = "black")

data("uspopchange")
# we will combine bar graph by category for better visual comparison
ggplot(uspopchange, aes(x = Abb, y = Change, fill = Region)) + 
  geom_bar(stat = "identity", position = "identity", colour = "black") + xlab("State")
# position="identity" with the bars. This will prevent a warning message about stacking not being 
# well defined for negative numbers

# Coloring negative and positive bars differently
data("climate")
head(climate)
ggplot(climate, aes(x = Year, y =  Anomaly10y, fill = ifelse(Anomaly10y >= 0, TRUE, FALSE))) + 
  geom_bar(stat = "identity", position = "identity")
# To reverse the colors and remove the legend (since the legend is for representation and not an actual variable in the data)
ggplot(climate, aes(x = Year, y =  Anomaly10y, fill = ifelse(Anomaly10y >= 0, TRUE, FALSE))) + 
  geom_bar(stat = "identity", position = "identity", colour = "black", size = 0.25) + 
  scale_fill_manual(values = c("#CCEEFF", "#FFDDDD"), guide = FALSE)

# Adjusting Bar Width and Spacing
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")
# to have the bars narrower
ggplot(pg_mean, aes(x = group, y = weight)) + geom_bar(stat = "identity", width = 0.8)
# The width can be max 1 ... 
# Adding space in grouped bars
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity", position = "dodge", width = 0.5) # Overall width reduction
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.5) # Adding width between bars within groups

# Making a stacked bar graph
# We dont give the position = "dodge" here
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity") 
# according to the arrangement of the stacks, the default legends are reversed.. making corrections
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity") + guides(fill = guide_legend(reverse = TRUE))

# Adding Labels to a Bar Graph
ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) + geom_bar(stat = "identity") +
  geom_text(aes(label = Weight), vjust = 1.5, colour = "white")
# vjust (vertical justification), moves the text above or below the top of the bars
# To adjust ylim to be little higher
ggplot(cabbage_exp, aes(x = interaction(Date, Cultivar), y = Weight)) + geom_bar(stat = "identity") +
  geom_text(aes(label = Weight), vjust = 1.5, colour = "white") + ylim(0, max(cabbage_exp$Weight) * 1.1)
# add label text to grouped bars
ggplot(cabbage_exp, aes(x = Date, y = Weight, fill = Cultivar)) + 
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_text(aes(label = Weight), vjust = 1.5, colour = "white", position = position_dodge(0.9))

# Making Cleveland Dot Plot
# Cleveland dot plots are used instead of bar graphs because they reduce visual clutter and are easier to read
# The simplest way to create a dot plot is to use geom_point

tophit <- tophitters2001[1:25,] # Take the top 25 from the tophitters data set
ggplot(tophit, aes(x = avg, y = name)) + geom_point()
# by default the items on the axis is ordered and since name is character so it is ordered alphabetically
ggplot(tophit, aes(x = avg, y = reorder(name, avg))) + geom_point(size = 3)
# to swap the axis
ggplot(tophit, aes(x = reorder(name, avg), y = avg)) + geom_point(size = 2)
# in case the avg are in two groups
ggplot(tophit, aes(x = avg, y = reorder(name, avg))) + 
  geom_segment(aes(yend = name), xend = 0, colour = "grey50") +
  geom_point(size = 2, aes(colour = lg)) + 
  scale_color_brewer(palette = "Set1", limits = c("NL", "AL"), guide = FALSE) +
  facet_grid(lg ~., scales = "free_y", space = "free_y") + xlab("Average") + ylab("Names")