# ************************************************************************************************************************
# Scatter Plots
# 
# This demonstrates Scatter Plots in detail using ggplot2
# Scatter plots are used to display the relationship between two continuous variables. 
# ************************************************************************************************************************
options(scipen = 999)

# Use of external libraries
library(ggplot2)
library(gcookbook)
library(MASS)

# Creating a Basic Scatter Plot
# ***************************************************************************
data("heightweight")
head(heightweight, 10)
# Plotting the scatter
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + geom_point()
# to use different shape and size in scatter plot
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + geom_point(shape = 21, size = 1.5)

# grouping datapoints by variable using shape and color
ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = sex)) + geom_point() # by colour
ggplot(heightweight, aes(x = ageYear, y = heightIn, shape = sex)) + geom_point() # by shape
ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = sex, shape = sex)) + geom_point() # map both

# We can further categorise the weight - whether the child weighed 100 pounds or more and add a column
# make a copy of the data
hw <- heightweight
hw$weightGroup <- ifelse(hw$weightLb >= 100, '>=100','<100')
# Now we will plot the data
ggplot(hw, aes(x = ageYear, y = heightIn, shape = sex, fill = weightGroup, colour = weightGroup)) + 
  geom_point()


# Mapping a Continuous Variable to Color or Size # using the continuous variable weightLb
ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) + geom_point()
ggplot(heightweight, aes(x = ageYear, y = heightIn, size = weightLb)) + geom_point()
# we can combine both
ggplot(heightweight, aes(x = ageYear, y = heightIn, size = weightLb, colour = weightLb)) + geom_point()

# If we use a categorical variable in the colour
ggplot(heightweight, aes(x = ageYear, y = heightIn, size = weightLb, colour = sex)) + geom_point()
# we see there is a significant amount of overlapping.. so we introduce transparency  by setting alpha = 0.5
ggplot(heightweight, aes(x = ageYear, y = heightIn, size = weightLb, colour = sex)) + geom_point(alpha = 0.5)
# We will scale the area to make the points proportional to the value and add a new colour palette
ggplot(heightweight, aes(x = ageYear, y = heightIn, size = weightLb, colour = sex)) + geom_point(alpha = 0.5) +
  scale_size_area() + scale_colour_brewer(palette="Set1")

# Dealing with Overplotting
# ***************************************************************************
# With large data sets, the points in a scatter plot may obscure each other and prevent the viewer from 
# accurately assessing the distribution of the data. Techniques to prevent overplotting

# A common overplotting case
data("diamonds")
ggplot(diamonds, aes(x = carat, y = price)) + geom_point()

# First solution to overplotting could be to introduce transparency
ggplot(diamonds, aes(x = carat, y = price)) + geom_point(alpha = 0.1) # introduce 90% transparency
# we see that the concentration is pretty dark at the lower end, which tells us the majority diamonds in the
# dataset are low priced.

# Second solution is to bin the points into rectangles and map the density of the points to fill the color of
# the rectangles... we use the function stat_bin_2d() which divides the space into 30 groups by default
ggplot(diamonds, aes(x = carat, y = price)) + geom_point(alpha = 0.1) + stat_bin_2d()

# Overplotting can also occur when the data is discrete on one or both axes
data("ChickWeight")
ggplot(ChickWeight, aes(x = Time, y = weight)) + geom_point()
# We can try to resolve the overlapping by randomly jitter the points with position_jitter  
ggplot(ChickWeight, aes(x = Time, y = weight)) + geom_point(position = "jitter")
# we can control the width the height of the jitter
ggplot(ChickWeight, aes(x = Time, y = weight)) + geom_point(position = position_jitter(width = 0.8, height = 0.8))

# Adding Fitted Regression Model Lines
# ***************************************************************************

# the base plot
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + geom_point()
# to fit the regression line, we use stat_smooth
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + geom_point() + stat_smooth(method = lm)
# By default the regression line gets showed with 95% confidence interval
# to add a 99% confidence region
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + geom_point() + stat_smooth(method = lm, level = 0.99)
# No confidence region
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + geom_point() + stat_smooth(method = lm, se = FALSE)
# Seggregating between male and female
ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = sex)) + geom_point() + stat_smooth(method = lm)
# we see that the blue line representing male is shorter than the red line which represents the female. By default
# the extrapolation will happen till the last value in the respective bucket. If we want the extrapolation to happen
# till the last value of the range, we use fullrange = TRUE
ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = sex)) + geom_point() + geom_smooth(method = lm, fullrange = TRUE)

# By default the stat_smooth method uses the loess (locally weighted polynomial) method to plot the curve
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + geom_point() + stat_smooth() # OR
ggplot(heightweight, aes(x = ageYear, y = heightIn)) + geom_point() + stat_smooth(method = loess)

data("biopsy")
b <- biopsy
# we would add a variable which is a numeric representation of class variable
# class = benign - value 0 - Does not have cancer
# class = malignant - value 1 - Have cancer
b$classn[b$class == "benign"] <- 0
b$classn[b$class == "malignant"] <- 1
# Check the data
head(b)
# Now we can use the stat_smooth method to plot a generalized linear model with family binomial
ggplot(b, aes(x = V1, y = classn)) + geom_point(position = position_jitter(width = 0.8, height = 0.8), alpha = 0.4) + 
  stat_smooth(method = glm, method.args = list(family = "binomial"))


# Adding Fitted lines from an existing model
# ***************************************************************************

# Sometimes we may want to do the model yourself and add it to the graph
# Lets build a polynomial model and use predict() function for graph fitment

model <- lm(heightIn ~ ageYear + I(ageYear^2), heightweight)
model
summary(model)
# Use this model to calculate the predicted values of height
xmin <- min(heightweight$ageYear)
xmax <- max(heightweight$ageYear)
predictedFrame <- data.frame(ageYear = seq(xmin, xmax, length.out = nrow(heightweight)))
# calculate the predicted values of the height
predictedFrame$heightIn <- predict(model, predictedFrame)
# Now lets check the data frame
head(predictedFrame)
# Now lets plot the data
ggplot(heightweight, aes(x = heightIn, y = ageYear)) + geom_point(colour = "red") + geom_line(data = predictedFrame)

# The model summary shows adjusted R-squared to be 0.4389 .. to add that into the plot as annotation
ggplot(heightweight, aes(x = heightIn, y = ageYear)) + geom_point(colour = "red") + geom_line(data = predictedFrame) +
  annotate("text", label = "Adjusted r^2 = 0.4389", x = 70, y = 11)

# Labelling Points in a scatter plot
# ***************************************************************************

# If we want to label points in a scatter plot
data("countries")
# create a small subset
subset.countries <- subset(countries, Year == 2009 & healthexp > 2000)
ggplot(subset.countries, aes(x = healthexp, y = infmortality)) + geom_point() + geom_text(aes(label = Name), size = 3)
# Because the label overlaps the points, we display is at slight offset
ggplot(subset.countries, aes(x = healthexp, y = infmortality)) + geom_point() + 
  geom_text(aes(y = infmortality+0.3, label = Name), size = 3, vjust = 0)

# to plot specific points
ggplot(subset.countries, aes(x = healthexp, y = infmortality)) + geom_point() + 
  annotate("text", x = 4350, y = 5.4, label = "Canada") + annotate("text", x=7400, y=6.8, label="USA")

# Creating Balloon Plots
# **************************************************************************

# In balloon plots the area of the dots is proportional to their numerical value
# lets use the countries data set - we will use a subset

cdat <- subset(countries, countries$Year == 2009 & Name %in% c("Canada", "Ireland", "United Kingdom", 
                                                               "United States",
                                                               "New Zealand", "Iceland", "Japan", "Luxembourg",
                                                               "Netherlands", "Switzerland"))
head(cdat)

# Plot of healthexp and infmortality
ggplot(data = cdat, aes(x = healthexp, y = infmortality, size = GDP)) + geom_point(shape = 21, colour = "Black",
                                                                                   fill = "cornsilk")
# if the points are looking small we can enlarge them
ggplot(data = cdat, aes(x = healthexp, y = infmortality, size = GDP)) + geom_point(shape = 21, colour = "Black",
                                                                                   fill = "cornsilk") +
  scale_size_area(max_size = 15)