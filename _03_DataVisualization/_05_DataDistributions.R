# ************************************************************************************************************************
# Summarized Data Distributions
# 
# This demonstrates xplores how to visualize summarized distributions of data using ggplot2
# ************************************************************************************************************************
options(scipen = 999) # No automatic roundoffs
rm(list = ls()) # clear all environment variables

# use of libraries
library(ggplot2)
library(gcookbook)
library(MASS)

# Making a Basic Histogram
# ***************************************************************************

# We will use geom_histogram and map a continuous variable to x
data("faithful")
ggplot(faithful, aes(x = waiting)) + geom_histogram(binwidth = 3, fill = "white", colour = "black")
# sometimes the shape of the histogram will depend on the binwidth

# Making multiple histograms from grouped data
data("birthwt")
head(birthwt)
# we plot histogram for bwt with smoke as a facet variable
ggplot(data = birthwt, aes(x = bwt)) + geom_histogram(binwidth = 30, fill = "skyblue", colour = "black") +
  facet_grid(smoke ~.)
# to change the labels to more meaningful
# copy the data
birthwt1 <- birthwt
birthwt1$smokeLabel <- ifelse(birthwt$smoke == 0, "No Smoke","Smoke")
head(birthwt1)
# we again plot histogram for bwt with smokeLabel as a facet variable
ggplot(data = birthwt1, aes(x = bwt)) + geom_histogram(binwidth = 30, fill = "skyblue", colour = "black") +
  facet_grid(smokeLabel ~.) + xlab("Birth Weight of Babies")

# Making a Density Curve
# ***************************************************************************

# to map a continuous variable we use a density curve
ggplot(data = faithful, aes(x = waiting)) + geom_density()
# to increase the range of y to include 0, we use expand limits
ggplot(data = faithful, aes(x = waiting)) + geom_line(stat = "density") + expand_limits(y = 0)
# to add some decorations to the density curve
ggplot(data = faithful, aes(x = waiting)) + geom_density(fill = "lightblue") + geom_line(stat = "density")  +
  expand_limits(y = 0)

# We can overlay the density curve on the histogram
ggplot(data = faithful, aes(x = waiting, y = ..density..)) + 
  geom_histogram(binwidth = 3, fill = "white", colour = "black") +
  geom_density(fill = "lightblue", alpha = 0.2)

# Making multiple density curves from grouped data
# plot the density of birthweight by smokeLabel
ggplot(data = birthwt1, aes(x = bwt, fill = smokeLabel)) + geom_density(alpha= 0.2) # set transparency to 80%
# to plot the above in two grids we use
ggplot(data = birthwt1, aes(x = bwt)) + geom_density() + facet_grid(smokeLabel ~.)
# to see the histograms along with the density curves
ggplot(data = birthwt1, aes(x = bwt, y = ..density..)) + 
  geom_histogram(binwidth = 300, fill = "white", colour = "black") + geom_density(fill = "skyblue", alpha = 0.1) + 
  facet_grid(smokeLabel ~.)

# Making a Frequency Polygon
# ***************************************************************************
ggplot(data = faithful, aes(x = waiting)) + geom_freqpoly()
# we can control the bin width as well
ggplot(data = faithful, aes(x = waiting)) + geom_freqpoly(binwidth = 3)

# Making a Density Plot of Two-Dimensional Data
# ***************************************************************************
# we use the function stat_density2d()
ggplot(data = faithful, aes(x = eruptions, y = waiting)) + geom_point() + stat_density2d()

# Now it is possible to map the height of the density curve to the colour of the contour lines
ggplot(data = faithful, aes(x = eruptions, y = waiting)) + geom_point() + stat_density2d(aes(colour = ..level..))
# if we dont want to see the contour lines and wish to view differently
ggplot(data = faithful, aes(x = eruptions, y = waiting)) + geom_point() + 
  stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE)

# To add text annotation to a plot
# ***************************************************************************
ggplot(data = faithful, aes(x = eruptions, y = waiting)) + geom_point() + 
  annotate("text", x = 3, y = 48, label = "Group1") + annotate("text", x = 4.5, y = 66, label = "Group2")

# change some cosmetics of annotations
ggplot(data = faithful, aes(x = eruptions, y = waiting)) + geom_point() + 
  annotate("text", x = 3, y = 48, label = "Group1", family = "serif", fontface = "italic", 
           colour = "darkred", size = 5) + 
  annotate("text", x = 4.5, y = 66, label = "Group2", family = "serif", fontface = "italic", 
           colour = "darkred", size = 5)