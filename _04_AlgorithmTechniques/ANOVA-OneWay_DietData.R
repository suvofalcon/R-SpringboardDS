# ************************************************************************************************************************
# ANOVA - One Way Anova

# This is also known as single factor ANOVA. ANOVA is a statistical method used to compare means of two or more 
# independent groups.
# The variable of interest of the dependent variable has to be a continuous random variable. It is a parametric method
# and makes assumptions that samples are independent and the underlying population is normally distributed.
# ************************************************************************************************************************
options(scipen = 999) # This will ensure no automatic rounding off will happen

# We will work with a dataset related to weight loss with relation to various diets

# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems
switch(Sys.info() [['sysname']],
       Windows = {DietData <- read.table("//10.0.1.1/Data/CodeMagic/Data Files/DietWeightLoss.txt", sep = "\t", header = TRUE)},
       Linux   = {DietData <- read.table("//10.0.1.1/Data/CodeMagic/Data Files/DietWeightLoss.txt", sep = "\t", header = TRUE)},
       Darwin  = {DietData <- read.table("//Volumes/Data/CodeMagic/Data Files/DietWeightLoss.txt", sep = "\t", header = TRUE)})

# Verify the data load
head(DietData)
str(DietData) # Explore the data types

# Weight loss is the dependent variable
class(DietData$WeightLoss)
# Diet is the independent or the explanatory variable which is discrete or categorical
levels(DietData$Diet) # there are four levels

# We will run a one way anova to understand whether different diets have any effect on the weight loss or the weight loss of the individuals are random

# Null Hypothesis (ho) - Diets have no effect on weight loss (Or Mean weight loss is same for every diet)
# Alternate Hypothesis - Diets have effects on weight loss

# Before we run anova, it may be useful to have a quick graphical views of the data
boxplot(DietData$WeightLoss~DietData$Diet)

# Now lets run ANOVA
DietData.ANOVA <- aov(DietData$WeightLoss ~ DietData$Diet)

# Seeing the detailed output
summary(DietData.ANOVA)

# Lets understand the summary object in detail
# We see the meanSquareBetween (Between Group Variances/Degrees of Freedom) is 32.44 and MeanSquareWithin is 5.30, giving us a 
# ANOVA Statistic(F Statistic) of 6.118

# The p-value (Probability of Random Chance outcome) is 0.00113 which is way less than 5% significance level and hence
# We reject the NULL HYPOTHESIS and conclude that Diet Type have effects on WeightLoss and Mean weight loss is different for different diet
# OR not all Means are equal

# Now to understand, which Means or diet may differ from the others - We can run PostHoc Tests and one such is TukeyHSD test

TukeyHSD(DietData.ANOVA)
# This returns at an overall 95% confidence level, difference in means between all possible pairs and an adjusted p-value

# To have a visual display of TukeyHSD results to understand the difference in mean between all possible pairs, or what diet type causes how much
# of the mean to differ we can use
plot(TukeyHSD(DietData.ANOVA), las = 1)

# From both the plots and TukeyHSD test, we see that C-A and C-B are the ones which is contributing to most of the difference in the 
# means

# ************************************************************************************************************************

# Now we will approach the same problem using Kruskal Wallis One Way of Analysis of Variance which is a non-parametric equivalent of the
# One way Analysis of Variance

# To conduct this test we use
DietData.Kruskal <- kruskal.test(DietData$WeightLoss ~ DietData$Diet)
# To see the results
DietData.Kruskal # We see the same p-value which is highly significant and hence we reject the NULL Hypothesis

# Kruskal Wallis is a non-parametric test, which means this doesnt make any assumptions on the underlying probability distributions of the
# population.
# ************************************************************************************************************************
# ANOVA - One Way Anova

# This is also known as single factor ANOVA. ANOVA is a statistical method used to compare means of two or more 
# independent groups.
# The variable of interest of the dependent variable has to be a continuous random variable. It is a parametric method
# and makes assumptions that samples are independent and the underlying population is normally distributed.
# ************************************************************************************************************************
options(scipen = 999) # This will ensure no automatic rounding off will happen

# We will work with a dataset related to weight loss with relation to various diets

# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems
switch(Sys.info() [['sysname']],
       Windows = {DietData <- read.table("//10.0.1.1/Data/CodeMagic/Data Files/DietWeightLoss.txt", sep = "\t", header = TRUE)},
       Linux   = {DietData <- read.table("//10.0.1.1/Data/CodeMagic/Data Files/DietWeightLoss.txt", sep = "\t", header = TRUE)},
       Darwin  = {DietData <- read.table("//Volumes/Data/CodeMagic/Data Files/DietWeightLoss.txt", sep = "\t", header = TRUE)})

# Verify the data load
head(DietData)
str(DietData) # Explore the data types

# Weight loss is the dependent variable
class(DietData$WeightLoss)
# Diet is the independent or the explanatory variable which is discrete or categorical
levels(DietData$Diet) # there are four levels

# We will run a one way anova to understand whether different diets have any effect on the weight loss or the weight loss of the individuals are random

# Null Hypothesis (ho) - Diets have no effect on weight loss (Or Mean weight loss is same for every diet)
# Alternate Hypothesis - Diets have effects on weight loss

# Before we run anova, it may be useful to have a quick graphical views of the data
boxplot(DietData$WeightLoss~DietData$Diet)

# Now lets run ANOVA
DietData.ANOVA <- aov(DietData$WeightLoss ~ DietData$Diet)

# Seeing the detailed output
summary(DietData.ANOVA)

# Lets understand the summary object in detail
# We see the meanSquareBetween (Between Group Variances/Degrees of Freedom) is 32.44 and MeanSquareWithin is 5.30, giving us a 
# ANOVA Statistic(F Statistic) of 6.118

# The p-value (Probability of Random Chance outcome) is 0.00113 which is way less than 5% significance level and hence
# We reject the NULL HYPOTHESIS and conclude that Diet Type have effects on WeightLoss and Mean weight loss is different for different diet
# OR not all Means are equal

# Now to understand, which Means or diet may differ from the others - We can run PostHoc Tests and one such is TukeyHSD test

TukeyHSD(DietData.ANOVA)
# This returns at an overall 95% confidence level, difference in means between all possible pairs and an adjusted p-value

# To have a visual display of TukeyHSD results to understand the difference in mean between all possible pairs, or what diet type causes how much
# of the mean to differ we can use
plot(TukeyHSD(DietData.ANOVA), las = 1)

# From both the plots and TukeyHSD test, we see that C-A and C-B are the ones which is contributing to most of the difference in the 
# means

# ************************************************************************************************************************

# Now we will approach the same problem using Kruskal Wallis One Way of Analysis of Variance which is a non-parametric equivalent of the
# One way Analysis of Variance

# To conduct this test we use
DietData.Kruskal <- kruskal.test(DietData$WeightLoss ~ DietData$Diet)
# To see the results
DietData.Kruskal # We see the same p-value which is highly significant and hence we reject the NULL Hypothesis

# Kruskal Wallis is a non-parametric test, which means this doesnt make any assumptions on the underlying probability distributions of the
# population.
