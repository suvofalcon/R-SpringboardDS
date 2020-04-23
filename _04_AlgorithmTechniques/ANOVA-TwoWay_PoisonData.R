# ************************************************************************************************************************
# ANOVA - Two way ANOVA
# This is also known as two factor ANOVA. 

# The analysis is based on a dataset Poison.txt.
# The dataset depicts survival time measured in animals, which had been given a poison and then were given an antidote
# The dataset depicts data for three different types of Poison and four types of treatment
# ************************************************************************************************************************

options(scipen = 999) # This will ensure no automatic rounding off will happen

# we will load the dataset (this is from SUVOS-TIME-CAPS)
switch(Sys.info() [['sysname']],
       Windows = {PoisonData <- read.table("//10.0.1.1/Data/CodeMagic/Data Files/Poison.txt", sep = "\t", header = TRUE)},
       Linux   = {PoisonData <- read.table("//10.0.1.1/Data/CodeMagic/Data Files/Poison.txt", sep = "\t", header = TRUE)},
       Darwin  = {PoisonData <- read.table("//Volumes/Data/CodeMagic/Data Files/Poison.txt", sep = "\t", header = TRUE)})

# The dataset is a balanced dataset, because for each combination of Poison and Treatments, we have the same number of measurements

# We will check the dataload
head(PoisonData)
str(PoisonData)
# convert the categorical variables into factors
PoisonData$Poison <- as.factor(PoisonData$Poison)
PoisonData$Treatment <- as.factor(PoisonData$Treatment)
# Check the levels of the cateorical variables
levels(PoisonData$Poison)
levels(PoisonData$Treatment)

# Some visualizations on the data
boxplot(PoisonData$Time ~ PoisonData$Poison * PoisonData$Treatment)
# There are 12 box plots because there are 12 combinations between three different Poison types and Four different treatments

# With a two way ANOVA in this case, we ask two questions - from objective perspective..

#1. Is there any difference in the mean survival times between the treatments with respect to a particular Poison
#2. Is there any difference in the mean survival times between the Poisons with respect to a particular treatment
#3. Is there any dependency of treatment levels with different types of poison and there combined effects on the survival time

# The effect of one factor on the variable of interest, irrespective of the other factor is known as the main effect
# The effect of different levels of one factor combined with different level of another factor on the dependent variable is interaction effect. Whether there exists any 
# dependency of one factor on the other

# Lets graphically understand the main effects

plot(PoisonData$Treatment, PoisonData$Time)
title(main = "Main Effects of Treatment", xlab = "Treatment Type", ylab = "Mean Survival Time")
plot(PoisonData$Poison, PoisonData$Time)
title(main = "Main Effects of Poison", xlab = "Poison Type", ylab = "Mean Survival Time")
# to graphically check the mean survival times between various treatments and poison type
barplot(tapply(PoisonData$Time, list(PoisonData$Treatment), mean))
title(main = "Average Survival time based on Treatments", xlab = "Treatment Type", ylab = "Mean Survival Time")
barplot(tapply(PoisonData$Time, list(PoisonData$Poison), mean))
title(main = "Average Survival time based on Poison", xlab = "Poison Type", ylab = "Mean Survival Time")
# combining two categorical variables we finally draw the interaction plot
# This will allow us to view the mean survival time for every combination of Levels of Treatment and Poison
interaction.plot(PoisonData$Treatment,PoisonData$Poison,PoisonData$Time, col = c("red", "blue", "green")) # The rule of thumb is if the lines cross, this suggests 
# interaction effect
# The data of the interaction plot can be arrived at by
tapply(PoisonData$Time, list(PoisonData$Treatment,PoisonData$Poison), mean)

# The above data analysis shows that there are main effects and interaction effect on the mean survival time. To get to conclusion, we now run the ANOVA
anova.model <- aov(PoisonData$Time ~ PoisonData$Treatment*PoisonData$Poison)
summary(anova.model)

# H01 - For Treatment the NULL Hypothesis is there is no effect on the treatment on mean survival time, irrespective of the Poison type being taken
# We reject this Null Hypothesis because of high statistical significance
# There is a Main effect of Treatment on mean Survival time

# H02 - For Poison the NULL Hypothesis is, there is no difference across the levels of Poison on mean survival time, irrespective of the treatment being taken
# We reject this NULL Hypothesis because of high Statistical Significance
# There are differences in Poison on the mean survival time

# H03 - There is no interaction effect on the Poison and Treatment. In other words there is no dependency between Poison and Treatment types.
# We fail to reject this Null hypothesis because of No statistical significance

# Based on this model, we can drop the interaction variable because of no statistical significance

# The next step of analysis is to look at pairwise comparison test between to see what level or combination is causing the mean to differ. This is similar to the TukeyHSD test of One way ANOVA
Tk.treatment <- TukeyHSD(anova.model, "PoisonData$Treatment")
Tk.treatment
# To have a visual display of TukeyHSD results, lets plot the TukeyHSD
plot(Tk.treatment, las = 1)
# We see from the above that combination has Treatment2-Treatment1 and Treatment4-Treatment1 and Treatment3-Treatment2 contributing to most of the difference in the mean survival time

Tk.poison <- TukeyHSD(anova.model, "PoisonData$Poison")
Tk.poison
# To have a visual display of TukeyHSD results, lets plot the TukeyHSD
plot(Tk.poison, las = 1)
# we see from the above that Combination pair of Poison3-Poison1 and Poison3-Poison2 contributing to most of the difference in the mean survival time

# Lets plot the residuals of the model and check for heteroscedasticity - 
# There should not be any pattern emerging out... variability should be same and sum of all residuals should be close to zero
plot(anova.model$residuals, main = "Residual Plot for the Anova Model", ylab = "Anova Model Residuals")
sum(anova.model$residuals)
