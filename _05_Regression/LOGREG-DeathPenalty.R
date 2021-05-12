# ********************************************************************************************************
# Logistic Regression - Logistic Regression on Death Penalty Dataset (Both with and Without Weights option)
#  
# Data Used - DeathPenalty.csv and DeathPenaltyOther.csv
# 
# Is the Death Penalty more likely if the victim is white?
# Is the death penalty more likely if the crime is horrible with more aggravating features?
# 
# Race of Victim (1 - White, 0 - Black)
# Aggravation Index (1 - least severe, 6 - Most Severe)
#
#                                   Death Penalty Dataset (Without Weights Option)
# ********************************************************************************************************
options(scipen = 999) # No automatic roundoffs

# Use of external libraries
library(ROCR)
library(caret)
library(ggplot2)

# load the dataset from SUVOS-TIME-CAPS
switch(Sys.info() [['sysname']],
       Windows = {deathPenalty <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/DeathPenalty.csv", header = TRUE)},
       Linux   = {deathPenalty <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/DeathPenalty.csv", header = TRUE)},
       Darwin  = {deathPenalty <- read.csv("//Volumes/Data/CodeMagic/Data Files/LedolterDatasets/DataText/DeathPenalty.csv", header = TRUE)})

# Check the data load
dim(deathPenalty)
str(deathPenalty)
head(deathPenalty)
unique(deathPenalty$Agg)
unique(deathPenalty$VRace)
unique(deathPenalty$Death)

# ********** Data Exploration and Data Preparation Block *************************************** #

# Check for missing values
sum(is.na(deathPenalty)) # There are no missing values

# ********** Regression Model **************************************************** #

# We will use a generalized model here
logit_model <- glm(Death ~ Agg + VRace, data = deathPenalty, family = "binomial")
# Check the glm results
summary(logit_model)

# We see all IDV's are statistically significant to predict the Likelihood of Death Penalty
# The response variable of the glm model denotes logit scores and we calculate probability for the same

# So the probability of Death Penalty if the Victim is Black and Aggravation of the crime is least
# newdata <- data.frame(Agg = 1, VRace = 0)
predict(logit_model, data.frame(Agg = 1, VRace = 0), type = "response")
# Probability is 0.5% if the Victim is Black and Aggravation of the crime is 1

# Probability if the Victim is White and Aggravation is max
# newdata <- data.frame(Agg = 6, VRace = 1)
predict(logit_model, data.frame(Agg = 6, VRace = 1), type = "response")
# Probability is 98.7% if the Victim is White and Aggravation of the crime is 6

# ********** Model Diagnostics **************************************************** #

# To understand, how the model is fitting the data, we can have a graphical approach

# With Variable Agg
plot(deathPenalty$Death ~ deathPenalty$Agg, col = "blue", pch = 19, cex = 0.75, xlab = "Aggravation of Crime",
     ylab = "Death Sentence Probabilities", main = "Death Sentence by Aggravation of Crime")
points(fitted.values(logit_model) ~ deathPenalty$Agg, col = "red", cex = 0.75)
# We would want to plot the probabilities for the Aggravation of crime by Race of the victims
# lets find the fitted values for Victims who are Black
# newdata <- data.frame(Agg = deathPenalty$Agg, VRace = 0)
fitted.black <- predict(logit_model, data.frame(Agg = deathPenalty$Agg, VRace = 0), type = "response")
# fitted values for Victims who are White
# newdata <- data.frame(Agg = deathPenalty$Agg, VRace = 1)
fitted.white <- predict(logit_model, data.frame(Agg = deathPenalty$Agg, VRace = 1), type = "response")

plot(fitted.black ~ deathPenalty$Agg, col = "black", cex = 0.75, type = "l", lwd = 2)
points(fitted.white ~ deathPenalty$Agg, col = "blue", cex = 0.75, type = "l", lwd = 2)

# using ggplot2
ggplot(deathPenalty, aes(x = Agg, colour = Race)) + geom_line(aes(y = fitted.black, colour = "black")) + geom_line(aes(y = fitted.white, colour = "white")) + 
  xlab ("Crime Aggravation") + ylab("Probablities") 
# It is clear from the above, that probability of death sentence for all levels of crime in case the victim is white is higher than in case(s) where victim is black

# With variable VRace
plot(deathPenalty$Death ~ deathPenalty$VRace, col = "blue", pch = 19, cex = 0.75, xlab = "Race of the Victim",
     ylab = "Death Sentence Probabilities", main = "Death Sentence by Race of the Victim")
points(fitted.values(logit_model) ~ deathPenalty$VRace, col = "red", cex = 0.75, type = "h")
AverageProbVictimRace <- aggregate(fitted.values(logit_model), list(deathPenalty$VRace), mean)
colnames(AverageProbVictimRace) <- c("VictimRace","AverageProbabilities")
ggplot(data = AverageProbVictimRace, aes(x = factor(VictimRace), y = AverageProbabilities)) + geom_bar(stat = "identity") + 
  geom_text(aes(label = round(AverageProbabilities,3)), vjust = 1.5, colour = "white") + xlab("Race of Victims") + ylab("Probabilities of Death Sentence")
# The above shows very clearly that Probability of a Death Sentence in case of White Victim is much higher than in case of a Black Victim

# Now lets build the Confusion Matrix
confusionMatrix(round(fitted.values(logit_model)), deathPenalty$Death)

# Plotting the performance graph and compute area under the curve
pred <- prediction(fitted.values(logit_model), deathPenalty$Death)
perf <- performance(pred, "tpr", "fpr") # tpr = TP/P, fpr = FP/N
plot(perf, lwd = 2, col.lab = "blue", col = "dark green")

# to get the area under the curve
area_under_curve <- performance(pred, "auc")
area_under_curve <- unlist(slot(area_under_curve, "y.values"))
area_under_curve # area under the curve is 96.04%


#                                   Death Penalty Dataset (With Weights Option)
# ***************************************************************************************************************************************************
# load the dataset from SUVOS-TIME-CAPS
switch(Sys.info() [['sysname']],
       Windows = {deathPenalty <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/DeathPenaltyOther.csv", header = TRUE)},
       Linux   = {deathPenalty <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/DeathPenaltyOther.csv", header = TRUE)},
       Darwin  = {deathPenalty <- read.csv("//Volumes/Data/CodeMagic/Data Files/LedolterDatasets/DataText/DeathPenaltyOther.csv", header = TRUE)})

head(deathPenalty)

# We follow the exact same approach as before and use the glm model as below (We took into account the weightage and Frequency)
logit_model <- glm(Death ~ Agg + VRace, data = deathPenalty, weights = Freq,family = "binomial")
summary(logit_model)
