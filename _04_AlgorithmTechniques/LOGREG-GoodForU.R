# ********************************************************************************************************
# Logistic Regression - Significance of the specific questions in driving a brand perception.
#                       What is the probability given the answers that customer would prefer brand A
#  
# Data Used - goodforu-class12.csv(Data Dictionary "goodforu-variable-description-class12" was referred)
# 
# Manufacturer wants to determine the factors impacting only A's brands
# What is the overall brand perception based on the four questions
# Farm Grown Ingredients ?
# Trans Fat?
# Natural Oils?
# Impact due to processing level?
# ********************************************************************************************************
options(scipen = 999) # Prevent automatic roundoffs

# Use of external libraries
library(ROCR)
library(caret)
library(ggplot2)

# load the dataset from SUVOS-TIME-CAPS
switch(Sys.info() [['sysname']],
       Windows = {goodforu <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/goodforu.csv", header = TRUE)},
       Linux   = {goodforu <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/goodforu.csv", header = TRUE)},
       Darwin  = {goodforu <- read.csv("//Volumes/Data/CodeMagic/Data Files/goodforu.csv", header = TRUE)})

# Check the data load
dim(goodforu)
str(goodforu)
head(goodforu)

# ********** Data Exploration and Data Preparation Block **************************************************** #

# Lets check for missing values
sum(is.na(goodforu)) # there are no missing values

# Looking at the data dictionary we understand
# X23   - Response Variable (Overall brand perception for A - 10 good for u, 1 - bad for u)
# X2    - Predictor Variable - Made from Farm grown ingredients for Brand A (1 = Made from Farm Grown, 2 = Not Made from Farm Grown)
# X9    - Predictor Variable - Trans Fat for Brand A (1 = Absence of TransFat, 2 = Presence of TransFat)
# X16   - Predictor Variable - Presence of Natural Oils in Brand A (1 = Presence of Natural Oils, 2 = Absence of Natural Oils)
# X30   - Predictor Variable - Impact due to processing level for Brand A (10 = minimally Processed, 1 = Heavily Processed)

# We will create a subset with only these variables for modelling
goodforu.subset <- data.frame(goodforu$X2, goodforu$X9, goodforu$X16, goodforu$X30, goodforu$X23)
# Put some meaningful colnames
colnames(goodforu.subset) <- c("FarmGrown", "TransFat", "NaturalOils", "ProcessingLevel", "GoodForU")

# Generating a derived variable
# We will create a derived variable for our response called the "PreferenceIndicator"
# We Will set the PreferenceIndicator to 1 (customer prefers brand A) of GoodForU response (X23) is >= 6
#                 else to 0 (Customer do not prefer the brand A)
# We will then model this derived variable

goodforu.subset$PreferenceIndicator <- ifelse(goodforu.subset$GoodForU >= 6, 1, 0)
head(goodforu.subset)

# ********** Regression Model **************************************************** #

# We will use generalized linear model here
logit_model <- glm(PreferenceIndicator ~ FarmGrown + TransFat + NaturalOils + ProcessingLevel, 
                   data = goodforu.subset, family = "binomial")
# Check the glm results
logit_model
summary(logit_model)
# We see all four IDV's - FarmGrown, TransFat, NaturalOils and ProcessingLevel are statistically significant
# In other words the data is coming from a population where Null Hypothesis is not true

# The response variable of the glm model denotes logit scores and we calculate probability for the same
# For example Probability of Brand Preference for A where FarmGrown = 1, TransFat = 2, NaturalOils = 2,
# and ProcessingLevel = 4 is calculated as follows

# Lets do cases - Are not made of FarmGrown, Presence of TransFat and are not made up with Natural Oils
# and heavily processed
newdata <- data.frame(FarmGrown = 2, TransFat = 2, NaturalOils = 2, ProcessingLevel = 1)
predict(logit_model, newdata, type = "response")
# The Probability that customer would prefer brand A with the above conditions is 3.4%

# On the other hana - If it is made up of FarmGrown Ingredients, Do not have TransFat, Made up with Natural Oils
# and is minimally processed
newdata <- data.frame(FarmGrown = 1, TransFat = 1, NaturalOils = 1, ProcessingLevel = 10)
predict(logit_model, newdata, type = "response")
# The Probability that customer would prefer brand A with the above conditions is 84.1%

# ********** Model Diagnostics **************************************************** #

# To know how the model is fitting the data, we can have graphical approach

# With variable ProcessingLevel
plot(goodforu.subset$PreferenceIndicator ~ goodforu.subset$ProcessingLevel, col = "blue", cex = 0.75,
     pch = 19, main = "Prob of Brand A Preference vs Processing Level", xlab = "Processing Level", 
     ylab = "Probabilities")
points(fitted.values(logit_model) ~ goodforu.subset$ProcessingLevel, col = "red", cex = 0.75)
# It is clear from the point that as the processing level of Brand A changes from Heavily Processed (1)
# to minimally processed (10) - The probabilities of customer preferring brand A also increases.

# Using ggplot2
ggplot() + geom_point(data = goodforu.subset, aes(x = ProcessingLevel, y = PreferenceIndicator), 
                      col = "blue", cex = 0.75) + 
  geom_point(data = goodforu.subset, 
             aes(x = ProcessingLevel, y = fitted.values(logit_model)), col = "red", cex = 0.75) + 
  xlab("Processing Level") + ylab("Probabilities")


# With Variable FarmGrown
plot(goodforu.subset$PreferenceIndicator ~ goodforu.subset$FarmGrown, col = "blue", cex = 0.75, 
     pch = 19, main = "Prob of Brand A Preference vs Farm Grown Ingredients", xlab = "Farm Grown",
     ylab = "Probabilities")
points(fitted.values(logit_model) ~ goodforu.subset$FarmGrown, col = "red", cex = 0.75, type = "h")
plot(tapply(fitted.values(logit_model), goodforu.subset$FarmGrown, mean), ylim = c(0, 0.5), type = "l",
     ylab = "Probabilities", xlab = "Farm Grown", lwd = 2, col = "blue", 
     main = "Probabilities vs Farm Grown")
# It is clear from the above plot that as the Ingredients changes from Farm Grown (1) to non Farm Grown (2)
# The average probabilities of brand preference for A decreases as well..

# With Variable TransFat
plot(goodforu.subset$PreferenceIndicator ~ goodforu.subset$TransFat, col = "blue", cex = 0.75,
     pch = 19, main = "Prob of Brand A Preference vs Presence of Trans Fat", xlab = "Presence of Trans Fat",
     ylab = "Probabilities")
points(fitted.values(logit_model) ~ goodforu.subset$TransFat, col = "red", cex = 0.75, type = "h")
plot(tapply(fitted.values(logit_model), goodforu.subset$TransFat, mean), type = "l", lwd = 2, col = "blue",
     xlab = "Presence of Trans Fat", ylab = "Probabilities of Preference", main = "Probabilities vs Trans Fat",
     ylim = c(0, 0.5))
# It is clear from the above plot with the absence of TransFat (1) the probability of a customer preferring
# brand A is higher as compared to the presence of TransFat(2)... Gradually the the average probability
# decreases as TransFat is introduced

# With Variable Natural Oils
plot(goodforu.subset$PreferenceIndicator ~ goodforu.subset$NaturalOils, col = "blue", cex = 0.75,
     pch = 19, main = "Prob of Brand A Preference vs Presence of Natural Oils", xlab = "Presence of Natural Oils",
     ylab = "Probabilities")
points(fitted.values(logit_model) ~ goodforu.subset$NaturalOils, col = "red", cex = 0.75, type = "h")
plot(tapply(fitted.values(logit_model), goodforu.subset$NaturalOils, mean), type = "l", lwd = 2, col = "blue",
     xlab = "Presence of Natural Oils", ylab = "Probabilities of Preference", main = "Probabilities vs Natural Oils",
     ylim = c(0, 0.5))
# From the above graph it is clear that absence of Natural Oils (2) in the Brand A food causes lower
# average probabilities in brand preference as compared to presence of Natural Oils (1)
# No presence of Natural Oils the probability of customer preferring Brand A decreases

# to check the fitted values
head(fitted.values(logit_model))
# Build the confusion Matrix
confusionMatrix(round(fitted.values(logit_model)), goodforu.subset$PreferenceIndicator)

# Plotting the performance graph and compute area under the curve
pred <- prediction(fitted.values(logit_model), goodforu.subset$PreferenceIndicator)
perf <- performance(pred, "tpr", "fpr") # tpr = TP/P, fpr = FP/N
plot(perf, lwd = 2, col.lab = "blue", col = "dark green")

# to get the area under the curve
area_under_curve <- performance(pred, "auc")
area_under_curve <- unlist(slot(area_under_curve, "y.values"))
area_under_curve # area under the curve is 76.7%
