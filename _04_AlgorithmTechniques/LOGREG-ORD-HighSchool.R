# ********************************************************************************************************
# Ordinal Logistic Regression - Ordinal Logistic Regression on ordered levels of the Response Variable
#  
# Data Used - hSchool.txt
# 
# In the following example we consider data for likelihood for applying to college.
# Outcomes are in a ordered manner - UnLikely(1), Slight Likely(2), Likely(3)
# 
# The predictor variables are GPA, Parent Has a College Degree, present high school is public
# GPA is continuous and Parent Education and Public is dichotomous
#
# ********************************************************************************************************
rm(list = ls()) # We will clear all variables created in the memory
options(scipen = 999) # No automatic round off of numbers

# Use of external libraries
library(MASS)

# load the dataset from SUVOS-TIME-CAPS
switch(Sys.info() [['sysname']],
       Windows = {hschool <- read.table("//10.0.1.1/Data/CodeMagic/Data Files/hSchool.txt", sep = "\t", header = TRUE)},
       Linux   = {hschool <- read.table("//10.0.1.1/Data/CodeMagic/Data Files/hSchool.txt", sep = "\t", header = TRUE)},
       Darwin  = {hschool <- read.table("//Volumes/Data/CodeMagic/Data Files/hSchool.txt", sep = "\t", header = TRUE)})

# Check the data load
head(hschool, 10)
str(hschool)

# ********** Data Exploration and Data Preparation Block *************************************** #

# Check for missing values
sum(is.na(hschool)) # There are no missing values

# The response variable should be a factor variable
hschool$Apply <- as.factor(hschool$Apply)

# Lets check the distribution of the response variables
table(hschool$Apply) # Most of the data concerns students who are unlikely to apply to college

# ********** Regression Model **************************************************** #

logit_model <- polr(Apply ~ pared + public + gpa, data = hschool, Hess = TRUE)
logit_model
# Check the summary on the Ordered Logistic
summary(logit_model)

# If we want to calculate the probabilities of the respective level for Pared = 0, public = 1 and gpa = 3.7
n1 <- data.frame(pared = 0, public = 1, gpa = 3.26)
predict(logit_model, n1, type = "probs")

# To verify if the probabilities we get has any statistical significance - for example changing any values of the independent variables causes 
# any statistical significance to the dependent variable

# now we know that if 0 lies in the confidence interval then alpha is not statistically significant (In a z distribution, mean = 0)
# since we are finding exp (and exp 0 = 1), we should see that 1 does not lie within confidence interval (2.5 % to 97.5% - performing a two - tail t test)

exp(cbind(OR = coef(logit_model), confint(logit_model))) # OR = odds ratio

# we see for variable public - 1 lies within the confidence interval and hence we can conclude that variable public has no statistical significance 
# in explaining the variations in the response variable and hence that variable can be dropped from the model and re-run the iteration

logit_model <- polr(Apply ~ pared + gpa, data = hschool, Hess = TRUE)
logit_model
# Check the summary on the Ordered Logistic
summary(logit_model)

# If we want to calculate the probabilities of the respective level for Pared = 0 and gpa = 3.7
n1 <- data.frame(pared = 0, gpa = 3.7)
predict(logit_model, n1, type = "probs")

# Explanations for the odds ratio
exp(cbind(OR = coef(logit_model), confint(logit_model))) # OR = odds ratio

# It can be explained as if the parents education goes from 0 to 1 the odds ratio improves by 2.854 for Apply(1) to Apply(2) and Apply(3)
# Which by business terms means - if the Parents is educated then the odds ratio of a student applying to college improves by 2.85 from Unlikely to
# Slightly Likely and improves by 2.85 from Unlikely to Likely
