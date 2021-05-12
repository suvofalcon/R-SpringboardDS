# ********************************************************************************************************
# Multinomial Logistic Regression - Multinomial Logistic Regression on multiple orders of the Response Variable
# Response variable levels are not ordered
# 
# Data Used - hSchool.txt
# 
# Model which will estimate the probabilities of student chosing a program type
#  
# The predictor variables are ses (socio economic status), reading schore (read), 
# writing score (write) and maths score (math) read, write and math is continuous while ses is categorical
#
# ********************************************************************************************************
rm(list = ls()) # We will clear all variables created in the memory
options(scipen = 999) # No automatic round off of numbers

# Use of external libraries
library(foreign)
library(nnet)

# load the dataset from SUVOS-TIME-CAPS
switch(Sys.info() [['sysname']],
       Windows = {programoptions <- read.table("//10.0.1.1/Data/CodeMagic/Data Files/ProgramOptions.txt", header = TRUE)},
       Linux   = {programoptions <- read.table("//10.0.1.1/Data/CodeMagic/Data Files/ProgramOptions.txt", header = TRUE)},
       Darwin  = {programoptions <- read.table("//Volumes/Data/CodeMagic/Data Files/ProgramOptions.txt", header = TRUE)})

# Check the data load
head(programoptions, 10)
str(programoptions)

# ********** Data Exploration and Data Preparation Block *************************************** #

# Check for missing values
sum(is.na(programoptions)) # There are no missing values

# Lets check the distribution of the response variables
table(programoptions$prog) # honors data has the highest frequencies and diploma and general has same distribution proportions

# ********** Regression Model **************************************************** #

# Before we run the regression, we need to relevel the response variable and chose a reference level.
# The output of other levels will be compared with the reference level
programoptions$prog <- relevel(programoptions$prog, ref = "honors")

# now we run the regression
logit_model <- multinom(prog ~ ses + read + write + math, data = programoptions)
logit_model
# Check the summary on the Ordered Logistic
summary(logit_model)

# Lets attempt to make a prediction using some test data
newdata <- data.frame(ses = c("low", "middle", "high"), read = mean(programoptions$read), write = mean(programoptions$write), 
                      math = mean(programoptions$math))
# Let us check the test data
newdata
# run the prediction on the test data
predict(logit_model, newdata, type = "probs")
# The above test shows that with the same read, write and math scores, what could be the difference between the socio economic status

# To verify if the probabilities we get has any statistical significance - for example changing any values of the independent variables causes 
# any statistical significance to the dependent variable

# now we know that if 0 lies in the confidence interval then alpha is not statistically significant (In a z distribution, mean = 0)
# since we are finding exp (and exp 0 = 1), we should see that 1 does not lie within confidence interval (2.5 % to 97.5% - performing a two - tail t test)

# lets evaluate which predictor variable do not cause statistical significance and can be eliminated from the model 
# calculate the confidence intervals
exp(confint(logit_model))

# looking at the confidence interval, we see for Diploma
# seslow, read and write is NOT statistically significant

# For General
# Seslow, sesmiddle, read and write is NOT statistically significant

# to understand the impact of the predictor variables which are statistically significant
exp(coef(logit_model))

# Explaining only for variables which are statistically significant in respective response level

# Diploma
# ----------
# For Math coefficient for Diploma is less than 1 - So if the Math score changes by 1 unit positively, the person is more likely
# to opt for a Honors degree than Diploma degree (all comparisons with Honors - since it is reflevel)
# Since the Sesmiddle coeffient is greater than 1 - So if the Socio economic status of the person is middle, then it more likely
# the person will opt for a Diploma degree as compared to Honors degree (all comparisons with Honors - since it is reflevel)

# General
# ----------
# Similarly Since the Math coefficient for general is also less than 1 - So if the Math score changes by 1 unit positively, the person is more likely
# to opt for a Honors degree than General degree (all comparisons with Honors - since it is reflevel)

# Re-run the final model

logit_model <- multinom(prog ~ ses + math, data = programoptions)
summary(logit_model)
