# ********************************************************************************************************
# Multinomial Logistic Regression - Multinomial Logistic Regression on multiple orders of the Response Variable
# Response variable levels are not ordered
# 
# Data Used - ForensicGlass Dataset
# 
# Model which will estimate the probabilities of the glass types
#  
# We analyze the data set of 214 glass shards of six possible glass types
# based on the available measurements on shard characteristics including Refractive index (RI)
# What are the probabilities that the glass belong to each of the types?
#
# ********************************************************************************************************
rm(list=ls()) # remove the existing objects
options(scipen = 8) # no automatic round offs of numbers

# use of external libraries
library(foreign)
library(nnet)
library(MASS)

# use the built in forensic glass dataset
data(fgl)
head(fgl, 10)
str(fgl)

# ********** Data Exploration and Data Preparation Block *************************************** #

# Check for missing values
sum(is.na(fgl)) # There are no missing values

# Lets check the distribution of the response variables
table(fgl$type) # the distribution is very uneven ...
plot(fgl$type)

# ********** Regression Model **************************************************** #

# Before we run the regression, we need to relevel the response variable and chose a reference level.
# The output of other levels will be compared with the reference level
fgl$type <- relevel(fgl$type, ref = "WinNF")

# Now we run the regression
logit_model <- multinom(type ~ RI + Na + Mg + Al + Si + K + Ca + Ba + Fe, data = fgl)
summary(logit_model)

# Lets attempt to make a prediction using some test data
newdata <- data.frame(RI = mean(fgl$RI), Na = mean(fgl$Na), Mg = mean(fgl$Mg), Al = mean(fgl$Al),
                      Si = mean(fgl$Si), K = mean(fgl$K), Ca = mean(fgl$Ca), Ba = mean(fgl$Ba),
                      Fe = mean(fgl$Fe))
# Let us check the test data
newdata
# run the prediction on the test data
predict(logit_model, newdata, type = "probs")
# This depicts that there is a 83.7% probability that the type is WinNF

# lets evaluate which predictor variable do not cause statistical significance and can be eliminated from the model 
# calculate the confidence intervals
exp(confint(logit_model))
# We see from the above that, atleast all variables are significant in predicting atleaest one of the levels
# of glass type and hence we keep all the predictor variables

# to understand the impact of the predictor variables which are statistically significant
exp(coef(logit_model))

# Some interpretations
# If there is a 1 unit increment of Fe content, it is more likely the glass type to be WinNF when compared to WinF
# Similarly if there is an once increment in Ba content, it is more likely the glass type be Veh when compared to 
# WinNF

# all comparisions should be with WinNF - which is the reference level..