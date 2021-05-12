# ********************************************************************************************************
# Logistic Regression - Demo using the GoodBad - Stripped off credit data from German Bank
# 
# Predict the Loan repayment ability of a customer using Credit History, Income Level and Employment Status 
# This uses a subset of the entire dataset and shows how to run a Logistic Regression
# ********************************************************************************************************
options(scipen = 999) # Prevent Automatic roundoffs

# Libraries
library(caret)
library(ROCR)
library(ggplot2)

# We will load the dataset from SUVOS-TIME-CAPS
switch(Sys.info() [['sysname']],
       Windows = {credit <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/GOODBAD.CSV", header = TRUE)},
       Linux   = {credit <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/GOODBAD.CSV", header = TRUE)},
       Darwin  = {credit <- read.csv("//Volumes/Data/CodeMagic/Data Files/GOODBAD.CSV", header = TRUE)})

# Check the data load
head(credit)
str(credit)
dim(credit)

# ********** Data Exploration and Data Preparation Block **************************************************** #

# Lets check some missing values
sum(is.na(credit)) # There are no missing values

# Now we will add some derived variables to perform the logistic regression

# CreditHistoryRating - 1,2,3,4,5 (1 being lowest and 5 being Highest)
# This is deduced from the variable CreditHistory
# .......... This Column needs understanding .... cross checking...
levels(credit$CreditHistory)
credit$CreditHistoryRating[credit$CreditHistory == "A30"] <- 1
credit$CreditHistoryRating[credit$CreditHistory == "A31"] <- 2
credit$CreditHistoryRating[credit$CreditHistory == "A32"] <- 3
credit$CreditHistoryRating[credit$CreditHistory == "A33"] <- 4
credit$CreditHistoryRating[credit$CreditHistory == "A34"] <- 5

# IncomeLevel - 1, 2, 3, 4 (1 being Lowest and 4 being Highest)
# This is deduced from the variable Check_Account_Status
levels(credit$Check_Account_Status)
credit$IncomeLevel[credit$Check_Account_Status == "A11"] <- 1
credit$IncomeLevel[credit$Check_Account_Status == "A12"] <- 2
credit$IncomeLevel[credit$Check_Account_Status == "A13"] <- 3
credit$IncomeLevel[credit$Check_Account_Status == "A14"] <- 4

# EmploymentStatus - 0, 1(0 - Employed, 1 - Unemployed)
# This is deduced from Job
levels(credit$Job)
credit$EmploymentStatus <- ifelse(credit$Job == "A171", 1, 0)

# Good Bad variable also needs to be corrected
# Deduced from Good.Bad variable (Good - 1, Bad - 0)
credit$Good.Bad <- ifelse(credit$Good.Bad == 1, 1, 0)

# We will now create a small subset on which we will run the logistic regression
# We will attempt to predict Good.Bad variable using CreditHistoryRating, IncomeLevel and EmploymentStatus only

credit.subset <- credit[,21:24]
str(credit.subset)
head(credit.subset)

# ********** Regression Model **************************************************** #

# run the glm regression (generalized linear model)
myresult <- glm(credit.subset$Good.Bad ~ credit.subset$CreditHistoryRating + credit.subset$IncomeLevel +
                  credit.subset$EmploymentStatus, family = "binomial")
# details of the generalized linear model
summary(myresult)
# We see EmploymentStatus is not statistically significant in predicting Good.Bad. Re-run the iteration
# dropping the EmploymentStatus variable

myresult <- glm(credit.subset$Good.Bad ~ credit.subset$CreditHistoryRating + credit.subset$IncomeLevel,
                family = "binomial")
summary(myresult)
# Deviance residuals are the residuals after performing logistic regression.
# They are calculated using the maximum likelihood function. They can have properties like regular residuals.
# For example - they can be symmetric

# The second set of output(s) are the coefficients. These we have seen in linear regression as well.
# The only difference is, in logistic regression we predict the logit score and not the outcome measures directly.

# For example looking at the CreditHistoryRating, we can say that it is very significant, in other words the data is
# coming from a population where Null Hypothesis (CreditHistory as no impact on Good.Bad) is not true. In other words
# CreditHistoryRating has an impact on the Good.Bad

# Logit Scores is not straight for interpretation and hence we convert them into odds for the same
# and then look at the probability values

# Now since only CreditHistoryRating and IncomeLevel is significant
# coefficient score for anyone non-defaulting for anyone with a creditHistoryRating of 5 = 0.39867 * 5 
# coefficient score for anyone with IncomeLevel 4 = 0.63059 * 4 
# to calculate the odds ratio
odds <- exp((5 * 0.39867)) * exp((0.63059 * 4))
odds
# Since Probability = Odds/(1 + odds)
prob <- odds/(1 + odds)
prob
# Hence the probability of a person not defaulting when IncomeLevel is 4 and CreditHistory Rating is 5 is 98.9%.. 

# to predict the Good.Bad against our dataset
predict(myresult, credit.subset, type = "response")

# Now going back to the summary results - after the coefficient values, we have Null deviance and Residual deviance
# Deviance are the errors, after the predictors have been accounted for. it is analogous to the residual sum of 
# squares in linear regression model..
# Null deviance shows the deviance of the model without any predictor and residual deviance shows, deviance
# with predictors.
# This are actually the chi-square values with the given degrees of freedom.

# Null deviance is the deviace without the presence of any explanatory variables (IDV's) and residual deviance is the deviance
# after the explanatory variables in the models (IDSV's) have been accounted for
# We see the degrees of freedom have been reduced after inclusion of the explanatory variables and even the deviances have been also reduced
# Akaike information criterion (AIC) - Is the relative measure of the quality of my model (how good or bad it is... )
# If we end up having number of models - we chose the one which has the least AIC score

# to check whether the model is useful or not, we simply need to calculate the chisq probability
pchisq(1221.7 - 1061.0, 999 - 997, lower.tail = FALSE) # since i am interested in right tail probability

# Since is p-value is very very small, it shows there is a statistically significant difference between the model
# with predictor and without predictors
# Or in other words, we can say that predictors are useful in predicting the non-defaulting behaviour of the customer

# To know how the model is fitting the data, we can have graphical approach
# for the variable CreditHistoryRating
plot(credit.subset$CreditHistoryRating, credit.subset$Good.Bad, col = "blue", pch = 19, cex = 1.5, main = "Probability of Non Defaulting with Credit History", xlab = "Credit History", ylab = "Non Defaulting (Probability)")
#Now to plot the fittedvalues from our model
points(credit.subset$CreditHistoryRating, fitted.values(myresult), col = "red", cex = 1.5)
# Use ggplot2
ggplot() + geom_point(data = credit.subset, aes(x = CreditHistoryRating, y = Good.Bad), col = "blue", cex = 0.75) +
  geom_point(data = credit.subset, aes(x = CreditHistoryRating, y = fitted.values(myresult)), col = "red", cex = 0.75) +
  xlab("Credit History Rating") + ylab("Probabilities - Good.Bad")

# similarly if we plot the InomeLevel with Good.Bad
plot(credit.subset$IncomeLevel, credit.subset$Good.Bad, col = "blue", pch = 19, cex = 1.5, main = "Probability of Non Defaulting with Income Level", xlab = "IncomeLevel", ylab = "Non Defaulting (Probability)")
points(credit.subset$IncomeLevel, fitted.values(myresult), col = "red", cex = 1.5)
# Use ggplot2
ggplot() + geom_point(data = credit.subset, aes(x = IncomeLevel, y = Good.Bad), col = "blue", cex = 0.75) +
  geom_point(data = credit.subset, aes(x = IncomeLevel, y = fitted.values(myresult)), col = "red", cex = 0.75) +
  xlab("Income Level") + ylab("Probabilities - Good.Bad")

# to check the fitted values
fitted.values(myresult)

# we will build the confusion matrix
library(caret)
confusionMatrix(round(fitted.values(myresult)), credit.subset$Good.Bad)

# Another approach 
predicted <- myresult$fitted.values
pred <- prediction(predicted, credit.subset$Good.Bad)
summary(pred)
perf <- performance(pred,"tpr", "fpr") # tpr = TP/P, fpr = FP/N
plot(perf)

# to get the area under the curve
auc <- performance(pred, "auc")
auc
auc <- unlist(slot(auc, "y.values"))
auc

