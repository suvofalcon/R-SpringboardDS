# ********************************************************************************************************
# Logistic Regression - Logistic Regression on the Loan Acceptance dataset
#  
# Data Used - UniversalBank.csv
# 
# The dataset has information on 5000 loan applications.
# The response is whether on not an offered loan has been accepted on an earlier location.
#
# Age of customer;
# Experience: professional experience in years;
# Income of customer;
# Family size of customer;
# CCAvg: average monthly credit card spending;
# Mortgage: size of mortgage;
# SecuritiesAccount: No/Yes;
# CDAccount: No/Yes;
# Online: No/Yes;
# CreditCard: No/Yes;
# Educational level: three categories (undergraduate, graduate, professional).    
# ********************************************************************************************************
options(scipen = 999) # Prevent automatic roundoffs

# Use of external libraries
library(ggplot2)
library(ROCR)
library(caret)

# Load the data from SUVOS-TIME-CAPS
switch(Sys.info() [['sysname']],
       Windows = {universalBank <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/UniversalBank.csv", header = TRUE)},
       Linux   = {universalBank <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/UniversalBank.csv", header = TRUE)},
       Darwin  = {universalBank <- read.csv("//Volumes/Data/CodeMagic/Data Files/LedolterDatasets/DataText/UniversalBank.csv", header = TRUE)})

# Check the data load
dim(universalBank)
str(universalBank)

# ********** Data Exploration and Data Preparation Block *************************************** #

# Check for missing values
sum(is.na(universalBank)) # There are no missing values

# We do not need the ID column in the regression analysis - dropping the column
universalBank$ID <- NULL
head(universalBank)

# We do not need the ZIP.code column in the regression analysis - dropping the column
universalBank$ZIP.Code <- NULL
head(universalBank)

# The variables which are discrete and has multiple levels has to be treated as factors
# For example Family and Education
unique(universalBank$Family)
unique(universalBank$Education)
# ********** Regression Model **************************************************** #

# We will use a generalized linear model here

logit_model <- glm(PersonalLoan ~ Age + Experience + Income + factor(Family) + CCAvg + 
                     factor(Education) + Mortgage + SecuritiesAccount + CDAccount + Online + 
                     CreditCard, data = universalBank, family = "binomial")
# check the model
summary(logit_model)

# Dropping the variable Age, Experience and Mortgage as they are not statistically significant in predicting
# accepting of PersonalLoan - re-run the model by dropping these variables
logit_model <- glm(PersonalLoan ~ Income + factor(Family) + CCAvg + 
                     factor(Education) + SecuritiesAccount + CDAccount + Online + 
                     CreditCard, data = universalBank, family = "binomial")
summary(logit_model)

# We observe for the Family variable only values 3 and 4 are significant. Which means for families whose
# number of members are 3 and 4 are statistically significant in predicting PersonalLoan acceptance variable

# Creating dummy indicator variable large family (1 for Members 3 and above, else 0)
universalBank$FamilyLarge <- ifelse(universalBank$Family >= 3, 1, 0)

# Re-run the regression, using these two indicator variables instead of Family Variable

logit_model <- glm(PersonalLoan ~ Income + FamilyLarge + CCAvg + 
                     factor(Education) + SecuritiesAccount + CDAccount + Online + 
                     CreditCard, data = universalBank, family = "binomial")
summary(logit_model)

# ********** Model Diagnostics **************************************************** #

# Lets understand how our predicted PersonalLoan acceptance varies with Income
ggplot() + geom_point(data = universalBank, aes(x = Income, y = PersonalLoan), col = "red", cex = 0.75) +
  geom_point(data = universalBank, aes(x = Income, y = fitted.values(logit_model)), col = "blue", cex = 0.75) + 
  ylab("PersonalLoan/Probabilities")
# This shows that there is a mild increase in probability for acceptance of PersonalLoan as the income 
# increases. However we see there are many rejects in PersonalLoan with higher income range as well...
# Which implies, Income is not a very strong criteria for acceptance of PersonalLoan

# With FamilyLarge
AvgProbLargeFamily <- aggregate(fitted.values(logit_model), list(universalBank$FamilyLarge), mean)
colnames(AvgProbLargeFamily) <- c("FamilyLarge", "AvgProbabilities")
ggplot(data = AvgProbLargeFamily, aes(x = factor(FamilyLarge), y = AvgProbabilities)) + geom_bar(stat = "identity") + 
  geom_text(aes(label = round(AvgProbabilities,3)), vjust = 1.5, colour = "white") + xlab("Family with Members 3 and Above") + ylab("Probabilities of Loan Acceptance")
# Acceptance of PersonalLoan - Average Probabilities is higher in case of Families with three or more members

# With Variable CCAvg
ggplot() + geom_point(data = universalBank, aes(x = CCAvg, y = PersonalLoan), col = "red", cex = 0.75) +
  geom_point(data = universalBank, aes(x = CCAvg, y = fitted.values(logit_model)), col = "blue", cex = 0.75) + 
  ylab("PersonalLoan/Probabilities")
# The above data is largely inconclusive and it is hard to conclude the probability of a PersonalLoan 
# Application in relation to avg credit card spending
# lets study the CCAvg variable in the dataset
ggplot(universalBank, aes(x = CCAvg), col = "darkgreen") + geom_histogram(binwidth = 0.25) + 
  xlab("Average CreditCard Spending") + ylab("Count")
# The average credit card spending variable is highly skewed (positively) and most of the customers is
# at a lower end of avg credit card spending.. hence direct correlation between Average Credit Card
# spending PersonalLoan Acceptance is not seen

# With Factor education
AvgProbEducation <- aggregate(fitted.values(logit_model), list(universalBank$Education), mean)
colnames(AvgProbEducation) <- c("Education","AvgProbabilities")
ggplot(data = AvgProbEducation, aes(x = factor(Education), y = AvgProbabilities)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(AvgProbabilities,3)), vjust = 1.5, colour = "white") + 
  xlab("Education Level") + ylab("Probabilities of Loan Acceptance")
# With Education Level - 3 the avg probabilities of loan acceptance is higher and then gradually
# declining with the level of education

# With Factor Securities Account
AvgProbSecurities <- aggregate(fitted.values(logit_model), list(universalBank$SecuritiesAccount), mean)
colnames(AvgProbSecurities) <- c("Securities","AvgProbabilities")
ggplot(data = AvgProbSecurities, aes(x = factor(Securities), y = AvgProbabilities)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(AvgProbabilities,3)), vjust = 1.5, colour = "white") + 
  xlab("Securities Account") + ylab("Probabilities of Loan Acceptance")
# Presence of a Securities Account, increases the PersonalLoan Acceptance probability

# With CD Account
AvgProbCD <- aggregate(fitted.values(logit_model), list(universalBank$CDAccount), mean)
colnames(AvgProbCD) <- c("CDAccount","AvgProbabilities")
ggplot(data = AvgProbCD, aes(x = factor(CDAccount), y = AvgProbabilities)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(AvgProbabilities,3)), vjust = 1.5, colour = "white") + 
  xlab("CD Account") + ylab("Probabilities of Loan Acceptance")
# Presence of Securities Account greatly increases the chances (probabilities) of accepting a personal loan

# With Online
AvgProbOnline <- aggregate(fitted.values(logit_model), list(universalBank$Online), mean)
colnames(AvgProbOnline) <- c("Online","AvgProbabilities")
ggplot(data = AvgProbOnline, aes(x = factor(Online), y = AvgProbabilities)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(AvgProbabilities,3)), vjust = 1.5, colour = "white") + 
  xlab("Online") + ylab("Probabilities of Loan Acceptance")
# Presence of Online account results in higher probability of Loan Acceptance

# With CreditCard
AvgProbCreditCard <- aggregate(fitted.values(logit_model), list(universalBank$CreditCard), mean)
colnames(AvgProbCreditCard) <- c("CreditCard","AvgProbabilities")
ggplot(data = AvgProbCreditCard, aes(x = factor(CreditCard), y = AvgProbabilities)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(AvgProbabilities,3)), vjust = 1.5, colour = "white") + 
  xlab("CreditCard") + ylab("Probabilities of Loan Acceptance")
# Presence of CreditCard shows a slightly higher probability of Loan Acceptance

# Now lets build the Confusion Matrix
confusionMatrix(round(fitted.values(logit_model)), universalBank$PersonalLoan)

# Plotting the performance graph and compute area under the curve
pred <- prediction(fitted.values(logit_model), universalBank$PersonalLoan)
perf <- performance(pred, "tpr", "fpr") # tpr = TP/P, fpr = FP/N
plot(perf, lwd = 2, col.lab = "blue", col = "dark green")

# to get the area under the curve
area_under_curve <- performance(pred, "auc")
area_under_curve <- unlist(slot(area_under_curve, "y.values"))
area_under_curve # area under the curve is 96.04%
