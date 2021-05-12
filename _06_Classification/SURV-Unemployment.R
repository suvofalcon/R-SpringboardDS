# ********************************************************************************************************
# Survival Analysis - Demo of Survival Analysis
#  
# Data Used - survival_unemployment.csv
# 
# The problem to consider here is unemployment
# We are looking individuals who are likely to find employment as early as possible. 
# The variables we are interested in
#
# Data Structure
#
# Input Variables...
# ui                      - Whether or not the person has unemployment insurance (1 = has insurance, 0 = otherwise)
# logwage                 - Continuous variable
# age                     - Age of the individual
#
# Output Variables...
# Spell                   - Number of Periods that the person is unemployed
# event                   - Whether the person has found a job (1 = found a job, 0 = still unemployed)
#
# Any customer who have not yet made the purchase is not a non-responder but a censored data.

# Censored data is an observation for whom the actual response tag is unknown. 
# So all the individuals who did not get a job along with the period being unemployed, is the censored observation.
# These should not be treated as a non-responder but a censored observation. 
# Other direct techniques like logistic regression cannot take censored data into the model. 
# However, survival analysis has a capability to take them into account.
# ********************************************************************************************************

rm(list = ls()) # Clear the existing environment variables
options(scipen = 999) # No automatic roundoffs

# Usage of external libraries
library(survival)

# we will load the survival_unemployment.csv 
# we will load the dataset (this is from SUVOS-TIME-CAPS)
switch(Sys.info() [['sysname']],
       Windows = {unemployed_data <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/survival_unemployment.csv", header = TRUE)},
       Linux   = {unemployed_data <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/survival_unemployment.csv", header = TRUE)},
       Darwin  = {unemployed_data <- read.csv("//Volumes/Data/CodeMagic/Data Files/survival_unemployment.csv", header = TRUE)})

# We will extract a dataset for modelling and call it as model_data
model_data <- data.frame(cbind(unemployed_data$spell, unemployed_data$event, unemployed_data$logwage, unemployed_data$ui, unemployed_data$age))
# Give appropriate namings for the columns
colnames(model_data) <- c("Time","Event","Logwage","UI","Age")

# Lets check the data load 
dim(model_data)
str(model_data)
head(model_data)

# Check for any missing values
colSums(is.na(model_data)) # Looks like there are no missing values

# Lets run some descriptive statistics to see the distribution of the data
summary(model_data)
# Time - Is a positively skewed distribution, most of the observations are on the lower range..
# Also denotes the average time the people are taking to get a job is 6.2 period

# Event - Mean is 0.321 which means 32% of the people have experienced the failure event or have 
# not survived (which is getting a job)

# *********************** Non Parametric Survival Model *********************************************************************

# This will give us a directional view of which profile has a better survival rate and it cannot be extrapolated to higher span
# predictions
# We will create a survival curve and get insights to the overall portfolio survival view

# We will do Kaplan-Meier non-parametric analysis
kmsurvival <- survfit(Surv(model_data$Time, model_data$Event) ~ 1)
# Check the summary of the analysis
summary(kmsurvival)
# Plot the analysis for better understanding
plot(kmsurvival, xlab = "Time in Periods", ylab = "Probability for Survival", 
     main = "Survival Probability vs. Time in Periods")

# Lets read the survival table
# In the first period all the subjects are applicable and 294 (n.event) has had the event happened (which is 
# getting a job, the survival rate is 0.912-91.2% people remained unemployed)
# similarly in the second period, number of people applicable is 2803 which is 3343 (total observations)
# minus 294 (who got job in the first period) and 178 people got job (event happened)
# survival rate is 0.854 - 85.4% (2803 - 178 = 2625 people survived)
# hazard rate = 1 - Survival rate = 14.6% (178 / 2803)
# As we traverse through the table, we see that the survival rate goes down as more and more people starts
# getting jobs

# The graph also shows the same thing - ex after 10 periods approximately 63% survived.

# Lets run the survival analysis by the categorical variable we have UI.. to see the effect of the variable
# on the overall survival rate
kmsurvivalUI <- survfit(Surv(model_data$Time, model_data$Event) ~ model_data$UI)
summary(kmsurvivalUI)
plot(kmsurvivalUI, xlab = "Time in Periods", ylab = "Probability for Survival", 
     main = "Survival Probability by presence of Unemployed Insurance" )
# Looking at the survival table, we see that Survival rate for people having unemployed insurance (UI = 1)
# is much higher than people having no employment insurance (UI = 0) - which is a bad thing
# This infers that people who have no unemployment insurance are getting job earlier


# *********************** Semi Parametric Survival Model *********************************************************************

# Cox(1972) introduced an approach which directly focuses on estimating the hazard function 
# directly using both time and profile variables.

# Running the cox proportional hazard model - estimating coefficients and hazard rates
coxanalysis <- coxph(Surv(model_data$Time, model_data$Event) ~ 
                       model_data$Logwage + model_data$UI + model_data$Age, method = "breslow")
summary(coxanalysis)
# The first line of the output summarizes the entire data. Total we have 3343 observations and in 
# 1073 observations event has already occured

# The second set is the coefficient table - can be explained/interpreted as below
# The coefficient of first input variable logwage is positive - which means
# As the wage of the individual increases - survival rate reduces (means the individual would have 
# lower unemployment duration) -- this is little counter-intuitive here - hazard rate increases
# The coeffient of the second input variable UI is negative - which means as an individual
# gets a unemployment insurance - survival rate increases (which means the chances of the individual 
# having higher unemployment duration) - hazard rate decreases
# Similarly for the third variable

# The third set is the hazard table which can be interpreted as 
# For the first variable logwage - unit increase in logwages results in 1.5865 - 1 = 0.5865 = 58.65%
# increase in the hazard rate
# For the second variable - individuals who have unemployed insurance is 1 - 0.3755 = 0.63 = 63% LOWER
# hazard rate as the coefficient  of UI is negative in the second table

# Individuals having higher wages are more likely to find a job because they have higher hazard rates
# Higher hazard rate = lower survival rate = more likely to have the event (death, which in this case is
# finding a job)

# We see that this result of semi parametric analysis is consistent with the results we got in 
# non parametric analysis

# *********************** Parametric Survival Model *********************************************************************

# Exponential, Weibull and log-logistic parametric model coefficients
exponential <- survreg(Surv(model_data$Time, model_data$Event) ~ 
                         model_data$Logwage + model_data$UI + model_data$Age, dist = "exponential")
summary(exponential)

weibull <- survreg(Surv(model_data$Time, model_data$Event) ~ 
                         model_data$Logwage + model_data$UI + model_data$Age, dist = "weibull")
summary(weibull)

loglogistic <- survreg(Surv(model_data$Time, model_data$Event) ~ 
                     model_data$Logwage + model_data$UI + model_data$Age, dist = "loglogistic")
summary(loglogistic)

# It is interesting to note that, the results are similar in magnitude with coxanalysis but opposite in the
# signs of coefficients .. so we need to interpret accordingly

# Remember - Positive coefficient means event happening faster (higher hazard rate and lower survival rate)
