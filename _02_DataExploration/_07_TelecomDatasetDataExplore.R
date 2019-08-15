# ************************************************************************************************************************
# Exploratory Data Analysis - Telecom Dataset
# The manager of the telecom firm notices higer customer attrition. Prevent loss of business to customers
# Also reduce costs for new customer acquisition
# To understand how the churn behaviour is dependent on various factors present in the given telecom data
# ************************************************************************************************************************

# We will perform Exploratopry Data Analysis using Base R
# Load the data

# Linux is not reading the file from network share and hence reading it from local

# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems
switch(Sys.info() [['sysname']],
       Windows = {Churn <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/Telecom_Class.txt", sep = ",", header = FALSE, stringsAsFactors = FALSE)},
       Linux =   {Churn <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/Telecom_Class.txt", sep = ",", header = FALSE, stringsAsFactors = FALSE)},
       Darwin =  {Churn <- read.csv("//Volumes/Data/CodeMagic/Data Files/Telecom_Class.txt", sep = ",", header = FALSE, stringsAsFactors = FALSE )})
# Check the load
head(Churn)
# Adding Data Columns
colnames(Churn) <- c("sbscrp_id","minuse1","minuse2","minuse3","minuse4","plan_type","prom1","prom2","prom3","prom4",
                     "svc_start_dt","svc_end_dt","zip_code","new_cell_ind","prizm_code","bdt")

# Reviewing the data
dim(Churn)
str(Churn)

# Reviewing the observations
head(Churn)
tail(Churn)

# Checking for missing values - Should be the first task in identifying data quality checks
colSums(is.na(Churn))

# Now to get a better understanding of the variable we do Uni-variable profiling
# Thumb rule is - Frequency Distribution for Categorical Variables
# Summary Statistics for numerical variables

table(Churn$plan_type) # Shows 200 for 10 plan type has the highest customers
table(Churn$new_cell_ind) # Unknown has the maximum customers

# Lets do some graphical analysis
par(mfrow = c(2,2))
hist(Churn$minuse1)
hist(Churn$minuse2)
hist(Churn$minuse3)
hist(Churn$minuse4)
dev.off()
# Run summary statistics
summary(Churn$minuse1)
summary(Churn$minuse2)
summary(Churn$minuse3)
summary(Churn$minuse4)

# Attempt to identify outliers using boxplots
par(mfrow = c(2,2))
boxplot(Churn$minuse1)
boxplot(Churn$minuse2)
boxplot(Churn$minuse3)
boxplot(Churn$minuse4)
dev.off()

# Creating Churn variable - this would reflect, whether the customer is already exited from the service or not
Churn$ind_churn <- ifelse(Churn$svc_end_dt == "", 0, 1)
# see the frequency distribution as to how many customers have exited (ind_churn = 1 as exit customers)
table(Churn$ind_churn)
# Computing the Churn % in the given Sample
sum(Churn$ind_churn == 1)/ nrow(Churn)

# Dealing with dates
Churn$bdt <- as.Date(Churn$bdt)
Churn$svc_start_dt <- as.Date(Churn$svc_start_dt)
Churn$svc_end_dt <- as.Date(Churn$svc_end_dt)

str(Churn)

# Missing Value Treatment
colSums(is.na(Churn))
Churn$minuse1[is.na(Churn$minuse1)] <- mean(Churn$minuse1, na.rm = TRUE)
Churn$minuse2[is.na(Churn$minuse2)] <- mean(Churn$minuse2, na.rm = TRUE)
Churn$minuse3[is.na(Churn$minuse3)] <- mean(Churn$minuse3, na.rm = TRUE)
Churn$minuse4[is.na(Churn$minuse4)] <- mean(Churn$minuse4, na.rm = TRUE)

# For missing value treatments of Categorical variables, we may replace the missing values with the ones which are at higher numbers in the dataset
table(Churn$prom1)
table(Churn$prom2)
table(Churn$prom3)
table(Churn$prom4)
Churn$prom1[is.na(Churn$prom1)] <- 0
Churn$prom2[is.na(Churn$prom2)] <- 0
Churn$prom3[is.na(Churn$prom3)] <- 0
Churn$prom4[is.na(Churn$prom4)] <- 0

# Bi-variate profiling or Exploratory Analysis - Cross Tabulations
tapply(Churn$ind_churn, Churn$plan_type, sum)
table(Churn$plan_type)
