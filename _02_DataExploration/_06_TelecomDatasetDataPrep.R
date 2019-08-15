# ************************************************************************************************************************
# Exploratory Data Analysis - Telecom Dataset
# This file will illustrate lots of data preparation acitivities using the telecom dataset
# ************************************************************************************************************************

# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems
switch(Sys.info() [['sysname']],
       Windows = {telecom.ds <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/telecom-class10.csv", header = TRUE)},
       Linux =   {telecom.ds <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/telecom-class10.csv", header = TRUE)},
       Darwin =  {telecom.ds <- read.csv("//Volumes/Data/CodeMagic/Data Files/telecom-class10.csv", header = TRUE )})
# check the data load
head(telecom.ds)
str(telecom.ds)
# we see promotions are treated as integers... they should be factors with two levels.. either 0 or 1
telecom.ds$prom1 <- as.factor(telecom.ds$prom1)
telecom.ds$prom2 <- as.factor(telecom.ds$prom2)
telecom.ds$prom3 <- as.factor(telecom.ds$prom3)
telecom.ds$prom4 <- as.factor(telecom.ds$prom4)

# ************************************************************************************************************************
# 1. How many variables have missing values present in them?

# we can find the frequency distribution of missing values across all variables
# Total missing value in the dataset
sum(is.na(telecom.ds))
# Frequency distribution of missing values by column variables
telecom.ds.na.summary <- colSums(is.na(telecom.ds))
telecom.ds.na.summary

# if the dataset is fairly large, programmatically extract just the column variable indexes which have NA within them
# find the index of the non zero values from the NA frequency distribution
telecom.na.index <- which(telecom.ds.na.summary != 0, arr.ind = TRUE)
telecom.na.index # this is the index of all column variables which has NA in the dataset

# total count of column variables which have NA present in them
length(telecom.na.index)

# ************************************************************************************************************************
# 2. Say if we delete all the observations that contains missing values, then what would be the total loss of observarions?

telecom.ds.noNA <- na.omit(telecom.ds)
# total loss of observations from original dataset
nrow(telecom.ds) - nrow(telecom.ds.noNA)

# ************************************************************************************************************************

# 3. If we decide to update the missing values with mean, what is the impact on standard deviation of each variable that
# contains missing values?
# Remember mean imputation is more used for numerical variables. In categorical variables, we can try at individual level

# lets find out the SD of each variable in the dataset (with NA's present)
# we will find out sd of only those variable which has NA present and is numeric.. For others it will not make any difference.

# lets create a mini dataset with only those columns that has NA values and numeric
telecom.mini.ds <- data.frame(cbind(telecom.ds$minuse1, telecom.ds$minuse2, telecom.ds$minuse3, telecom.ds$minuse3))
colnames(telecom.mini.ds) <- c("MinutesUse1", "MinutesUse2", "MinutesUse3", "MinutesUse4")
# lets do a quick check
head(telecom.mini.ds)
# standard deviation of each variable in the dataset
telecom.mini.ds.sd.summary <- data.frame(apply(telecom.mini.ds, 2, sd, na.rm = TRUE))
colnames(telecom.mini.ds.sd.summary) <- "Standard Deviation"
telecom.mini.ds.sd.summary

# we will now replace each of the NA value in the column variable with mean -
replaceNAMean <- function(column.variable){
  column.variable[is.na(column.variable)] <- mean(column.variable, na.rm = TRUE)
  column.variable
}

# we will apply the above for all columns in the mini data set and the result goes into the new data frame
telecom.mini.ds.NoNA <- data.frame(apply(telecom.mini.ds, 2, replaceNAMean))
head(telecom.mini.ds.NoNA)
telecom.mini.ds.NoNA.Sd.Summary <- data.frame(apply(telecom.mini.ds.NoNA, 2, sd))
colnames(telecom.mini.ds.NoNA.Sd.Summary) <- "SD After NA with Mean"
head(telecom.mini.ds.NoNA.Sd.Summary)

# difference in SD
sd.difference <- (telecom.mini.ds.sd.summary - telecom.mini.ds.NoNA.Sd.Summary)
colnames(sd.difference) <- "SD Difference"

# comprehensive view of difference in SD if NA is substituted with mean
cbind(telecom.mini.ds.sd.summary, telecom.mini.ds.NoNA.Sd.Summary, sd.difference)

# Promo 3 and Promo 4 are factor variables with Values 0 or 1 .. We can replace them with either 0 or 1
telecom.ds$prom3[is.na(telecom.ds$prom3)] <- 0
telecom.ds$prom4[is.na(telecom.ds$prom4)] <- 0
sum(is.na(telecom.ds$prom4))

# Final Dataset after Na removal
telecom.ds.final <- telecom.ds
telecom.ds.final$minuse1 <- telecom.mini.ds.NoNA$MinutesUse1
telecom.ds.final$minuse2 <- telecom.mini.ds.NoNA$MinutesUse2
telecom.ds.final$minuse3 <- telecom.mini.ds.NoNA$MinutesUse3
telecom.ds.final$minuse4 <- telecom.mini.ds.NoNA$MinutesUse4
telecom.ds.final$prom3 <- telecom.ds$prom3
telecom.ds.final$prom4 <- telecom.ds$prom4
head(telecom.ds.final)
colSums(is.na(telecom.ds.final)) # Check there should be no NA values anywhere


# ************************************************************************************************************************

# 4. How many outliers can you identify for each variable? Remember outlier identification is more subjective process and you should consider the business problem being solved.

# we will use graphical techniqies as well as summary statistics, data investigation to determine outliers. The outliers identified here could be potential outliers, although the final
# confirmation of the outlier has to be ascertained through domain knowledge, domain experts or customers (in this case the telecom company has given the dataset)

# variable - sbscrp_id: Unique identifier for each subscriber

# a quick way to check whether the subscription id are of the same length
min(telecom.ds$sbscrp_id)
max(telecom.ds$sbscrp_id)
# we will check for duplicates
subscID.dupIndex <- which(duplicated(telecom.ds$sbscrp_id) == TRUE)
subscID.dupIndex # row number has subscrp_id which has been duplicated. This is supposed to be unique
# duplication subscription ID value
telecom.ds$sbscrp_id[subscID.dupIndex]
# we will then extract all thr rows which has duplicated subscID - just to ensure that the same number is not re-used for multiple customers
subset.data.frame(telecom.ds, telecom.ds$sbscrp_id == (telecom.ds$sbscrp_id[subscID.dupIndex]))
# from the above data set, we see there are two active customers with the same svc start date with same subscription ids. This is an erroneous data and needs to be investigated.

# Potential Outlier - This value is duplicated
subset.data.frame(telecom.ds, telecom.ds$sbscrp_id == (telecom.ds$sbscrp_id[subscID.dupIndex]))

# Variable - minuse1 : Minutes used in month 1
# ---------------------------------------------------------------

# A quick way to do some sanity checks
summary(telecom.ds$minuse1)

# some graphical plots
boxplot(telecom.ds$minuse1) # shows there is one value which is more than 1000 minutes
plot(telecom.ds$minuse1, type = "b")
hist(telecom.ds$minuse1)
# lets extract the value and investigate the whole record
subset.data.frame(telecom.ds, telecom.ds$minuse1 > 1000)
# this shows significantly higher usage in month1 as compared to month2 and month3
# lets find out where there are other customers, who have used more than 1000 minutes in month1
subset.default(telecom.ds$minuse1, telecom.ds$minuse1 > 1000)
# looks like this is the only customer in the entire data set having usage more than 1000 minutes in month1

# Potential Outlier - This value is very high as compared to other usages in month 1
subset.data.frame(telecom.ds, telecom.ds$minuse1 > 1000)
subset.default(telecom.ds$minuse1, telecom.ds$minuse1 > 1000)

# Variable - minuse2 : Minutes used in month 2
# ---------------------------------------------------------------

# A quick way to do some sanity checks
summary(telecom.ds$minuse2)
# some graphical plots
boxplot(telecom.ds$minuse2) # shows there is one value which is more than 1000 minutes
plot(telecom.ds$minuse2, type = "b")
hist(telecom.ds$minuse2)
# lets extract two records and investigate >= 1500 minutes and < 0 minutes,,,
subset.data.frame(telecom.ds, telecom.ds$minuse2 >= 1500)
# extract the record for the negative usage
subset.data.frame(telecom.ds, telecom.ds$minuse2 < 0)
# Looks like this customer has minimal usage in month 1, negative in month 2 with a high in month 3. It is possible that negative usage may be because
# of some credit given to boost usage. Need to find out from customer/domain experts.

# Potential Outlier - The two value below needs clarification
subset.data.frame(telecom.ds, telecom.ds$minuse2 >= 1500)
subset.data.frame(telecom.ds, telecom.ds$minuse2 < 0)

# Variable - minuse3 : Minutes used in month 3
# ---------------------------------------------------------------

# A quick way to do some sanity checks
summary(telecom.ds$minuse3)
# some graphical plots
boxplot(telecom.ds$minuse3) # shows there is one value which is more than 1000 minutes
plot(telecom.ds$minuse3, type = "b") # we see two records with extreme high values than rest of the records
hist(telecom.ds$minuse3)
# lets extract two records and investigate >= 1500 minutes
subset.data.frame(telecom.ds, telecom.ds$minuse3 >= 1500)

# Potential Outlier - They may be just high usage customers, but can be confirmed back from business
subset.data.frame(telecom.ds, telecom.ds$minuse3 >= 1500)

# Variable - minuse4 : Minutes used in month 4
# ---------------------------------------------------------------

# A quick way to do some sanity checks
summary(telecom.ds$minuse4)
# some graphical plots
boxplot(telecom.ds$minuse4) # shows there is one value which is more than 1000 minutes
plot(telecom.ds$minuse4, type = "b") # we see two records with extreme high values than rest of the records
hist(telecom.ds$minuse4)
# one value clearly stands out from graph and summary statistics
subset.data.frame(telecom.ds, telecom.ds$minuse4 == max(telecom.ds$minuse4, na.rm = TRUE))
# even the usage in month 4 exceeds the total minutes in a month

# Potential Outlier - minuse 4
subset.data.frame(telecom.ds, telecom.ds$minuse4 == max(telecom.ds$minuse4, na.rm = TRUE))

# Variable - plan_type
# ---------------------------------------------------------------
# some sanity checks
levels(telecom.ds$plan_type)
subset.data.frame(telecom.ds, telecom.ds$plan_type == "")
# we see there are three customers, who have no plan type. All of them jave joined the service on August and have participated in a promo during
# month 1. It may so happen, that they are in some generic plan and that is ot of a specific type. Hence the plan type is missing.

# Potential outlier - Need to clarify from business on plan type being empty
subset.data.frame(telecom.ds, telecom.ds$plan_type == "")

# Variable - prom1, prom2, prom3, prom4
# ---------------------------------------------------------------
# some sanity checks
levels(telecom.ds$prom1)
levels(telecom.ds$prom2)
levels(telecom.ds$prom3)
levels(telecom.ds$prom4)

# Potential Outlier - None, there are however missing values, which would need missing value treatment for prom3 and prom4

# Variable - svc_start_dt
# ---------------------------------------------------------------
# some sanity checks
str(telecom.ds$svc_start_dt)
sum(is.na(telecom.ds$svc_start_dt)) # there are no missing values
serviceStart.date <- as.Date(as.character(telecom.ds$svc_start_dt), format = "%d-%B-%y")
summary(serviceStart.date)
# some graphical checks
plot(serviceStart.date)

# Potential Outlier - none, - reveals nothing unusual

# Variable - svc_end_dt
# ---------------------------------------------------------------
# some sanity checks
str(telecom.ds$svc_end_dt)
sum(is.na(telecom.ds$svc_end_dt)) # there are no missing values
serviceEnd.date <- as.Date(as.character(telecom.ds$svc_end_dt), format = "%d-%B-%y")
summary(serviceEnd.date)
# some graphical checks
plot(serviceEnd.date)

# Potential Outlier - none, - reveals nothing unusual

# Variable - birth_dt
# ---------------------------------------------------------------
str(telecom.ds$birth_dt)
levels(telecom.ds$birth_dt)
# check for customers having no date of birth
nrow(subset.data.frame(telecom.ds, telecom.ds$birth_dt == ""))

# Potential Outlier - 15 customers having no data of birth in the record. This would not allow us to do any analysis involving age of the customer.
subset.data.frame(telecom.ds, telecom.ds$birth_dt == "")

# Variable - zip_code
# ---------------------------------------------------------------
str(telecom.ds$zip_code)
range(telecom.ds$zip_code) # this shows zip code ranges from three digits to five digits
# zip codes should ideally be five digits... extract all records having zip codes less than 5 digits
subset.data.frame(telecom.ds, nchar(telecom.ds$zip_code) < 5)
# number of records having zip codes less than 5
nrow(subset.data.frame(telecom.ds, nchar(telecom.ds$zip_code) < 5))
# The above 170 records needs clarification has to whether the zip codes has trailing values 0 which got truncated.

# Potential Outliers - None

# Variable - new_cell_ind
# ---------------------------------------------------------------
str(telecom.ds$new_cell_ind)
levels(telecom.ds$new_cell_ind)

# Potential Outliers - None.. Just to understand what is Unknown("U") - If we need to do some analysos whether new Cell Customers and promotional
# offers are related
subset.data.frame(telecom.ds, telecom.ds$new_cell_ind == "U")


# ************************************************************************************************************************
# 5. If you replaced the top two extreme values by the mean for any one of the variables, how would the standard deviation change?

# Again standard deviation changes are compared for numeric columns only. lets say we do it for Minuse1 column
# we have extracted earlier a mini dataset having only the numeric columns - telecom.mini.ds
head(telecom.mini.ds)
telecom.sd.MinUse1 <- data.frame(sd(telecom.mini.ds$MinutesUse1, na.rm = TRUE))
colnames(telecom.sd.MinUse1) <- "MinutesUse1 SD"

# we will now replace the top two values with the mean
quantile(telecom.mini.ds, na.rm = TRUE)
telecom.mini.ds.copy <- telecom.mini.ds # we change the top 2 value with mean in this dataset

replaceTopExtremeValuesWithMean <- function(column.variable, numberTopValues){
  for(index in 1:numberTopValues){
    maxtop1 <- max(column.variable, na.rm = TRUE)
    top1Index <- which(column.variable == maxtop1)
    column.variable[top1Index] <- mean(column.variable, na.rm = TRUE)
  }
  return(column.variable)
}
# to change the top 2 extreme value we call the above function...(second argument, tells the number of top values we want to replace)
telecom.mini.ds.copy$MinutesUse1 <- replaceTopExtremeValuesWithMean(telecom.mini.ds.copy$MinutesUse1, 2)

telecom.mini.ds.copy.sd.MinutesUse1 <- data.frame(sd(telecom.mini.ds.copy$MinutesUse1, na.rm = TRUE))
colnames(telecom.mini.ds.copy.sd.MinutesUse1) <- "MinutesUse1 SD Change After Mean Replace"

# To see the change in SD
cbind(telecom.sd.MinUse1, telecom.mini.ds.copy.sd.MinutesUse1)