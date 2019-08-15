# ************************************************************************************************************************
# Exploratory Data Analysis - Alumni Contribution Data
# Processing the information and getting to know the data on Contribution.csv data set
# The dataset is about the contribution received by liberal arts college in Midwest from its respective
# alumni for the years 2000-2004

# This makes extensive usage of lattice package
# ************************************************************************************************************************
options(scipen = 999) # preventing no automatic roundoffs

library(lattice)
library(ellipse)
# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems
switch(Sys.info() [['sysname']],
       Windows = {donations <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/contribution.csv", header = TRUE)},
       Linux =   {donations <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/LedolterDatasets/DataText/contribution.csv", header = TRUE)},
       Darwin =  {donations <- read.csv("//Volumes/Data/CodeMagic/Data Files/LedolterDatasets/DataText/contribution.csv", header = TRUE)})

# Check the data load
dim(donations)
head(donations)
str(donations)

# Frequency distributions of total records by year
table(donations$Class.Year) # clear trend that number of alumni donated increased over years
# to visualize the data
barchart(table(donations$Class.Year), horizontal = FALSE, xlab = "Class Year", col = "black")

# Lets add a derived variable as total contribution
donations$TotalContrib <- donations$FY00Giving + donations$FY01Giving + donations$FY02Giving +
  donations$FY03Giving + donations$FY04Giving
# Perform summary statistics on total contribution
summary(donations$TotalContrib)
sd(donations$TotalContrib)
# lets take a detailed look on quantile distribution
quantile(donations$TotalContrib, probs = seq(0,1,0.05))
# This is a highly skewed distribution. More than 30% of the alumni gave nothing. 90% of the alumni
# gave $1050 or less and only 5% gave more than $2000 and largest contribution is $172,000 approx.

# Frequency distribution on the total donations
hist(donations$TotalContrib) 
# This frequency distribution is not useful because of large number of alumni giving very less donations
# We will omit all donations which are zero and more than $1000, which will give us detailed
# view of contributions between $1 to $1000 range
hist(subset(donations,donations$TotalContrib != 0 & donations$TotalContrib <= 1000)$TotalContrib)
# Viewing the boxplots
boxplot(donations$TotalContrib, horizontal = TRUE, xlab = "Total Contributions")
# seeing the boxplot after removing the outliers
boxplot(donations$TotalContrib, outline = FALSE, horizontal = TRUE, xlab = "Total Contributions")

# we now list the alumuni who gave $30,000 and above during the period and also list their major and
# next degree
highdonors <- subset(donations, donations$TotalContrib >= 30000)
highdonors[order(highdonors$TotalContrib, decreasing = TRUE), ]
# Total donations with respect to Gender
tapply(donations$TotalContrib, list(donations$Gender), sum, na.rm = TRUE)
# Average contributions with respect to year
barchart(tapply(donations$TotalContrib, list(donations$Class.Year), mean, na.rm = TRUE), col = "black")
# Average contributions with respect to Marital status
barchart(tapply(donations$TotalContrib, list(donations$Marital.Status), mean, na.rm = TRUE), col = "black")
# Average contributions with respect to Major degree
barchart(tapply(donations$TotalContrib, list(donations$Major), mean, na.rm = TRUE), horiz = TRUE, col = "black")
# Average contributions with respect to Next degree
barchart(tapply(donations$TotalContrib, list(donations$Next.Degree), mean, na.rm = TRUE), horiz = TRUE, col = "black")

# Average donations received by class.year in the receiving year
barchart(tapply(donations$FY00Giving, list(donations$Class.Year), mean, na.rm = TRUE), col = "black")
barchart(tapply(donations$FY01Giving, list(donations$Class.Year), mean, na.rm = TRUE), col = "black")
barchart(tapply(donations$FY02Giving, list(donations$Class.Year), mean, na.rm = TRUE), col = "black")
barchart(tapply(donations$FY03Giving, list(donations$Class.Year), mean, na.rm = TRUE), col = "black")
barchart(tapply(donations$FY04Giving, list(donations$Class.Year), mean, na.rm = TRUE), col = "black")

# Creating an indicator variable for alumni who have contributed and who didnt.. 1 for contribution and 0 for no contribution
donations$ContribIndicator <- ifelse(donations$TotalContrib > 0, 1, 0)
# Number of almuni split with contribute and no contribute
table(donations$ContribIndicator)

# donors vs non-donors by year
table(donations$ContribIndicator, donations$Class.Year)
# visualize the same
barplot(table(donations$ContribIndicator, donations$Class.Year), beside = TRUE)
# use a mosaic plot
mosaicplot(donations$Class.Year ~ donations$ContribIndicator, 
           main = "Contributors vs Non-Contributors by Class.Year", xlab = "Class.Year", ylab = "Contribution Indicator")
# shows class of 1957 has the highest proportion of contributors
tapply(donations$ContribIndicator, list(donations$Class.Year), mean, na.rm = TRUE)

# We explore pairwise correlations
# If we know the amount an alumnus gives in one year, does this give us information about how much that person
# will give in another year
corrData <- data.frame(donations$FY04Giving, donations$FY03Giving, donations$FY02Giving, donations$FY01Giving,
                       donations$FY00Giving)
cor(corrData)
plot(corrData)
plotcorr(cor(corrData))

# We see the proportion of alumni making a contribution is same for Men and Women
mosaicplot(donations$Gender ~ donations$ContribIndicator)
# Married alumni is more likely to contribute and it consists of largest population
mosaicplot(donations$Marital.Status ~ donations$ContribIndicator)

# Alumni who have attended an informational meeting are more likely to contribute
mosaicplot(donations$AttendenceEvent ~ donations$ContribIndicator)
# More than half of all alumni have attended such meeting.

# Distributions of alumni who have attended the event
table(donations$AttendenceEvent)
# distribution of event attendance and marital status
attendance_maritalstatus <- table(donations$Marital.Status, donations$ContribIndicator, donations$AttendenceEvent)
mosaicplot(attendance_maritalstatus[,,1])
mosaicplot(attendance_maritalstatus[,,2])
# The likelihood of giving increases with attendance, but the rel- ative proportions of giving across the 
# marital status groups are fairly similar. This tells us that there is a main effect of attendance, 
# but that there is not much of an interaction effect.

# It may be of interest to predict the likelihood of 2004 giving on the basis of the previous giving 
# history (2000–2003), donor characteristics, and whether a graduate had attended an informational meeting. 
# Logistic regression models or classification trees will be the prime models. 