# ************************************************************************************************************************
# Exploratory Data Analysis - 2006 birth data
# Processing the information and getting to know the data on 2006 data set

# This makes extensive usage of lattice package
# ************************************************************************************************************************
options(scipen = 999) # preventing no automatic roundoffs

library(lattice)
library(nutshell)

# use the inbuilt dataset in lattice package
data(births2006.smpl)
# check rows and columns
dim(births2006.smpl)
# check the data load
head(births2006.smpl)
str(births2006.smpl)
?births2006.smpl

# to check the frequencies of birth according to the day of the week
births.dow <- table(births2006.smpl$DOB_WK)
births.dow # we see more number of births during the weekdays than on the weekends(1 - 7)
# If we need to visualize this
barchart(births.dow, ylab = "Day of the week", col = "black")

# understanding frequency distribution by weeks of days based on method of birth
dob.dm.tbl <- table(WK = births2006.smpl$DOB_WK, MM = births2006.smpl$DMETH_REC)
dob.dm.tbl
# dropping the unknown column
dob.dm.tbl <- dob.dm.tbl[,-2]
# Visualizing the data
barchart(dob.dm.tbl, horizontal = FALSE, groups = FALSE, xlab = "Day of the Week", col = "black")
# to visualize with a trellis graph (also a stacked barchart)
barchart(dob.dm.tbl, ylab = "Day of the Week")
# we see the number of Vaginal births are way mmore than c-section in every days of the week

# understanding the effect on Plural births on birth weight. With common sense, birth weight would
# decrease on plural births
levels(births2006.smpl$DPLURAL)
histogram(~births2006.smpl$DBWT|births2006.smpl$DPLURAL, layout = c(1,nlevels(births2006.smpl$DPLURAL)), col = "black", xlab = "Birth Weight",
          main = "Birth Weights by Plural Births")
# understanding effect on Delivery methods on birth weight
levels(births2006.smpl$DMETH_REC)
histogram(~births2006.smpl$DBWT|births2006.smpl$DMETH_REC, layout = c(1,nlevels(births2006.smpl$DMETH_REC)), col = "black", xlab = "Birth Weight",
          main = "Birth Weights by Method of delivery")
# Conclusion - Birth weight decreases by multiple births and remains largely unaffected by 
# method of delivery

# Number of observations by number of births
table(births2006.smpl$DPLURAL)
# The number of observations on Quadruplet and Quintuplet is very low as compared to total births
# This is also shown by density plot and dot plot
densityplot(~births2006.smpl$DBWT, groups = births2006.smpl$DPLURAL, plot.points = FALSE, xlab = "Birth Weight")
dotplot(~births2006.smpl$DBWT|births2006.smpl$DPLURAL, layout = c(1,5), plot.points = FALSE, col = "black")

# Birthweight on day of week - seems to be no apparent relationship
xyplot(births2006.smpl$DBWT~births2006.smpl$DOB_WK, col = "black") # There is no association between birthweight and day of the week
xyplot(births2006.smpl$DBWT~births2006.smpl$DOB_WK|births2006.smpl$DPLURAL, layout = c(1,nlevels(births2006.smpl$DPLURAL)), col = "black", xlab = "Day of Week",
       ylab = "Birth Weight")

# Analyse possible effect on birth weight against weight gain during pregnancy
xyplot(births2006.smpl$DBWT~births2006.smpl$WTGAIN, col = "black", xlab = "Weight Gain During Pregnancy", 
       ylab = "Birth Weight", main = "Birth Weight vs Weight Gain during pregnancy")
# Birth weight to Weight gain during pregnancy by number of births
xyplot(births2006.smpl$DBWT~births2006.smpl$WTGAIN|births2006.smpl$DPLURAL, layout = c(1,5), col = "black")
smoothScatter(births2006.smpl$WTGAIN, births2006.smpl$DBWT)
# There is a little association between birth weight and weight gain during pregnancy

# Lets understand the birth weight against the APGAR score
boxplot(births2006.smpl$DBWT~births2006.smpl$APGAR5, ylab = "Birth Weight", xlab = "APGAR Score")
# The above shows a strong relationship between APGAR score and birthweight
# Analysing the above by SEX as well
bwplot(births2006.smpl$DBWT~as.factor(births2006.smpl$APGAR5)|as.factor(births2006.smpl$SEX), xlab = "APGAR Score")

# Lets see the average birth weight as a function of multiple births
tapply(births2006.smpl$DBWT, factor(births2006.smpl$DPLURAL), mean, na.rm = TRUE)
barplot(tapply(births2006.smpl$DBWT, factor(births2006.smpl$DPLURAL), mean, na.rm = TRUE))
# segregating the above further with SEX
tapply(births2006.smpl$DBWT, list(births2006.smpl$DPLURAL, births2006.smpl$SEX), mean, na.rm = TRUE)
barplot(tapply(births2006.smpl$DBWT, list(births2006.smpl$DPLURAL, births2006.smpl$SEX), mean, na.rm = TRUE), 
        beside = TRUE, ylab = "Average Birth Weight")

# But what questions would we want to have answered with these data? One may wish to predict the 
# birth weight from characteristics such as the estimated gestation period and the weight gain of the 
# mother; for that, one could use regression and regression trees. Or, one may want to identify 
# births that lead to very low APGAR scores, for which purpose, one could use classification methods.