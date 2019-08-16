# *************************************************************************************************************
# Text Mining - Dealing with Regular Expressions
#
# The code deomonstrates the regular expressions in R
# in amazon reviews dataset on sony watches
# 
# **************************************************************************************************************
rm(list = ls()) # clear the environment area

# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems

switch(Sys.info() [['sysname']],
       Windows = {reviews <- read.csv("//192.168.1.33/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/reviews.csv", header = TRUE, stringsAsFactors = FALSE)},
       Linux   = {reviews <- read.csv("//192.168.1.33/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/reviews.csv", header = TRUE, stringsAsFactors = FALSE)},
       Darwin  = {reviews <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/reviews.csv", header = TRUE, stringsAsFactors = FALSE)})

# Check the data load
dim(reviews)
str(reviews)
head(reviews)

# We will just extract the review_title
reviews <- data.frame(review_title = reviews$review_title)
names(reviews)
head(reviews)
dim(reviews)

# Checking which expressions have "Star" - trying to understand the rating

comments <- reviews[grep("*Star", reviews$review_title),"review_title"]
# to check how many strings we have which talk about "Star"
length(comments)

# If i want to replace "Star" with "rating"
sub("(Star)", "rating", comments, perl = TRUE)

# Position of "Star"
regexpr("Star", comments)
