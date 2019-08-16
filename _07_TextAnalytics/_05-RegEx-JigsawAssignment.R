# *************************************************************************************************************
# Text Mining - Dealing with Regular Expressions
#
# Assignment for Class 03
# 
# **************************************************************************************************************
rm(list = ls()) # Clear the environment and run time variables

# use of external libraries
library(ggplot2)

# Question - 1
# ---------------------------------------------------------------

# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems

switch(Sys.info() [['sysname']],
       Windows = {textfile <- read.csv("//192.168.1.33/Data/CodeMagic/Data Files/TextMining/Assignments/Topic3-RegularExpressions /Dataset/textfile.csv", header = TRUE, stringsAsFactors = FALSE)},
       Linux   = {textfile <- read.csv("//192.168.1.33/Data/CodeMagic/Data Files/TextMining/Assignments/Topic3-RegularExpressions /Dataset/textfile.csv", header = TRUE, stringsAsFactors = FALSE)},
       Darwin  = {textfile <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/Assignments/Topic3-RegularExpressions /Dataset/textfile.csv", header = TRUE, stringsAsFactors = FALSE)})

# Check the data load
dim(textfile)
str(textfile)
head(textfile)

# a) Clean the text column - Remove Punctuation and Numbers
textfile$text <- gsub("[[:punct:]]","", textfile$text) # This will remove all punctuations
textfile$text <- gsub("[[:digit:]]","", textfile$text) # This will remove all numbers
# Now convert the text to lower
textfile$text <- tolower(textfile$text)

# b) Irrespective of the case of the “text”, replace the words “government” to "govt" and  
# “politician” to "modi"
textfile$text <- gsub("(government)","govt", textfile$text)
textfile$text <- gsub("(politician)","modi", textfile$text)

# c) Now find the frequency distribution of the words in the text column 
textfreq <- data.frame(table(textfile$text))
colnames(textfreq) <- c("TextItems", "Count")
# visualizing the frequency distribution
ggplot(textfreq, aes(x = TextItems, y = Count)) + geom_bar(stat = "Identity", col = "black") + xlab("Text Items") +
  geom_text(aes(label = Count), vjust = 1.5, colour = "white")

# Question - 2
# ---------------------------------------------------------------

# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems

switch(Sys.info() [['sysname']],
       Windows = {book_reviews <- read.csv("//192.168.1.33/Data/CodeMagic/Data Files/TextMining/Assignments/Topic3-RegularExpressions /Dataset/book_reviews.csv", header = TRUE, stringsAsFactors = FALSE)},
       Linux   = {book_reviews <- read.csv("//192.168.1.33/Data/CodeMagic/Data Files/TextMining/Assignments/Topic3-RegularExpressions /Dataset/book_reviews.csv", header = TRUE, stringsAsFactors = FALSE)},
       Darwin  = {book_reviews <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/Assignments/Topic3-RegularExpressions /Dataset/book_reviews.csv", header = TRUE, stringsAsFactors = FALSE)})

# Check the data load
str(book_reviews)
head(book_reviews)

# a) Clean the “Review” column  - Remove punctuations and numbers ; convert to lower case
book_reviews$Review <- gsub("[[:punct:]]", "", book_reviews$Review)
book_reviews$Review <- gsub("[[:digit:]]","", book_reviews$Review)
book_reviews$Review <- tolower(book_reviews$Review)
# Check the review data
book_reviews$Review

# b) Study how many people are having a good opinion of the book

# We will filter out a subset of only good comments
good_reviews <- book_reviews[grep("*good*", book_reviews$Review),]
# Check all the good comments
good_reviews$Review
# Number of good comments
length(good_reviews$Review) # There are seven happy customers

# c) Study the frequency distribution of satisfied customers by gender and age
freqtable <- data.frame(table(good_reviews$Age, good_reviews$Gender))
colnames(freqtable) <- c("Age", "Gender", "Count")
head(freqtable)
ggplot(freqtable, aes(x = Age, y = Count, fill = Gender)) + 
  geom_bar(stat = "identity", position = "dodge", col = "black") + 
  geom_text(aes(label = Count), vjust = 1.5, colour = "white", position = position_dodge(0.9))

# The above shows that in the age of 20 there are no males and we have 2 male satisfied customers
# In age 25 there is 1 male and 1 female satisfied customers
# There is one satisfied female customer in age 32
