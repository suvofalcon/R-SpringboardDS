# *************************************************************************************************************
# Text Mining - Hierarchial Clustering on the NYTIMES dataset
#
# A sample dataset containing labelled headlines. Analyse the subject of the articles. Subjects are mapped
# under Topic Code
#
# This code has been developed using R version 3.2.3 (hence few new function calls has been made)
# 
# The objective is to build a hierarchial cluster , plot a dendogram and interpret the results
# **************************************************************************************************************

rm(list = ls()) # We clear all runtime variables in the Environment

# Use of external libraries
library(tm)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)
library(ggdendro)

# Load the tweets data
switch(Sys.info() [['sysname']],
       Windows = {nytimes <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/TextMining/Assignments/Topic7-Clustering/Dataset/nytimes.csv", 
                                         header = TRUE, stringsAsFactors = FALSE)},
       Linux   = {nytimes <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/TextMining/Assignments/Topic7-Clustering/Dataset/nytimes.csv", 
                                         header = TRUE, stringsAsFactors = FALSE)},
       Darwin  = {nytimes <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/Assignments/Topic7-Clustering/Dataset/nytimes.csv", 
                                         header = TRUE, stringsAsFactors = FALSE)})

# Lets check the data load
dim(nytimes)
str(nytimes)
# check the first few rows
head(nytimes, 10)

# We will work with the Subject column

# We convert to a data frame and only the second column
nytimesdata <- data.frame(nytimes$Subject)
names(nytimesdata) <- "Subject"
# Now lets see the structure
str(nytimesdata) # We see that the tweets column has been converted to a factor

# ********** Data Pre-Processing **************************************************** #

# Building the text corpus (a large structured set of texts or documents)
# To build a corpus , we use the function corpus and specify the source
# source in this case is a vector - it can be data frame, xml, URI and many more
nytimesdata.corpus <- Corpus(VectorSource(nytimesdata$Subject))
# check the summary
summary(nytimesdata.corpus) # This Corpus is a collection of 1000 text documents
# inspecting the first five elements of the corpus
inspect(nytimesdata.corpus[1:5])
# showing the content of the first element
nytimesdata.corpus[[1]]$content # OR 
content(nytimesdata.corpus[[1]])

# Now we do some data transformations - this is basically to clean the data
# Since R is casesenstive, so we either convert the data to upper case or lower case...
nytimesdata.corpus <- tm_map(nytimesdata.corpus, tolower)
# We would remove the extra white space
nytimesdata.corpus <- tm_map(nytimesdata.corpus, stripWhitespace)
# Remove the punctuations
nytimesdata.corpus <- tm_map(nytimesdata.corpus, removePunctuation)
# Remove the numbers
nytimesdata.corpus <- tm_map(nytimesdata.corpus, removeNumbers)

# Stopwords in English language are words, which do not add any additional meaning - for ex all conjunctions
# to check the stopwords in english language
stopwords('english')

# Now we will remove some stopwords from data. We can add more words apart from the standard list as well
stopwords_list <- c(stopwords(kind = "en"),'http*')
nytimesdata.corpus <- tm_map(nytimesdata.corpus, removeWords, stopwords_list)

# Some of the tm_map functions do not return TextDocument and instead returns characters and the DocumentTermMatrix isnt sure how to handle 
# a corpus of characters and hence without this conversion, the term document matrix will fail
nytimesdata.corpus <- tm_map(nytimesdata.corpus, PlainTextDocument)

# Now lets again see the first two contents to verify the transformations

content(nytimesdata.corpus[[1]])
content(nytimesdata.corpus[[2]])

# ********** Text Mining **************************************************** #

# Building a term document matrix
nytimesdata.tdm <- TermDocumentMatrix(nytimesdata.corpus)
nytimesdata.tdm # This shows number of words in the total number of documents (1000 in this case)
dim(nytimesdata.tdm) # The matrix has 2492 rows and 1000 columns

# Remove sparse terms
# A sparse parameter of 0.9 we select from those terms which are less than 90% empty
nytimesdata.tdm <- removeSparseTerms(nytimesdata.tdm, sparse = 0.97)
nytimesdata.matrix <- as.matrix(nytimesdata.tdm)


# Finding the distance metric between the terms in the document
distmatrix <- dist(scale(nytimesdata.matrix), method = "euclidean")
# Applying hierarchial clustering algorithm
nytimesdata.h <- hclust(distmatrix, method = "ward.D2")

# The cluster height used in the dendogram
nytimesdata.h$height
# labels for each of the objects being clustered
nytimesdata.h$labels

# to get a flat clustering
k <- cutree(nytimesdata.h, k = 3) # we want 5 clusters

# plot the dendogram  
ggdendrogram(nytimesdata.h, cex = 0.9) # a basic plot
ggdendrogram(nytimesdata.h, rotate = FALSE, size = 4, theme_dendro = FALSE, color = "tomato")
