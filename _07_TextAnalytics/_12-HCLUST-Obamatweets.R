# *************************************************************************************************************
# Text Mining - Cluster Analysis using Hierarchical Clustering.
#
# Dataset Used - Tweets database about obama
#
# We will build a hierarchial clustering using the tweets pulled from twitter about obama
# **************************************************************************************************************

## As opposed to K-means clustering, hierarchical clustering is method of cluster analysis which seeks to build  a hierarchy
## of clusters.

# Lets assume some points in a two dimensional space and assume that each of these points are documents containing words. Let us also
# assume that each of these documents are singleton clusters by itself.

# First lets look for a pair of clusters which are close to each other (pair of documents..) so we put a link between them and they become 
# a single cluster. Now the link height is dependent on the distance between the pairs.. More is the distance more is the height of the link
# These links develops into a tree like structure called the dendogram which shows hierarchial relationship between the clusters

# Now how deep we want the tree to grow ? Depends on how many clusters are needed. For this we pickup a threshold and we cut the tree
# according to the threshold. If we cut the tree at the top we get fewer bigger clusters and if we cut the tree towards the bottom, we get
# more clusters smaller in sizes..

rm(list = ls()) # We clear all runtime variables in the Environment

# Use of external libraries
library(tm)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)
library(ggdendro)

# we will load the datasets (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems
switch(Sys.info() [['sysname']],
       Windows = {obama1 <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama1.csv", header = FALSE)
       obama2 <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama2.csv", header = FALSE)
       obama3 <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama3.csv", header = FALSE)
       obama4 <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama4.csv", header = FALSE)},
       
       Linux   = {obama1 <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama1.csv", header = FALSE)
       obama2 <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama2.csv", header = FALSE)
       obama3 <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama3.csv", header = FALSE)
       obama4 <- read.csv("//10.0.1.1/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama4.csv", header = FALSE)},
       
       Darwin  = {obama1 <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama1.csv", header = FALSE)
       obama2 <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama2.csv", header = FALSE)
       obama3 <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama3.csv", header = FALSE)
       obama4 <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama4.csv", header = FALSE)})

# We will combine these individual into one 
obama <- rbind(obama1,obama2, obama3, obama4)
# Check the data types
str(obama)
# Check the names of Obama
names(obama) # The names are V1 and V2 because we havd had header=FALSE , while we read the dataset

# In this we need only the V2 column, which is the actual tweets

# We convert to a data frame and only the second column
obamatweets <- data.frame(obama$V2)
names(obamatweets) <- "Tweet_Text"
# Now lets see the structure
str(obamatweets) # We see that the tweets column has been converted to a factor

# ********** Data Pre-Processing **************************************************** #

# Building the text corpus (a large structured set of texts or documents)
# To build a corpus , we use the function corpus and specify the source
# My source in this case is a vector - it can be data frame, xml, URI and many more
obamatweets.corpus <- Corpus(VectorSource(obamatweets$Tweet_Text))
# check the summary
summary(obamatweets.corpus) # This Corpus is a collection of 1074 text documents
# inspecting the first five elements of the corpus
inspect(obamatweets.corpus[1:5])
# showing the content of the first element
obamatweets.corpus[[1]]$content # OR 
content(obamatweets.corpus[[1]])

# Now we do some data transformations - this is basically to clean the data
# Since R is casesenstive, so we either convert the data to upper case or lower case...
obamatweets.corpus <- tm_map(obamatweets.corpus, tolower)
# We would remove the extra white space
obamatweets.corpus <- tm_map(obamatweets.corpus, stripWhitespace)
# Remove the punctuations
obamatweets.corpus <- tm_map(obamatweets.corpus, removePunctuation)
# Remove the numbers
obamatweets.corpus <- tm_map(obamatweets.corpus, removeNumbers)

# Stopwords in English language are words, which do not add any additional meaning - for ex all conjunctions
# to check the stopwords in english language
stopwords('english')

# Now we will remove some stopwords from data. We can add more words apart from the standard list as well
stopwords_list <- c(stopwords(kind = "en"),'http*')
obamatweets.corpus <- tm_map(obamatweets.corpus, removeWords, stopwords_list)

# Some of the tm_map functions do not return TextDocument and instead returns characters and the DocumentTermMatrix isnt sure how to handle 
# a corpus of characters and hence without this conversion, the term document matrix will fail
obamatweets.corpus <- tm_map(obamatweets.corpus, PlainTextDocument)

# Now lets again see the first two contents to verify the transformations

content(obamatweets.corpus[[1]])
content(obamatweets.corpus[[2]])

# ********** Text Mining **************************************************** #

# Building a term document matrix
obamatweets.tdm <- TermDocumentMatrix(obamatweets.corpus)
obamatweets.tdm # This shows number of words in the total number of documents (1074 in this case)
dim(obamatweets.tdm) # The matrix has 1800 rows and 1074 rows

# Remove sparse terms
# A sparse parameter of 0.9 we select from those terms which are less than 90% empty
obamatweets.rs <- removeSparseTerms(obamatweets.tdm, sparse = 0.97)
obamatweets.matrix <- as.matrix(obamatweets.rs)


# Finding the distance metric between the terms in the document
distmatrix <- dist(scale(obamatweets.matrix), method = "euclidean")
# Applying hierarchial clustering algorithm
obamatweets.h <- hclust(distmatrix, method = "ward.D2")

# The cluster height used in the dendogram
obamatweets.h$height
# labels for each of the objects being clustered
obamatweets.h$labels

# to get a flat clustering
k <- cutree(obamatweets.h, k = 5) # we want 5 clusters

# plot the dendogram  
ggdendrogram(obamatweets.h, cex = 0.9) # a basic plot
ggdendrogram(obamatweets.h, rotate = FALSE, size = 4, theme_dendro = FALSE, color = "tomato")
