# *************************************************************************************************************
# Text Mining - Using TM Package explore the NYTIMES dataset
#
# A sample dataset containing labelled headlines. Analyse the subject of the articles. Subjects are mapped
# under Topic Code
#
# This code has been developed using R version 3.2.3 (hence few new function calls has been made)
# 
# The code analysis all subject rows in the dataset
# The code also delves specifically deep into two of largest topic (two topic codes which has largest articles)
#
# Lots of common code has been written in the form of functions
# **************************************************************************************************************
rm(list = ls())

# use of external libraries
library(tm)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)
library(grid)

# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems
switch(Sys.info() [['sysname']],
       Windows = {nytimes <- read.csv("//192.168.1.33/Data/CodeMagic/Data Files/TextMining/Assignments/Topic2-Tm Package/Dataset/nytimes.csv", header = TRUE)},
       Linux   = {nytimes <- read.csv("//192.168.1.33/Data/CodeMagic/Data Files/TextMining/Assignments/Topic2-Tm Package/Dataset/nytimes.csv", header = TRUE)},
       Darwin  = {nytimes <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/Assignments/Topic2-Tm Package/Dataset/nytimes.csv", header = TRUE)})

# Lets check the data load
dim(nytimes)
str(nytimes)
# check the first few rows
head(nytimes, 10)

# ********** To be used Functions **************************************************** #

###################################################
## Function - This function creates a Corpus and apply data cleaning and transformations based on TopicCode
## If TopicCode is NULL, it creates a corpus with all the Subject data elements
## Else it will filter the dataset and create corpus from the Subject data elements based on Topic Code
###################################################

createCleanCorpus <- function(dataset, subjectByTopicCode = NULL){
  # Since we would do analysis on the subject column - so we will pass on this column and the source is Vector
  if (is.null(subjectByTopicCode)){
    datasetCorpus <- Corpus(VectorSource(dataset$Subject))
  }
  else{
    datasetSubset <- subset.data.frame(dataset, dataset$Topic.Code == subjectByTopicCode)
    datasetCorpus <- Corpus(VectorSource(datasetSubset$Subject))
  }
  datasetCorpus <- tm_map(datasetCorpus, tolower) # all characters converted to lowercase
  datasetCorpus <- tm_map(datasetCorpus, stripWhitespace) # all uneven white spaces are removed
  datasetCorpus <- tm_map(datasetCorpus, removePunctuation) # remove punctuations
  datasetCorpus <- tm_map(datasetCorpus, removeNumbers) # remove Numbers
  datasetCorpus <- tm_map(datasetCorpus, removeWords, stopwords(kind = "en")) # remove stopwords
  
  # in R version 3.2.2 Some of the tm_map functions do not return TextDocument and instead returns characters 
  # and the DocumentTermMatrix isnt sure how to handle a corpus of characters and hence without this conversion, 
  # the term document matrix will fail
  datasetCorpus <- tm_map(datasetCorpus, PlainTextDocument)
  return (datasetCorpus)
}

###################################################
## Function - This calculates the Word Frequencies in a Term Document Matrix
###################################################

buildWordFrequencies <- function(termDocMatrix){
  wordFrequencies <- data.frame(apply(inspect(termDocMatrix), 1, sum))
  # giving a proper structure to the data frame
  wordFrequencies <- data.frame(Word = row.names(wordFrequencies), Freq = wordFrequencies[,1])
  # Lets try to order the data frame in decreasing order, to see which ones have highest and very high frequencies
  wordFrequencies <- wordFrequencies[order(wordFrequencies$Freq, decreasing = TRUE), ]
  # we dont need the row names
  row.names(wordFrequencies) <- NULL
  # print("Initial few rows of the ordered word frequencies...")
  # head(wordFrequencies)
  return(wordFrequencies)
}

###################################################
## Function - This builds the word cloud
###################################################

constructWordCloud <- function(corpusData, minFreq, maxWords){
  # First we need to chose a palette for colors - to check all available color palettes
  display.brewer.all()
  # now if want to use Dark2 palette and chose all 8 amongst the 8 available colors, we use
  display.brewer.pal(8, "Dark2") # Similarly we can do for other palettes as well
  # Now we will plot the word cloud
  wordcloud(corpusData, min.freq = minFreq, max.words = maxWords, 
            random.order = TRUE, colors = brewer.pal(8, "Dark2"))
}

# ********** Data Pre-Processing **************************************************** #

# Building the text corpus (a large structured set of texts or documents)

# Lets do the analysis with the whole of "Subject" Column and also by the top 4 topic codes.
nytimescorpus <- createCleanCorpus(nytimes)

# check the summary
head(summary(nytimescorpus))
dim(summary(nytimescorpus)) # This Corpus is a collection of 1000 text documents
# inspecting the first five elements of the corpus
str(inspect(nytimescorpus[1:5]))

# In R version 3.2.2 inspect will show the count of metadata and characters in the five documents
# of the corpus.. to see the content we use the content function
# Examine the first two content of the corpus
content(nytimescorpus[[1]]) # conntent of the first document
# OR 
nytimescorpus[[1]]$content
content(nytimescorpus[[2]]) # content of the second document

# Find out the first two most commonly used topic codes 
sort(table(as.factor(nytimes$Topic.Code)), decreasing = TRUE)
nytimescorpus_Topic19 <- createCleanCorpus(nytimes, 19)
nytimescorpus_Topic16 <- createCleanCorpus(nytimes, 16)
# check the summary
head(summary(nytimescorpus_Topic19))
dim(summary(nytimescorpus_Topic19)) # This Corpus is a collection of 216 text documents
head(summary(nytimescorpus_Topic16))
dim(summary(nytimescorpus_Topic16)) # This Corpus is a collection of 144 text documents

# Similarly lets check the content of ther first two documents in the corpus
content(nytimescorpus_Topic19[[1]]) # content of the first document for Topic Code = 19
content(nytimescorpus_Topic19[[2]]) # content of the second document for Topic Code = 19

content(nytimescorpus_Topic16[[1]]) # content of the first document for Topic Code = 16
content(nytimescorpus_Topic16[[2]]) # content of the second document for Topic Code = 16

# ********** Mining and Analysis ************************************************************ #

# Building the term document matrix

# For the entire Subject data in the dataset
# --------------------------------------------
# Lets build the term document matrix
nytimestdm <- TermDocumentMatrix(nytimescorpus)
nytimestdm # This shows there are 2492 terms in the matrix amongst 1000 documents
# Removing sparse terms - Term-document matrices tend to get very big already for normal sized data sets.
# We would want to remove those terms which occur infrequently. 
nytimestdm_imp <- removeSparseTerms(nytimestdm, 0.99)
nytimestdm_imp # This has been reduced to 34 terms in 1000 documents.
# These are the most frequently occuring term in the entire subset
# to see the word frequencies 
wordFrequencies <- buildWordFrequencies(nytimestdm_imp)
head(wordFrequencies)
# Lets try to visualize it through a cleveland and dot plot
# We will have a graphical representation of the same
ggplot(wordFrequencies, aes(x = Freq, y = reorder(Word, Freq))) + geom_point(size = 2) + 
  geom_segment(aes(yend = Word), xend = 0, colour = "darkblue") + xlab("Frequencies") + ylab("Words") +
  labs(title = "Most Frequent Words")
# To find words which has occured atleast 20 times and above
findFreqTerms(nytimestdm, 20)
# find association between words
findAssocs(nytimestdm, "clinton", 0.20) # Which words have atleast >= 20% correlation with "clinton"
findAssocs(nytimestdm, "election", 0.4) # Which words have atleast >= 40% correlation with "election"
# To plot the word cloud
constructWordCloud(nytimescorpus, 10, 100)
# On seeing the word cloud, we see that most of the articles have been related to US presidents 
# Jeorge Bush and Clinton and important events during their times. Clinton scandal, Iraq war during Bush,
# foreign policies related to China, Israel and dometic policies related to tax, healthcare etc. 


# In Depth analysis for Topic19 and Topic16
# --------------------------------------------

nytimestdm_Topic19 <- TermDocumentMatrix(nytimescorpus_Topic19)
nytimestdm_Topic16 <- TermDocumentMatrix(nytimescorpus_Topic16)
nytimestdm_Topic19 # There are 720 terms in the matrix
nytimestdm_Topic16 # There are 518 terms in the matrix
# Removing sparse terms and keeping the most important ones
nytimestdm_Topic19_imp <- removeSparseTerms(nytimestdm_Topic19, 0.98)
nytimestdm_Topic19_imp # This has been removed to 16 words
nytimestdm_Topic16_imp <- removeSparseTerms(nytimestdm_Topic16, 0.97)
nytimestdm_Topic16_imp # This has been removed to 17 words
# See the frequent words
wordFrequenciesTopic19 <- buildWordFrequencies(nytimestdm_Topic19_imp)
head(wordFrequenciesTopic19)
wordFrequenciesTopic16 <- buildWordFrequencies(nytimestdm_Topic16_imp)
head(wordFrequenciesTopic16)
# We will have a graphical representation of Topic - 19
plot1 <- ggplot(wordFrequenciesTopic19, aes(x = Freq, y = reorder(Word, Freq))) + geom_point(size = 2) + 
  geom_segment(aes(yend = Word), xend = 0, colour = "darkblue") + xlab("Frequencies") + ylab("Words") +
  labs(title = "Most Frequent Words - Topic19")
# We will have a graphical representation of Topic - 16
plot2 <- ggplot(wordFrequenciesTopic16, aes(x = Freq, y = reorder(Word, Freq))) + geom_point(size = 2) + 
  geom_segment(aes(yend = Word), xend = 0, colour = "darkblue") + xlab("Frequencies") + ylab("Words") +
  labs(title = "Most Frequent Words - Topic16")
# We will plot the two graphs for better visualization
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,1)))
print(plot1, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(plot2, vp=viewport(layout.pos.row=2, layout.pos.col=1))

# To find words which has occured atleast 10 times and above for both the topics
findFreqTerms(nytimestdm_Topic19, 10)
findFreqTerms(nytimestdm_Topic16, 10)
# find association between words
findAssocs(nytimestdm_Topic19, "israeli", 0.3) # Which words have atleast >= 30% correlation with "israeli"
findAssocs(nytimestdm_Topic16, "iraqi", 0.3) # Which words have atleast >= 30% correlation with "iraqi"

# To plot the word clouds
# Words which has occured atleast 5 times in Topic 19 to a limit of 100 words
constructWordCloud(nytimescorpus_Topic19, 5, 100) 
# Words which has occured atleast 5 times in Topic 16 to a limit of 100 words
constructWordCloud(nytimescorpus_Topic16, 5, 100)
# Topic 19 talks mainly about foreign policies during President Bush
# Topic 16 talks more about specifics of Iraq war... 
