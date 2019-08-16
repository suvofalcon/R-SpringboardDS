# *************************************************************************************************************
# Text Mining - Demonstration TM Package
#
# This file demonstrates various uses of the TM package in R for Text Mining
# The data is a collection of various tweets about obama 
# **************************************************************************************************************
rm(list = ls())

# Use of external libraries
library(tm)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)
library(stringr)

# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems
obama1 <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama1.csv",header = FALSE)
obama2 <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama2.csv", header = FALSE)
obama3 <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama3.csv", header = FALSE)
obama4 <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/obama4.csv", header = FALSE)

# First step in text mining is data aggregation
# 1 - Typically this can be done using social media listening tools such as Radian6, Buzz Metrics
# 2 - Accessing API - Application Programming Interfaces i.e, Twitter, Facebook, Google Analytics
# 3 - Web Crawling i.e, data from blogs, review forums etc

# Examples of handling twitter data
# We have taken data from twitter about President Obama
# There are four CSV files, we read all of them one ny one

# We will combine these individual into one 
obama <- rbind(obama1,obama2, obama3, obama4)
# Check the data types
str(obama)
# Check the names of Obama
names(obama) # The names are V1 and V2 because we have had header=FALSE , while we read the dataset

# In this we need only the V2 column, which is the actual tweets

# We convert to a data frame and only the second column
obamatweets <- data.frame(obama$V2)
names(obamatweets) <- "Tweet_Text"
# Now lets see the structure
str(obamatweets) # We see that the tweets column has been converted to a factor

# This will remove all non-graphical characters
#obamatweets$Tweet_Text=str_replace_all(obamatweets$Tweet_Text,"[^[:graph:]]", "") 

# ********** Data Pre-Processing **************************************************** #

# Building the text corpus (a large structured set of texts or documents)
# To build a corpus , we use the function corpus and specify the source
# My source in this case is a vector - it can be data frame, xml, URI and many more
obamatweets.corpus <- Corpus(VectorSource(obamatweets$Tweet_Text))
# check the summary
head(summary(obamatweets.corpus))
dim(summary(obamatweets.corpus)) # This Corpus is a collection of 1074 text documents
# inspecting the first five elements of the corpus
inspect(obamatweets.corpus[1:5])
str(inspect(obamatweets.corpus[1:5]))
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

# if we want to inspect first 10 rows and the first 10 columns
inspect(obamatweets.tdm[1:10, 1:10])
 
# Removing sparse terms - Term-document matrices tend to get very big already for normal sized data sets.
# We would want to remove those terms which occur infrequently. We would remove all those terms that have at least 97 percentage of sparse elements
obamatweets.tdm.imp <- removeSparseTerms(obamatweets.tdm, 0.97) 
obamatweets.tdm.imp # We see the number of words have been reduced from 1800 to 25
# if we want to inspect first 10 rows and the first 10 columns
inspect(obamatweets.tdm.imp[1:10, 1:10])

# Now we will find word and frequencies
wordFrequencies <- data.frame(apply(inspect(obamatweets.tdm.imp), 1, sum))
# giving proper structure to the data frame
wordFrequencies <- data.frame(Word = row.names(wordFrequencies), Freq = wordFrequencies[,1])
head(wordFrequencies)

# Lets try to order the data frame in decreasing order, to see which ones have highest and very high frequencies
wordFrequencies <- wordFrequencies[order(wordFrequencies$Freq, decreasing = TRUE), ]
# we dont need the row names
row.names(wordFrequencies) <- NULL
View(wordFrequencies)

# We will have a graphical representation of the same (Cleveland and Dot Plot)
ggplot(wordFrequencies, aes(x = Freq, y = reorder(Word, Freq))) + geom_point(size = 2) + 
  geom_segment(aes(yend = Word), xend = 0, colour = "darkblue") + xlab("Frequencies") + ylab("Words")


# We will now do some basic analysis
# Finding the most frequent terms/words
findFreqTerms(obamatweets.tdm, 10) # Words that are occuring atleast a minimum 10 times
findFreqTerms(obamatweets.tdm, 20) # Words that are occuring atleast a minimum 20 times
findFreqTerms(obamatweets.tdm, 30) # Words that are occuring atleast a minimum 30 times
findFreqTerms(obamatweets.tdm, 50) # Words that are occuring atleast a minimum 50 times
findFreqTerms(obamatweets.tdm, 70) # Words that are occuring atleast a minimum 70 times

# Finding association between terms/words - Which is finding correlation between the words
findAssocs(obamatweets.tdm,"republicans", 0.4) # Which words have atleast >= 40% correlation with the word republicans
findAssocs(obamatweets.tdm,"climate", 0.3) # Which words have atleast >= 30% correlation with the word climate
findAssocs(obamatweets.tdm,"myanmar", 0.3) # Which words have atleast >= 30% correlation with the word myanmar
findAssocs(obamatweets.tdm,"immigration", 0.5) # Which words have atleast >= 50% correlation with the word immigration

# Building a Word Cloud
# Visualizing all the frequently occuring terms with the help of word cloud

# First we need to chose a palette for colors - to check all available color palettes
display.brewer.all()
# now if want to use Dark2 palette and chose all 8 amongst the 8 availabe colors, we use
display.brewer.pal(8, "Dark2") # Similarly we can do for other palettes as well

# Now we will plot the word cloud
wordcloud(obamatweets.corpus, min.freq = 20, max.words = 100, random.order = TRUE, colors = brewer.pal(8, "Dark2"))
# This shows the words which has occured at a minimum 20 times and more and not more than 100 words are shown in the cloud
# Obama word has occured max number of times followed by myanmar, president, suu, kyi and others - the color depiction, size and bold are in the order 
# of the frequencies of the word occuring
# we can specify the font etc in the wordcloud
wordcloud(obamatweets.corpus, min.freq = 20, max.words = 100, random.order = TRUE, colors = brewer.pal(8, "Dark2"), vfont = c("script","plain"))
