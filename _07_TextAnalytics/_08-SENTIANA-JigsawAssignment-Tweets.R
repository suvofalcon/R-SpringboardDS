# *************************************************************************************************************
# Text Mining - Machine Learning Algorithms for Sentiment Classification on tweets.csv dataset
#
# Dictionary: 
# .	Classifier : Tells you whether the tweet is a positive sentiment(classifier=1) or negative sentiment (classifier=0)
# .	Date : When the tweets were extracted
# .	Text : The tweets
# .	Type : training or testing data
#
# .	A sample dataset containing random tweets extracted from twitter is given to you. 
# .	The classification of the tweets- whether a tweet is a positive or negative sentiment-is also provided to you
# .	Study the structure of the variables in the dataset [Very important]
# .	Build two classification models(SVM and Maximum entropy models) on the training data to classify the tweets as positive or negative
# .	Test the model
# .	Show the various model performance metrics used to validate the model
# .	Give a thorough description of your results obtained.
# 
# **************************************************************************************************************
rm(list = ls()) # clear all runtime environment variables

# Use of external libraries
library(tm)
library(RTextTools)
library(wordcloud)
library(caret)
library(ggplot2)
library(RColorBrewer)

# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems
switch(Sys.info() [['sysname']],
       Windows = {tweets <- read.csv("//SUVOS-TIME-CAPS/Data/CodeMagic/Data Files/TextMining/Assignments/Topic4-Sentiment Analysis/dataset/tweets.csv", 
                                      header = TRUE, stringsAsFactors = FALSE)},
       Linux   = {tweets <- read.csv("//SUVOS-TIME-CAPS/Data/CodeMagic/Data Files/TextMining/Assignments/Topic4-Sentiment Analysis/dataset/tweets.csv", 
                                      header = TRUE, stringsAsFactors = FALSE)},
       Darwin  = {tweets <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/Assignments/Topic4-Sentiment Analysis/dataset/tweets.csv", 
                                      header = TRUE, stringsAsFactors = FALSE)})

# Lets view the data load
str(tweets) # the data types are correct, because we loaded the data with stringsAsFactor = FALSE 
dim(tweets)
# first few rows
head(tweets)

# ********** To be used Functions **************************************************** #

###################################################
## Function - This function creates a Corpus and apply data cleaning and transformations 
###################################################

createCleanCorpus <- function(dataset){
  tempCorpus <- Corpus(VectorSource(dataset))
  tempCorpus <- tm_map(tempCorpus, stripWhitespace) # all uneven white spaces are removed
  tempCorpus <- tm_map(tempCorpus, removePunctuation) # remove punctuations
  tempCorpus <- tm_map(tempCorpus, removeNumbers) # remove Numbers
  tempCorpus <- tm_map(tempCorpus, removeWords, stopwords(kind = "english")) # remove stopwords
  
  # The latest version of tm (0.60) made it so we can't use functions with tm_map that operate 
  # on simple character values any more.Just replace it with content_transformer()
  # The content_transformer function wrapper will convert everything to the correct data type within the corpus.
  tempCorpus <- tm_map(tempCorpus, content_transformer(tolower)) # all characters converted to lowercase
  
  # in R version 3.2.2 onwards Some of the tm_map functions do not return TextDocument and instead returns characters 
  # and the DocumentTermMatrix isnt sure how to handle a corpus of characters and hence without this conversion, 
  # the term document matrix will fail
  tempCorpus <- tm_map(tempCorpus, PlainTextDocument)
  return (tempCorpus)
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


# ********** Data Exploration **************************************************** #

# We would not need the date column for the analysis or classification
tweets <- tweets[,-2]
head(tweets)

# based on the type, lets seggregate training and test dataset
train_tweets <- subset.data.frame(tweets, tweets$type == "train")
test_tweets <- subset.data.frame(tweets, tweets$type == "test")
# dimensions of training and test datasets
dim(train_tweets)
dim(test_tweets)

# Lets check the distribution of the classifier
table(tweets$classifier)
# shows there is almost equal proportions positive (1) and negative (0) sentiments

# to see the proportion of distribution of positive and negative sentiments between the training and test 
prop.table(table(train_tweets$classifier))
prop.table(table(test_tweets$classifier))
# The above proportions are also same ... so both training and test datasets has equal proportions between positive and negative sentiments

# We will create two corpus for positive and negative classifications
positiveTweetsCorpus <- createCleanCorpus(subset(tweets$text, tweets$classifier == 1))
negativeTweetsCorpus <- createCleanCorpus(subset(tweets$text, tweets$classifier == 0))

# Examine the first two content of both corpus
positiveTweetsCorpus[[1]]$content
positiveTweetsCorpus[[2]]$content
content(negativeTweetsCorpus[[1]])
content(negativeTweetsCorpus[[2]])

# Lets build the term document Matrix
positiveTweetsTDM <- TermDocumentMatrix(positiveTweetsCorpus)
negativeTweetsTDM <- TermDocumentMatrix(negativeTweetsCorpus)
positiveTweetsTDM # Shows 4578 terms
negativeTweetsTDM # Shows 4963 terms
# removing terms which occur infrequently
positiveTweetsTDM <- removeSparseTerms(positiveTweetsTDM, 0.99)
negativeTweetsTDM <- removeSparseTerms(negativeTweetsTDM, 0.99)
positiveTweetsTDM # Shows 96 terms after sparse term removal
negativeTweetsTDM # Shows 94 terms after sparse term removal

# Lets build the word frequencies
positiveWordFrequencies <- buildWordFrequencies(positiveTweetsTDM)
negativeWordFrequencies <- buildWordFrequencies(negativeTweetsTDM)
head(positiveWordFrequencies)
head(negativeWordFrequencies)

# Lets try to visualize it through a cleveland and dot plot
# We will have a graphical representation of the same
ggplot(positiveWordFrequencies, aes(x = Freq, y = reorder(Word, Freq))) + geom_point(size = 2) + 
  geom_segment(aes(yend = Word), xend = 0, colour = "darkblue") + xlab("Frequencies") + ylab("Words") +
  labs(title = "Most Frequent Words in Positive Classification")

ggplot(negativeWordFrequencies, aes(x = Freq, y = reorder(Word, Freq))) + geom_point(size = 2) + 
  geom_segment(aes(yend = Word), xend = 0, colour = "darkblue") + xlab("Frequencies") + ylab("Words") +
  labs(title = "Most Frequent Words in Negative Classification")

# Visualize Word Cloud in positive and negative classification
constructWordCloud(positiveTweetsCorpus, 10, 100)
constructWordCloud(negativeTweetsCorpus, 10, 100)

# ********** Classification Algorithms **************************************************** #

# We will build a document term matrix
docMatrix <- create_matrix(tweets$text, language = "english", removeNumbers = TRUE, removePunctuation = TRUE, removeSparseTerms = 0,
                           removeStopwords = TRUE, stripWhitespace = TRUE, toLower = TRUE)
# will convert it to a matrix data type
docMatrix <- as.matrix(docMatrix)
# Check the dimensions
dim(docMatrix)

# The next step is to pass this matrix to a container and specify the response variable and training
# and test rows 
tweetsContainer <- create_container(docMatrix, as.factor(tweets$classifier), trainSize = 1:2194, testSize = 2195:3135,
                                    virgin = FALSE)

# Now we will use the container and train the model
# We will use SVM and MAXENT to train on the data and understand the pattern
tweetsModels <- train_models(tweetsContainer, algorithms = c("SVM","MAXENT"))

# Once the model is run, we will now test the model - it will test both the models on the test data in the container
tweetsResults <- classify_models(tweetsContainer, tweetsModels)
# Check the initial few rows
head(tweetsResults)
# Check the dimensions, it should have the same size as that of the test data
dim(tweetsResults)

# ********** Model Performance Metrics **************************************************** #

# ---------- Confusion Matrix

# Confusion Matrix for SVM
confusionMatrix(tweetsResults$SVM_LABEL, test_tweets$classifier)
# Confusion Matrix for MAXENT
confusionMatrix(tweetsResults$MAXENTROPY_LABEL, test_tweets$classifier)

# True Positives(TP) = the number of cases which were correctly classified to be positive, i.e. 
# were predicted to be a success and were actually observed to be a success
# SVM - 296 and # MAXENT - 324

# False Positives(FP) = the number of cases which were incorrectly classified as positive, 
# i.e. were predicted to be a success but were actually observed to be a failure
# SVM - 122 and # MAXENT - 174

# True Negatives(TN) = the number of cases which were correctly classified to be negative, 
# i.e. were predicted to be a failure and were actually observed to be a failure
# SVM - 361 and # MAXENT - 309

# False Negatives(FN) = the number of cases which were incorrectly classified as negative, 
# i.e. were predicted to be a negative but were actually observed to be a success
# SVM - 162 and # MAXENT - 134

# Sensitivity is the proportion of observed positives classified correctly
# SVM - 74% and MAXENT - 64%

# Specificity is the proportion of observed negatives classified correctly
# SVM - 64% and MAXENT - 70%

# lets calculate the recall accuracy separately for the both the models
recall_accuracy(as.factor(test_tweets$classifier), tweetsResults$SVM_LABEL)
recall_accuracy(as.factor(test_tweets$classifier), tweetsResults$MAXENTROPY_LABEL)

# Both the model are projecting low kappa value, which infers there is a significant predictions, occuring 
# by random chance and not by predicting ability of the model

# ----------------- Model Summary
# We can create an analytics (depicting ensemble and algorithm performance)
tweetsAnalytics <- create_analytics(tweetsContainer, tweetsResults)
summary(tweetsAnalytics)

# Check the ensemble summary
tweetsAnalytics@ensemble_summary

# The analytics shows us that SVM algorithm performs marginally better in precision, recall accuracy and F-measure than 
# Maximum Entropy algorithm

# The ensemble agreement - shows that usage of 2 or more algorithms gives an optimum document coverage of 76% along with an optimal
# recall accuracy of the model by 75%.

# ********** Cross Validation and Final Classification **************************************************** #

# We will perform a K-fold cross validation technique, by considering K=4, which is essentially running the training and validation
# on 4 different combinations of training and test dataset from the overall data
k <- 4
set.seed(1234)
cross_validate(tweetsContainer, k, "SVM")
cross_validate(tweetsContainer, k, "MAXENT")

# Looking at the cross validation we see that mean accuracy of SVM is way higher than MAXENT and hence we finalize the SVM classification
# as the result classification of our model

# The final classification
tweetsResults$SVM_LABEL
