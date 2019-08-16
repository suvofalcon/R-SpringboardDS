# *************************************************************************************************************
# Text Mining - Machine Learning Algorithms for Sentiment Classification
#
# A sample dataset containing reviews about Sony watches from Amazon
#
# This code has been developed using R version 3.2.X (hence few new function calls has been made)
# 
# **************************************************************************************************************
rm(list = ls()) # clear the runtime environment variable
options(scipen = 999) # no automatic roundoffs

# Use of external libraries
library(tm)
library(plyr)
library(stringr)
library(ggplot2)
library(RTextTools)
library(e1071)
library(caret)

# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems
switch(Sys.info() [['sysname']],
       Windows = {reviews <- read.csv("//SUVOS-TIME-CAPS/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/reviews.csv", header = TRUE)},
       Linux   = {reviews <- read.csv("//SUVOS-TIME-CAPS/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/reviews.csv", header = TRUE)},
       Darwin  = {reviews <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/reviews.csv", header = TRUE)})

# View the data load
head(reviews)
str(reviews)
dim(reviews)
# We would be interested in keeping only the columns Date, review_title, review_data
reviews <- reviews[,c(5,7,8)]
# Now we check the subset data
names(reviews)
dim(reviews)
head(reviews)

# ********** Data Pre-Processing **************************************************** #

# Build a Text Corpus
tweets.corpus <- Corpus(VectorSource(reviews$review_title))
tweets.corpus
inspect(tweets.corpus[1:5])
# to inspect the content of the fifth element of the review
content(tweets.corpus[[5]]) # OR
tweets.corpus[[5]]$content

# Data Transformations - Cleaning
tweets.corpus <- tm_map(tweets.corpus, tolower) # Converting to lower case
tweets.corpus <- tm_map(tweets.corpus, stripWhitespace) # Removing extra white space
tweets.corpus <- tm_map(tweets.corpus, removePunctuation) # Removing punctuations
tweets.corpus <- tm_map(tweets.corpus, removeNumbers) # Removing Numbers
my_stopwords <- c(stopwords('english'), 'available') # Can add more words apart from standard list
tweets.corpus <- tm_map(tweets.corpus, removeWords, my_stopwords)

# ********** Tagging to Sentiments **************************************************** #

# In this case, we are going to take our reviews data and then we are going to see a match for 
# every words in the reviews with the words in the dictionary

# I have two dictionaries, positive words and negative words

# lets extract the reviews
tweets.text <- reviews$review_title

# Read the dictionaries (positive)
switch(Sys.info() [['sysname']],
       Windows = {pos <- scan("//SUVOS-TIME-CAPS/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/positive-words.txt", what = "character", comment.char = ";")},
       Linux   = {pos <- scan("//SUVOS-TIME-CAPS/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/positive-words.txt", what = "character", comment.char = ";")},
       Darwin  = {pos <- scan("//Volumes/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/positive-words.txt", what = "character", comment.char = ";")})

# Read the dictionaries (negative)
switch(Sys.info() [['sysname']],
       Windows = {neg <- scan("//SUVOS-TIME-CAPS/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/negative-words.txt", what = "character", comment.char = ";")},
       Linux   = {neg <- scan("//SUVOS-TIME-CAPS/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/negative-words.txt", what = "character", comment.char = ";")},
       Darwin  = {neg <- scan("//Volumes/Data/CodeMagic/Data Files/TextMining/ClassCodes/Datasets/negative-words.txt", what = "character", comment.char = ";")})

# The above reads the dictionaries as each word as character and they are separated by ';'
# Lets display the positive words dictionary
pos

# Lets add few more words to dictionaries
pos[2007:2013] <- c("spectacular","everyday","better","top","thumbs","four","five")
neg[4784:4789] <- c("one","two","careful","sync","Beware","suck")

# We will find out the match for every word in the reviews with every word in our dictionaries. We have loaded two dictionaries (postive and negative)

# Famous Jeffreybreen Algorithm to "Tag" sentiments to sentences

score.sentiment = function(sentences, pos.words, neg.words, .progress = "none"){
  
  # to get the scores
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    
    # clean up sentences with R's regex-driven global substitute, gsub()
    sentence <- gsub("[[:punct:]]","", sentence) # removes punctuation
    sentence <- gsub("[[:cntrl:]]","", sentence) # removes control characters
    sentence <- gsub("\\d+","", sentence) # removes digits 
    # OR sentence <- gsub("[[:digit:]]","",sentence)
    
    # and convert to lower case
    sentence <- tolower(sentence)
    
    # Split sentences into words, str_split is in the stringr package 
    word.list <- str_split(sentence, "\\s+")
    # We then unlist to convert into vector
    words <- unlist(word.list)
    
    # now compare the words to the dictionary of positive and negative words
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    # Wherever it will not find a match, it will return NA and wherever it finds a match a it will return the position of the matched word in the dictionary
    
    # Since match returns the position of the matched term or NA, we need TRUE/FALSE
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress = .progress)

  scores.df <- data.frame(score = scores, text = sentences)
  return(scores.df)
}

# Now lets use the function for analysis

analysis <- score.sentiment(tweets.text, pos, neg, .progress = "text")
names(analysis)
View(analysis) 
# This shows a score assigned to every statement - whether the score is accurate or not
# depends on how good or bad the dictionary is (bag of words)

# check the structure
str(analysis)
# Checking the overall sentiment
table(analysis$score) 
# The table shows how many positives and negatives and neutrals and individual breakups within the
# buckets
mean(analysis$score)
# visualizing the frequency distribution
scorefreq <- data.frame(table(analysis$score))
colnames(scorefreq) <- c("Scores","Freq")
ggplot(scorefreq, aes(x = Scores, y = Freq)) + geom_bar(stat = "Identity", col = "black") + 
  xlab("Scores") + geom_text(aes(label = Freq), vjust = 1.5, colour = "white")

analysis$text <- as.character(analysis$text) # this should not be treated as factor
str(analysis)
# now we will assign only three scores to the sentiments by adding a new column
analysis$sentiment <- ifelse(analysis$score > 0, "positive",
                             ifelse(analysis$score < 0, "negative",
                             "neutral"))
# now lets check the table form of this column
table(analysis$sentiment)
# check the few rows of this dataset
head(analysis)
# since there are presence of punctuations, we clean the data again
analysis$text <- gsub("[[:punct:]]","", analysis$text)
str(analysis)
head(analysis)

# Now the document is ready for its classification algorithms

# ********** Sampling data into Training and Text **************************************************** #

# split the data into training and test
sampling <- sort(sample(nrow(analysis), nrow(analysis) * 0.7))

# now subset according to sampling
train_tweets <- analysis[sampling,]
test_tweets <- analysis[-sampling,]

# if we want to see the distribution of the three classes in both the training and test dataset
prop.table(table(train_tweets$sentiment))
prop.table(table(test_tweets$sentiment))
# we see the distribution of positive, negative and neutral in both test and training dataset is almost same

# ********** Classification Algorithms **************************************************** #

# Tagging the testing and training datasets
dim(test_tweets)
dim(train_tweets)
# we will join these two dataset and introduce a column called type which will specify which column is 
# a part of training dataset and which one is a part of test dataset
train_tweets$type <- "train"
test_tweets$type <- "test"

tweets <- rbind(train_tweets, test_tweets)
dim(tweets) # back to original number of rows

# Now we will build a document term matrix
# this will take the second column value (text)
docMatrix <- create_matrix(tweets[,2], language = "english", removeNumbers = TRUE, removePunctuation = TRUE,
                           removeSparseTerms = 0, removeStopwords = TRUE, stripWhitespace = TRUE, toLower = TRUE)
# convert it to a matrix data type
mat <- as.matrix(docMatrix)
head(mat)

# The next step is to pass this matrix to a container and specify the response variable and training
# and test rows -- unless this is done, we wont be able to build any models
container <- create_container(mat, as.numeric(as.factor(tweets[,3])), trainSize = 1:244, testSize = 245:349,
                              virgin = FALSE)

# Now we will use the container and train the model
# Algorithms used to train the data : SVM, RF, TREE, MAXENT, BAGGING
models <- train_models(container, algorithms = c("MAXENT","SVM", "RF", "BAGGING", "TREE"))
# it will run all the above algorithms on the train data and understand and pattern

# Test the model
results <- classify_models(container, models) # now it will test all the models on the test data
class(results)
# check the initial row
head(results)
# for every algorithm it tells the classification and its respective probability
# 1 - negative, 2 - neutral and 3 - positive

# Now lets build the Accuracy Table and Confusion Matrix
table(results[,"FORESTS_LABEL"],tweets[245:349, 3]) # Confusion Matrix for Random Forests algorithm
# OR
confusionMatrix(results$FORESTS_LABEL, as.numeric(as.factor(tweets[245:349, 3])))
# Similarly
confusionMatrix(results$MAXENTROPY_LABEL, as.numeric(as.factor(tweets[245:349, 3])))

# Now to calculate the recall accuracy
recall_accuracy(as.numeric(as.factor(tweets[245:349, 3])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[245:349, 3])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[245:349, 3])), results[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[245:349, 3])), results[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(tweets[245:349, 3])), results[,"SVM_LABEL"])


# ********** Model Summary **************************************************** #

# We can create an analytics (depicting ensemble and algorithm performance)
analytics <- create_analytics(container, results)
summary(analytics)
# For every algorithm being run, it gives the precision, recall and f-measure
# What does this object contain
head(analytics@document_summary)

# ********** Ensemble Agreement **************************************************** #

# Coverage - this simply refers to the percentage of documents that meet the recall accuracy threshold

# When we use many algorithms to classify the data, then ensemble agreement is a good metric to look at
# This simply refers to whether multiple algorithms make the same prediction concerning the class of the
# event

# Its been proven that, if we build many models for predicting the data, and then if we take ensemble
# of the predictions, then there is a very high probability that the predicted values will be equal to the
# actual values

# Example - Using a four-ensemble agreement approach, Collingwood and Wilkerson found that when four of
# their algorithms agree on the label of a textual document -
# The predicted class matches the actual class over 90% of the time.
# The rate was just 45% when two algorithms agree on the text label.

# Ensemble agreement approach is on the majority approach (if majority of the algorithms agree on a classification)
# the major agreement is taken as the predicted classification

# to check the ensemble summary
analytics@ensemble_summary
# when we say n >=2 (number of algorithms), we see the coverage is 1 (number of documents participating)
# or agreeing on the recall accuracy which is 74%

# Now, we see as the number of algorithm increases, we see the number of participating document reduces
# but the recall accuracy percentage increases... 
# For example when number of algorithms is >= 5, number of documents participating is 63% of all total
# and all 63% agreeing on the recall accuracy of 91%..

# So the ideal situation would be to choose the number of algorithms where the number of participating
# documents are higher and so is the recall accuracy 

# So looking at the table below -- n >= 4 maybe the right number for the ensemble

# ********** Cross Validation **************************************************** #

# How do we chose these algorithms, we do it by the method of cross validation

# to test out the models, if we repeatedly take random samples from the data, then its possible that
# the same record, may appear in the sample more than once

# In K-Fold cross validation - data is divided into k completely separate paritions called "folds"

# Now if a machine learning model is built on 80% of the data and is tested on the folds matching
# 20% of the data - now after this training and validation of the model has occured k times, that is with
# k different training and testing data combination, then the average performance across all the folds
# gets reported - This is called the k-fold cross validation technique

N <- 4 # (number of folds)
set.seed(1024) # four different paritions remains the same
cross_validate(container, N, "MAXENT")
cross_validate(container, N, "TREE")
cross_validate(container, N, "SVM")
cross_validate(container, N, "RF")
cross_validate(container, N, "BAGGING")
# We can check the mean accuracy of both the algorithms and take the top four

# Lets subset the label columns from the results dataset
results1 <- results[, c(1,3,5,7,9)]

# For a moment, if we assume that all five algorithms are doing very good - by our ensemble agreement

# So what we want to do now, is for each row, we want to chose the classification which is majority
results1$majority <- NA
for(i in 1 : nrow(results1)){
  
  # Getting the frequency distribution for the classifications
  print(i)
  p <- data.frame(table(c(results$MAXENTROPY_LABEL[i], results$TREE_LABEL[i], results$FORESTS_LABEL[i],
                          results$SVM_LABEL[i], results$BAGGING_LABEL[i])))
  # choosing the classification that occurs maximum
  # putting this value into the new column "majority"
  results1$majority[i] <- paste(p$Var1[p$Freq == max(p$Freq)])
  #rm(p)
}
results1$majority <- as.numeric(results1$majority)
table(results1$majority)
results1$majority
head(results1)
