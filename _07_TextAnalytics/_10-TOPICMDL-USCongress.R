# *************************************************************************************************************
# Text Mining - Machine Learning Algorithms for Topic Modelling
#
# Dataset Used - USCongress.csv
#
# Building an LDA based Topic Model based on the "text" column in the dataset
# 1. ID - A unique identifier for the bill.
# 2. cong - The session of congress that the bill first appeared in.
# 3. billnum - The number of the bill as it appears in the congressional docket.
# 4. h_or_sen - A field specifying whether the bill was introduced in the House (HR) or the Senate (S).
# 5. major - A manually labeled topic code corresponding to the subject of the bill.
#
# Although a manually labeled topic code is given, but we will use the text column to build a topic model of our own
# **************************************************************************************************************
rm(list = ls()) # We clear all runtime variables in the Environment

# Use of external libraries
library(tm)
library(RTextTools)
library(topicmodels)
library(ggplot2)

# Load the USCongress data

# we will load the dataset (this is from SUVOS-TIME-CAPS)
# The load command will be slightly different for different Operating Systems
switch(Sys.info() [['sysname']],
       Windows = {USCongress <- read.csv("//SUVOS-TIME-CAPS/Data/CodeMagic/Data Files/TextMining/Assignments/Topic5-Topic modelling/dataset/USCongress.csv", 
                                     header = TRUE, stringsAsFactors = FALSE)},
       Linux   = {USCongress <- read.csv("//SUVOS-TIME-CAPS/Data/CodeMagic/Data Files/TextMining/Assignments/Topic5-Topic modelling/dataset/USCongress.csv", 
                                     header = TRUE, stringsAsFactors = FALSE)},
       Darwin  = {USCongress <- read.csv("//Volumes/Data/CodeMagic/Data Files/TextMining/Assignments/Topic5-Topic modelling/dataset/USCongress.csv", 
                                     header = TRUE, stringsAsFactors = FALSE)})
# Check the data load
dim(USCongress) # 4449 rows and 6 columns
head(USCongress)
str(USCongress)

# We will create a Document Term Matrix, by taking only the text columns from data and We will also clean the data alongside
docMatrix <- create_matrix(as.vector(USCongress$text), language = "english", removeNumbers = TRUE, 
                           removePunctuation = TRUE, removeSparseTerms = 0, removeStopwords = TRUE, stripWhitespace = TRUE, toLower = TRUE)

# Lets inspect the first 10 rows and first 10 columns
inspect(docMatrix[1:10, 1:10])

# ********** Find Optimum Topic Numbers **************************************************** #

# Lets find the best number of topics for these set of documents

# For this we need to build multiple LDA models on these set documents and then take the log likelihood
# Lets say we decide the topics would be between 2 - 30 (this range is arbitary and can be anything, but CPU intensive)

# k is - we decide how many different topics we need to identify within these set of documents
# So intermediate_model, will contain models with number of topics from 2 to 30
intermediate_model <- lapply(seq(2, 30, by = 1), function (k){LDA(docMatrix, k)})
# Next for every LDA model with k topics (LDA model with topic-2, LDA model with topic-3 etc etc... we are going to find out the log likelihood)
# We will transform the same intermediate_model
log_model <- as.data.frame(as.matrix(lapply(intermediate_model, logLik)))
# Log liklihood determines how good the model is with the associated k value (number of topics) -- higher the value of the log likelihood
# better the model is performing with the required number of topics (k)

# It is observed that as the topic number keeps on increasing, the log likelihood also keeps on increasing - till the time, we reach at 
# the optimum number of topics, beyond which further increase on topic numbers decreases the log likelihood
final_model <- data.frame(topics = c(seq(2,30, by = 1)), log_likelihood = as.numeric(as.matrix(log_model)))
final_model

# to visualize this
ggplot(final_model, aes(x = topics, y = log_likelihood)) + geom_line(col = "blue") + geom_point()
# We see that log_likelihood increases as the number of topics increases

# to find the optimum number of topics (max log_likelihood)
kOptimum <- final_model[which.max(final_model$log_likelihood), 1]
cat("Best Topic Number is : ",kOptimum) # This is what we can verify from the graph as well 


# ********** Classify individual Text to Topic Numbers **************************************************** #

# We will now use this topic number to classify individual Text in the dataset into one of topic numbers

# Divide data into Training and test matrix (we will use 70% for training and 30% for test data)
train_docMatrix <- docMatrix[1:3114, ]
test_docMatrix <- docMatrix[3115:4449, ]

# Building the model on train data - First parameter is the training data and the second is the number of topic we want from the document
train_lda <- LDA(train_docMatrix, kOptimum)

# Once we have run the LDA, now we want to see for every document, we would see three topics amongst max of 29 present in the entire superset.
# We want to see just 3 topics from every document. 
# For every document, it shows three topic numbers which are associated with that document in some proportions
get_topics(train_lda, 3)

# If we want to see the highest probability occurence of topic for every document
# This is calculated internally by the probability values from the topic distribution itself
topics(train_lda)
train.topics <- topics(train_lda)

# To see five terms in each of the topics
get_terms(train_lda, 5)
# If we want to see the term which has occured the max in each of Topic
terms(train_lda)

# Now we will apply this model in the test subset
test.topics <- posterior(train_lda, test_docMatrix)
# Now lets see the contents - 10 rows and 10 columns
test.topics$topics[1:10, 1:10] # The row number starts from 701 because the test data is from 701 to 1000
# This shows for every document what is the probability (distribution) of the topics

# Now we want to assign the topic which has the highest probability for every document in the test.topics
test.topics <- apply(test.topics$topics, 1, which.max)
test.topics # We will see for all the documents in the test subset that has been assigned a topic code (the one which has the highest probability)

# We will now join the predicted topic number to the original test data
USCongressTest <- USCongress[3115:4449, ]
finalUSCongressTestDataSet <- data.frame(Title = USCongressTest$text, Pred_topic = test.topics)
head(finalUSCongressTestDataSet)
View(finalUSCongressTestDataSet)

# to visualize the distribution of topics
topic_dist <- as.data.frame(table(finalUSCongressTestDataSet$Pred_topic))
ggplot(topic_dist, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity") + geom_text(aes(label = Freq), vjust = 1.5, colour = "white") + 
  xlab("Topic Numbers") + ylab("Number of Documents") + labs(title = "Topic Distribution by Documents")
