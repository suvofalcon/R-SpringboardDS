# *************************************************************************************************************
# Text Mining - Machine Learning Algorithms for Topic Modelling
#
# Technique shown here is called Latent Drichilet Analysis or LDA
#
# A sample dataset containing reviews about Sony watches from Amazon
#
# This code has been developed using R version 3.2.X (hence few new function calls has been made)
# 
# **************************************************************************************************************

# Topic Modelling - If I have 100M news Articles and if there is a simple question, as to what is there in those 100 M news articles, what
# type are they? what are the topics discussed in these articles..
# Topic Modelling is an approach to try and understand, what the document is talking about. It uncovers, what are the latent (hidden) topics 
# are in the corpus. 
# Assumption of Topic Modelling, is every document is made up of multiple topics and words in the document are generated from multiple topics

# Document -> Topics -> Words

# Types of Topic Modelling (Algorithms)
# - Latent Semantic Indexing (LSI)
# - Probalistic Latent Semantic Indexing (PLSI)
# - Latent Dirichlet Allocation (LDA) - most commonly used

# Intuition Behind LDA
#---------------------------
# A generative Model - Discovered in 2003 by David Blei, Andrew Ng and Michael Jordan

# It is known as generative Model, because it mimicks the exact writing process of a document - given a bunch of words, it generates the document.

# Lets say for example - We want to write a document about a Technology Company A investing or acquiring another technology company B.

# We would refer here a bags of words and each bags will be for each topics etc

# To write the document, we would need words, we would need Topic and we would then write the document. 
# Lets assume, that we decide the topics, which is going to be a part of the document. We are going to mention about the companies taking part in the
# acquisition, we would mention about the technology which is the primary reason behind the acquisition and we would need to mention about the financials
# related to the acqusition. So Topic1 - Companies, Topic2 - Technology and Topic3 - Financials.
# Now the article may also draw from other topics as well, for example Government, but the above are the main topics where majority of the word
# will belong.
# Now lets assign some weightages against these topics... Lets say, we assign 25% weightage to Companies, 70% weightage to Technology (70% of my
# words in the document will come from Technology Topic) and 5% weightage to topic financials. These weightages are based on how we want to write
# the document.

# Now we look for the words into the respective topic bags, pull the words out and frame the document. We ensure that 25% of the words in the document
# are pulled out from Companies bag, 75% words are pulled out from Technology bag and 5% are pulled out from Finance bag.

# Mathematics Behind LDA
#---------------------------
# Lets assume we have a collection of documents and we want to identify the underlying topics in the document. Assume each document contains
# a mix of different topics and lets also assume that topics are nothing but a collection of words that have different probability of appearance
# in the passages discussing the topic.

# Lets say we have two topics and words within them
# Topic1 -- technology, Satellite, internet, Space exploration...
# Topic2 -- money, investment, billion, dollars, financing...
# Now if we get a new word W, how are we going to decide, whether a new word W belongs to Topic T? 

# Questions to consider 
# A - How often does the word "money" appear in Topic T elsewhere? If it often occurs in discussions of Topic T, then this instance of "money"
# might belong to Topic T as well.
# B - How common is Topic T in the rest of the document

# We use Bayes theorem here to find out the probability of new word W in Topic T.
# For each possible Topic T - Multiply the frequency of this new word W in T by the number of other words in Document D that already belong to T
# The result will represent the probability that this word came from Topic T

# P(T|W, D) = (#Words W in Topic T) * (#Words in D that belong to T) /(# Words in Document D)

# We would find the probability of this word W belonging to every Topic and We would chose the topic that has the 
# highest probability for the new word

# This is also known as probabilistic topic modelling because of the overall emphasis on the probabilities.
# We start with randomly assigning each word to a topic and then using bayes theorem, we could go through the collection word by word and 
# re-assign each word to a topic. As we do this, there are two things, that happen
# -- 1. Words will gradually become more common to topic where they are already common
# -- 2. Topic will become more common to documents, where they are already common.
# Thus model will gradually become more consistent as topic focusses on more words and documents.
# But there will never be perfect consistency, because in practical terms there is no one to one mapping between a word and a document.

# LDA assumes that each document has a topic distribution and every topic has an underlying distribution of words
# Document exhibits multiple topics and each document exhibits topics in different proportion and each word in each document is drawn from
# one of these topics where the selected topic is chosen from the per document distribution over topics

# **************************************************************************************************************
rm(list = ls()) # Clear the runtime variables in the environment

# Use of external libraries
library(tm)
library(RTextTools)
library(topicmodels)
library(ggplot2)

# Load the data
data("NYTimes")
# Lets check the dimension of the data
dim(NYTimes)

# We will work with a subset of 1000 rows chosent randomly all rows to be unique and will not repeat in the sample (replace = FALSE)
nytimessubset <- NYTimes[sample(1:3104, size = 1000, replace = FALSE), ]
# lets check the datasubset
dim(nytimessubset)
head(nytimessubset)

# We will create a Document Term Matrix, by taking only the text columns from the subset data
# We will also clean the data alongside
docMatrix <- create_matrix(cbind(as.vector(nytimessubset$Title), as.vector(nytimessubset$Subject)), language = "english", removeNumbers = TRUE, 
                           removePunctuation = TRUE, removeSparseTerms = 0, removeStopwords = TRUE, stripWhitespace = TRUE, toLower = TRUE)

inspect(docMatrix[1:10, 1:10])

# Lets find out number of unique topics existing in the subset
nTopics <- length(unique(nytimessubset$Topic.Code)) # We see 27 unique topics are present in the NYTimes subset

# Divide data into Training and test matrix (we will use 70% for training and 30% for test data)
train_docMatrix <- docMatrix[1:700, ]
test_docMatrix <- docMatrix[701:1000, ]

# Building the model on train data - First parameter is the training data and the second is the number of topic we want from the document
train_lda <- LDA(train_docMatrix, nTopics)

# Once we have run the LDA, now we want to see for every document, we want to see five topics amongst max of 27 present in the entire superset.
# We want to see just five topics from every document. These five topics are there in some proportions
get_topics(train_lda, 5)
# For every document, it shows five topic numbers which are associated with that document in some proportions

# Also if we want to see the terms in each of the topics
get_terms(train_lda, 5) # We would want to see five terms in each of the topic

# If we want to see the highest probability occurence of topic for every document
# This is calculated internally by the probability values from the topic distribution itself
topics(train_lda)

# If we want to see the term which has occured the max in each of Topic
terms(train_lda)

# Now we will write the terms in a CSV file separately
write.csv(data.frame(get_terms(train_lda, nTopics)),"terms_nytimes.csv", row.names = FALSE)

# get the top most topics for every document
train.topics <- topics(train_lda) # So now we know, that all 700 documents have been assigned a topic (one which is the most occuring)

# Testing the model
test.topics <- posterior(train_lda, test_docMatrix)
# Now lets see the contents - 10 rows and maybe just five columns
test.topics$topics[1:10, 1:5] # The row number starts from 701 because the test data is from 701 to 1000
# This shows for every document what is the probability (distribution) of the topics

# Now we want to assign the topic which has the highest probability for every document in the test.topics
test.topics <- apply(test.topics$topics, 1, which.max)
test.topics # We will see for all the 300 documents has been assigned a topic code (the one which has the highest probability)

# We will now join the predicted topic number to the original test data
test_nytimessubset <- nytimessubset[701:1000, ]
finalTestDataset <- data.frame(Title = test_nytimessubset$Title, Subject = test_nytimessubset$Subject, Pred_topic = test.topics)
head(finalTestDataset)

# Frequency distribution of topic across various document
table(finalTestDataset$Pred_topic)

# In case we want to see the documents that belong to topic 7
View(subset.data.frame(finalTestDataset, finalTestDataset$Pred_topic == 7))

##--------------------Another method to choose the optimal number of topics ---------#

# Now in the above example, the topic codes for each of the document was given, based on which the model is built. In case we are in a scenario, where the topic
# code is not mentioned


# We need to find the best number of topics for the set of documents

# For this we need to build multiple LDA models on the same set of documents and then take the log likelihood
# Lets say we decide the topics would be between 2 - 10 (this range is arbitary and can be anything, but remember this is very CPU intensive)
# k is - we decide how many different topics we need to identify within a set of documents
# So best.model, will contain models with number of topics from 2 to 10
best.model <- lapply(seq(2, 10, by = 1), function (k){LDA(docMatrix, k)})

# Next for every LDA model with k topics (LDA model with topic-2, LDA model with topic-3 etc etc... we are going to find out the log likelihood)
best_model <- as.data.frame(as.matrix(lapply(best.model, logLik)))
# Log liklihood determines how good the model is with the associated k value (number of topics) -- higher the value of the log likelihood
# better the model is performing with the required number of topics (k)

# It is observed that as the topic number keeps on increasing, the log likelihood also keeps on increasing - till the time, we reach at 
# the optimum number of topics, beyond which further increase on topic numbers decreases the log likelihood

final_best_model <- data.frame(topics = c(seq(2,10, by = 1)), log_likelihood = as.numeric(as.matrix(best_model)))
head(final_best_model) # here we see that every model we generated with different topic number, what is the log likelihood value
# to visualize this graphically
with(final_best_model, qplot(topics, log_likelihood, col = "red"))

# We see that as the number of topics increases, the values of log likelihood also increases and for number of topics as 10, it gives the 
# highest log likelihood value - So we should chose that number which gives the highest log likelihood in place of nTopics and build the model
# in case when explicit topic distribution is not done in the training dataset

# Just to get the optimum number of topics
kOptimum <- final_best_model[which.max(final_best_model$log_likelihood),1]
cat("Best Topic Number is : ",kOptimum) # This is what we can verify from the graph as well 
