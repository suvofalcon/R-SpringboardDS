# *************************************************************************************************************
# Text Mining - Machine Learning Algorithm of Network Analysis.
#
# Dataset Used - 
#
# How to build and analyze a network of tweets
#
# Useful to understand how many connections a specific word has with other words

# Using network terminology, our keywords are the nodes in a network, which are called vertices and 
# connections are named as edges.

# Using network analysis we can not only determine which all terms appear frequently together, but we can
# also visualize how are words and tweets are connected as network of terms. This way we can resolve the
# number of connections a specific word has with other words
# **************************************************************************************************************

# Weighted Network
# --------------------------
# In some networks, not all nodes and edges are created equal.. for example each of the edges may represent different types of interaction
# In biological network where the nodes represent enzymes, the edges will be different based on the different types of chemical interaction
# between them. 

# In a network if the nodes and edges can be represented by a single number, that can represent the strength of interaction. However a good model 
# can be weighted graph model. A weighted network (graph) can have different sizes of nodes and edges.
# In social network, not all contacts are friends, some may be familes , some may be colleagues - they have different levels of interactions between
# them, so we see nodes are of different nature and edges are of different widths

# Adjacency Matrix - represents each row for a node

rm(list = ls()) # We clear all runtime variables in the Environment

# Use of external libraries
library(igraph)
library(tm)

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

removeURLs <- function(x) gsub("http[[:alnum:]]*", "", x)
obamatweets.corpus <- tm_map(obamatweets.corpus, removeURLs)

# Stopwords in English language are words, which do not add any additional meaning - for ex all conjunctions
# to check the stopwords in english language
stopwords('english')

# Now we will remove some stopwords from data. We can add more words apart from the standard list as well
stopwords_list <- c(stopwords(kind = "en"),'http*','via','rt')
obamatweets.corpus <- tm_map(obamatweets.corpus, removeWords, stopwords_list)

# Some of the tm_map functions do not return TextDocument and instead returns characters and the DocumentTermMatrix isnt sure how to handle 
# a corpus of characters and hence without this conversion, the term document matrix will fail
obamatweets.corpus <- tm_map(obamatweets.corpus, PlainTextDocument)

# Now lets again see the first two contents to verify the transformations

content(obamatweets.corpus[[1]])
content(obamatweets.corpus[[2]])

# ********** Network Analysis **************************************************** #

# Building a term document matrix
obamatweets.tdm <- TermDocumentMatrix(obamatweets.corpus)
obamatweets.tdm # This shows number of words in the total number of documents (1074 in this case)
dim(obamatweets.tdm) # The matrix has 1460 rows and 1074 rows

# if we want to inspect first 10 rows and the first 10 columns
inspect(obamatweets.tdm[1:10, 1:10])

# Removing sparse terms - Term-document matrices tend to get very big already for normal sized data sets.
# We would want to remove those terms which occur infrequently. We would remove all those terms that have at least 97 percentage of sparse elements
# i,e terms that occuring 0 times in a document
obamatweets.tdm.imp <- removeSparseTerms(obamatweets.tdm, sparse = 0.97) # This also means we will retain all those words which are occuring or present n 
obamatweets.tdm.imp # We see the number of words have been reduced from 1460 to 24

# Transform TDM into a matrix
obama.m <- as.matrix(obamatweets.tdm.imp)
# to check the first 10 contents of this matrix
obama.m[1:10, 1:10]

# We will transform the matrix to a boolean matrix. Contains 1/0, indicates 1 for existing values other than zero
# Therefore we are just modifying the matrix to indicate yes or no for the presence of the word in the document

# convert to a boolean matrix
obama.m[obama.m >= 1] <- 1

# The next step is to convert this data into adjacency matrix
# Adjacency Matrix - will show how many connections each term has
# This will require the product of two matrices, using the %*% matrix operator
# Through the inner product of terms, we will arrive at the number of times each term appears together in a document

# Build a term adjacency matrix
obama.m2 <- obama.m %*% t(obama.m)
# to view the top 10 words
obama.m2[1:10, 1:10]

# Lets choose a subset of obama.m2
obama.test <- obama.m2[5:10, 5:10]
View(obama.test)

# The next step is to build an adjacency graph - this will create graphs from adjacency matrix
obama.graph <- graph.adjacency(obama.test, weighted = "TRUE", mode = "undirected")

# Once we get a graph object, lets extract Vertices/nodes
V(obama.graph)
# Lets extract the connections/edges
E(obama.graph)
get.edgelist(obama.graph) # This is also another wat to get the connections/edges

# Number of connections of each nodes - which is the degree of connections
degree(obama.graph, loops = TRUE)
degree(obama.graph, loops = FALSE) # loops = FALSE means, it will not consider its own occurence

# Since we have given weighted as TRUE - these weights are assigned in case some words occur more than the others
# to check, whether any words has been assigned
if (!is.null(E(obama.graph)$weight)){
  # In case the weights has been assigned, we can have the summary of weights
  summary_weights <- cbind(get.edgelist(obama.graph), round(E(obama.graph)$weight, 3))
} else{
  summary_weights <- NULL
} # We see no summary weights has been assigned

# Now we will again build an adjacency graph with mode as directed
obama.graph <- graph.adjacency(obama.test, weighted = "TRUE", mode = "directed")
# Number of connections of each nodes - which is the degree of connections
degree(obama.graph, loops = TRUE)
degree(obama.graph, loops = FALSE) # loops = FALSE means, it will not consider its own occurence

# If we compare the degree of directed graph vs the undirected graph we would see that the directed graph has twice the degree for every
# words as compared to undirected graph. This is because every bidirectional relationship in case of directed has been counted twice, which
# is not the case for undirected graphs. Since all edges are bidirectional, hence the degree is twice

# So our final choice would be undirected
obama.graph <- graph.adjacency(obama.test, weighted = "TRUE", mode = "undirected")

# before we plot the graph, we need to set the layout
?layout
layout1 <- layout.fruchterman.reingold(obama.graph, dim = 2) # dim = 2 means, it would be a two dimensional plot
# now plot the graph
plot(obama.graph, layout = layout1, vertex.size = 10, vertex.label.color = "darkred")

# Now lets try and build a network with bigger set of words occuring across the tweets

obama.m3 <- obama.m2[1:20, 1:20]
obama.g <- graph.adjacency(obama.m3, weighted = "TRUE", mode = "undirected")

# Remove loops
obama.g <- simplify(obama.g) # This will remove the self loops and hence it will have reduced number of edges

# Number of vertices
V(obama.g)
# Number of edges or connections
E(obama.g)
get.edgelist(obama.g)

# Just to get the count of number of edges and vertices
ecount(obama.g) # total number of connections
vcount(obama.g) # total number of nodes or vertices in the graph

# Since we have given weighted as TRUE - these weights are assigned in case some words occur more than the others
# to check, whether any words has been assigned
if (!is.null(E(obama.g)$weight)){
  # In case the weights has been assigned, we can have the summary of weights
  summary_weights <- cbind(get.edgelist(obama.g), round(E(obama.g)$weight, 3))
} else{
  summary_weights <- NULL
} # We see no summary weights has been assigned

# Now lets say, if we intend to assign some random weights to our list of connections we use a random uniform distribution
E(obama.g)$weight <- runif(ecount(obama.g))

# Number of connections for each person
degree(obama.g, mode = "out", loops = TRUE)

# set labels and degrees of vertices
V(obama.g)$label <- V(obama.g)$name
V(obama.g)$degree <- degree(obama.g)

# plot fruchterman reingold
layout1 <- layout.fruchterman.reingold(obama.g, dim = 2) 
# now plot the graph
plot(obama.g, layout = layout1, vertex.size = 2, vertex.label.color = "darkred")

# In case we want to use another layout called kamada kawai
plot(obama.g, layout = layout.kamada.kawai)

# Lets add some more modifications
V(obama.g)$label.cex <- 2 * V(obama.g)$degree/ max(V(obama.g)$degree) # This is going to help which vertices has higher connections
V(obama.g)$label.color <- "blue"
V(obama.g)$frame.color <- NA
plot(obama.g, layout = layout.kamada.kawai)

# only if weights exist
edge_weight <- (-log(E(obama.g)$weight)) / max(-log(E(obama.g)$weight))
E(obama.g)$color <- rgb(0.3, 0.3, 0, edge_weight)
E(obama.g)$width <- edge_weight
plot(obama.g, layout = layout.kamada.kawai)
plot(obama.g, layout = layout1, vertex.size = 2, vertex.label.color = "darkred")
