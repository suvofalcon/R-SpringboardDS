# ********************************************************************************************************
# Association Rules - Market Basket Analysis
#  
# Data Used - groceries.csv
# 
# We would try to find association between items in the store by analysing the
# purchase transactions. To discover interesting patterns and relationships
# We would use this for recommendations, rearrangement of items, promotional offers etc
#     
# ********************************************************************************************************
rm(list = ls()) # Clear the environment data and run time variables
options(scipen = 999) # No automatic round offs

# Use of external libraries
library(arules)
library(arulesViz)

# Use the dataset Groceries in arules package
data("Groceries") # this data is transactions

# To view the details of this,we use
summary(Groceries)
# The above creates a sparse matrix, with rows 9835 (which are number of transactions) and 169 columns 
# which is the total number of items
# The sparse matrix is essentially a representation of which items are a part of which transaction (denoted by 1)
# and the items which are not a part of that transactions (denoted by 0)
# In this kind of a matrix, number of 0 will be way higher than number of 1 and hence this is known as sparse matrix.

# This shows that density is 0.02609146 - which is a representation of non empty cells in the matrix. Or in other words
# total number of items which was purchased in a transaction / total number of items which was possible to be in 
# that transaction - Or how many cells in that matrix is not zero..
# To calculate - the total number of cells in the sparse matrix is (9835 * 169)
# Total number of non empty cells - (9835 * 169) * density

# Then we see most frequent item is Whole Milk and it was purchased in 2513 transaction
# Second most is other vegerables and it was purchased in 1903 transactions out of 9835 transactions

# Next is the size of the transactions
# For example 2159 transactions had just 1 item
# 1005 transactions had just 4 items
# The highest in this dataset is 32 items - which was bought in 1 transaction

# Then we get some summary statistics on the transaction sizes..
# Min number of item bought is 1 and max is 32, mean is 4.409 which means maority transactions were of fewer items
# Its a highly positively skewed distribution - shows 75% of the transactions had lesser than 6 items being bought

# The last information has the items alphabetically arranged and states that the first column in the sparse matrix
# is abrasive cleaner, second is artif.sweetner etc


# ********** Sparse Matrix Exploration  *************************************** #

# to explore the matrix we use inspect command
inspect(Groceries[1:3]) # inspect the first three transactions

# Now to look at the support - we use itemFrequency - how frequently the item has occured in the data
# Support - what percentage of transaction does the particular item show up

itemFrequency(Groceries[,2]) # to check the itemFrequency of the second item in the Groceries dataset
# This shows that this item has appeared in about 9% of the total transactions
# so 0.09395018 * 9358 = total transactions where this item has appeared

# Now to check the support for first five items
itemFrequency(Groceries[,1:5]) # to check the item frequencies (support) for the first five items
itemFrequency(Groceries) # to check for all items (not practical, because number of items could be very large)

# to graphically see the item frequencies
# This will plot the item frequencies for all the items which has less 
itemFrequencyPlot(Groceries, support = 0.20) # Lets only look at items that show up in 20% and more of transactions
# To specifically check the support for whole milk
itemFrequency(Groceries[,"whole milk"])
# We see only, whole milk... lets reduce the support to 10%
itemFrequencyPlot(Groceries, support = 0.10, col = "darkBlue")

# Another way to use the itemFrequencyPlot - for example if we want to see the top 15 items
itemFrequencyPlot(Groceries, topN = 15, col = "darkblue")


# Now to calculate the confidence
# Confidence - Is a measure of the proportion of transactions where the presence of an item or a set of items, results
# in the presence of another item or another set of items
# So if i buy Item A, Item B - How likely it is that I will buy Item C

# So we are essentially looking for items or item set which have high support and high confidence


# ********** Data Modelling  *************************************** #

# To discover the rules, we do the modelling

model_rules <- apriori(Groceries) # if we run like this, the algorithm will run for default support of 0.1 and
# default confidence of 0.8
model_rules 
# When we run this, we see there are no rules, because the default minimums are more stringent
# and the algorithm could not find any rules
# We will re-run this by relaxing the criteria

model_rules <- apriori(Groceries, parameter = list(support = 0.007, confidence = 0.25, minlen = 2))
# minlen is the minimum length of the rule
# if we look at the model_rules now
model_rules # we see there are 363 rules
# to check the summary
summary(model_rules)

# We see that 363 rules has been divided into 137 rules with 2 items, 214 rules with 3 items and 12 rules with 4 items
# 3 items rule means - If i buy item 1 and item 2 - it is likely that i will buy item 3

# Lets look at some of the rules using inspect
inspect(model_rules) # to see all 363 rules
# Lets say, we want to see the first five rules
inspect(model_rules[1:3])
# The first rule says, that customers who are buying herbs are also likely to buy root vegetables
# It has a support for 0.007 and Confidence of 43% and lift of 3.95
# Lift of 3.95 for this rule means - how much likely root vegetables are going to be purchased with herbs
# as compared to root vegetables purchased in general in all the transactions (in result terms - it is 4 times more likely)

# Lets sort the model_rules by lift and view the top 4
inspect(sort(model_rules, by = "lift") [ 1:4])
# The rule 44 says sour cream shows up with berries in a transaction 3.8 times more than in general transactions
# So may be we can keep sour cream in close to berries in the aisles in super market or if someone buys berries
# online, we may recommend purchase of sour cream - or may give special deal or promos

# lets look at some more rules
inspect(sort(model_rules, by = "lift") [ 4:10])

# Now these rules needs to be reviewed by business, subject matter experts and decision, action plans needs to be
# taken

# Lets say we want to visualize these rules, we can do this using
plot(model_rules, method = "graph", interactive = TRUE, shading = NA)
