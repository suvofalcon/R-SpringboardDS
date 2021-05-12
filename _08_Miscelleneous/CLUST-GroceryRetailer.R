# ************************************************************************************
# How to do Clustering Using R
# ************************************************************************************

# We have a grocery retailer with 515 stores, spread across the KA and TN. The company wants to create store clusters
# based on mix of sales by category and avg.sales per square foot of space

# The Data Dictionary is as follows

# Store Num - Unique identifier for each store
# Cat1-4 :: Sales by Category (in 000 Rs)
# Cat1 - Fresh Foods Category
# Cat2 - Frozen Foods
# Cat3 - Health and Beauty
# Cat4 - Tobacco and alcohol
# Size: Area of Store (in Sqft)
# Sale: Total Sales of the store
# State : Location of the store (KA, TN) - Karnataka and TamilNadu

# load the dataset
stores.ds <- read.csv("//Volumes/Data/CodeMagic/Data Files/clustering_data-class13.csv", header = TRUE)
str(stores.ds)
head(stores.ds)
# some junk variable column got included... lets remove it from the dataset
stores.ds$X <- NULL

#********************* Data Exploration and Data Preparation Block ************************************

# Check for missing values
sum(is.na(stores.ds))
colSums(is.na(stores.ds)) # looks like there are no missing values in the dataset

# Now Since we need to create clusters based on sales by Category, Avg Sales per Square foot of space.. 
# We create few additional variables
# Sales per Square Foot = Total Sales/Store Size
stores.ds$PerSftSale <- (stores.ds$Sale/stores.ds$Size)
# Share of Sale by Category
# For example Proportion of Cat1 = Cat1Sales/TotalSales for every store and similarly for other category as well
stores.ds$PCat1 <- (stores.ds$Cat1/stores.ds$Sale)
stores.ds$PCat2 <- (stores.ds$Cat2/stores.ds$Sale)
stores.ds$PCat3 <- (stores.ds$Cat3/stores.ds$Sale)
stores.ds$PCat4 <- (stores.ds$Cat4/stores.ds$Sale)
names(stores.ds) # Just to check whether the variables have been added to the dataset

# Now lets perform some summary statistics
summary(stores.ds)
# for standard deviation
sapply(stores.ds[,9:13], sd, na.rm = TRUE) # standard deviation for the columns 9 to 13.. we can do for the entire dataset as well

#********************* Clustering Block ****************************************************************

# Now we will be getting ready for the clustering process. As per the problem statement, for the criteria of clustering
# We will be using only the derived variables i.e - from Columns 9 to 13. (PerSftSale to PCat4...)
# This is because we need to create clusters based on mix of sales by Category and Avg Sales Per Sft..

# So lets subset the data and create only those which is needed
#stores.sb <- subset.data.frame(stores.ds[,9 : 13])
stores.sb <- subset.data.frame(stores.ds[,9 : 13])
head(stores.sb)

# The first step of Clustering is to scale the data
# Scale function on a data frame, converts it to matrix... hence we convert it to back to data frame and assigns it to the same subject
stores.sb <- data.frame(scale(stores.sb))
class(stores.sb)
head(stores.sb)
# Now if we need to assign weights to the data as per its importance
# lets say we think the PerSftSale has more weightage and than sales proportion of each of the category, we assign weightage as
stores.sb$PerSftSale_3 <- stores.sb$PerSftSale * 3 # we give it a weight of 3.. weight of 3 is purely subjective
head(stores.sb)

# Now we will run the kmeans clustering
# kmeans() performs the k-means clustering. It stores its return in an object, which will have the cluster information
# The first argument is the dataset, on which it has to run
# The second argument gives the number of clusters that have to result from the clustering process

# fit <- kmeans(stores.sb[,2:5], centers = 8) # we want to run from column 2 onwards, because we want to use PerSftSale_3 variable which has the
# correct weightage applied on the PerSftSale variable
fit <- kmeans(stores.sb[,2:6], centers = 8)
names(fit)
fit

# Now we will have to evaluate the strength of the clusters from the output of the kmeans algorithm
# any clustering output gives two important paramters
# tot.withinss - total within group sum of squares. This provides a measure of the average distance between each member of the cluster
# betweenss - between group sum of squares. This provides a measure of the separation between cluster centroids.
# ratio of the two provides a measure of homogeneity within compared to heterogeneity outside
# this also means, how much variation has been thrown out of individual clusters.. lower the value means, too much variation exists within
# tot.withinss  - if it is very low, it means clusters are very and they are very homogeneous
# betweenss - if they are very high, it means clusters are well separated and they are very heterogeneous
# we can create a ratio of betweenss and withinss - higher the ratio better are the clusters
strength <- fit$betweenss/fit$tot.withinss
strength

# We can also run the next approach of clustering called the Hierarchial Clustering
# hclust() is a function which does the hierarchial clustering in R.. Before we can do hierarchial clustering, the data has to be converted
# into distances - using dist(). Distances are between every observations and the result is stored in a matrix
# Plotting the hclust() output draws the dendrogram ( which is the association diagram of the various records)
# cutree() is the method that lets one cut the tree at desired height
# table(hclus_cluster) lets one find out the number of clusters when the tree is cut at that height and the number of records in each cluster
# cbind() lets one attach the cluster information to the scaled dataframe
hclust_output <- hclust(dist(stores.sb[,2:6]))
plot(hclust_output) # this will plot the dendogram
# once we have arrived at the hierarchy, then we are going to cut 8 clusters from the hierarchy
hclust_clusters <- cutree(hclust_output, k = 8)  # k stands for how many clusters we want to cut it
table(hclust_clusters) # this is going to give me number of observations in each of the clusters
# Merge back the cluster information into the original data frame
stores.sb$clusters <- cbind(stores.sb, hclust_clusters)
head(stores.sb)

# The way to do this is to perform hierarchial clustering - get the centers of the the clusters and then use that information
# as the initial seed to the k-means algorithm, instead of the algorithm choosing the random seeds
# we will get the cluster centers
cluster_centers <- aggregate(stores.sb[,2:6], list(stores.sb$clusters$hclust_clusters),mean)
names(cluster_centers)
cluster_centers # each of the Group.1 variable is the center of the respective cluster( that respective row )
# now the first column is just informative and really not needed in real world need.. dropping the first column
cluster_centers$Group.1 <- NULL
# Now we will pass these centers to the k-means alogorithm
fit_1 <- kmeans(stores.sb[,2:6], centers = cluster_centers)
fit_1 # kmeans algorithm will be very very stable, as it is not going to randomly choose the seed, instead cluster_centers will be used
# as seed, which is obtained using Hierarchial Clustering

# Now we will pass this information back to the original dataset so that we can start doing profiling
stores.final <- data.frame(stores.ds, fit_1$cluster)
head(stores.final)
# aggregate the cluster means based on the cluster they belong to
cluster_mean <- aggregate(stores.final[,9:13], list(stores.final$fit_1.cluster), mean)
cluster_mean
# Again since we dont need the first column
cluster_mean$Group.1 <- NULL
cluster_mean
# Now if we have to explain the dataset .. we say for cluster 1 the avg PerSftSale is Rs 247.8896, the proportion of Cat1 sale is aropund 42.3%
# and so on ... 

# Now for profiling, we would need the mean of the population and that is
pop_means <- colMeans(stores.final[,9:13])
# Now if I have to compare the means of individual clusters with the means of the population, I can add the population mean data as the last
# row in the cluster_mean dataset
cluster_mean <- rbind(cluster_mean, pop_means)
round(cluster_mean, digits = 2) # rounding of all values in the datset to after 2 digits decimal
# The first 8 rows are the averages for the eight clusters and the 9th row is the population data
# Now in business terms if we need to interpret the cluster_mean dataset we say
# The average PerSftSale for the stores in the first cluster is Rs 250 and whereas the population is Rs 210. So the stores in the first cluster
# are doing much better than the general population
# Similarly for cluster 8 the Average PerSftSale is way higher than the general population and hence they are highly separated.
# if we need to see the details of those stores in cluster 8
subset.data.frame(stores.final, stores.final$fit_1.cluster == 8) # There are only three stores in that cluster
# Since this cluster is behaving very differently from rest of the population .. I would want to see
# the average size and average sale of all the stores by clusters
aggregate(stores.final[,6:7], list(stores.final$fit_1.cluster), mean) # we will do it for just Sale and State Variable
# the average size and sale for the whole population
colMeans(stores.final[,6:7])
# looking at the last two data, if we see very closely, although the store avg size for cluster 8 is almost half of the average store
# size of the population .. it is recording a much larger sale than the population average