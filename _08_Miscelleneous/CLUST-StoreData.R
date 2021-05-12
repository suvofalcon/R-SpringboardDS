
# ************************************************************************************
# Clustering on Store Data - Create Store Clusters based on mix of Sales by Category
#                            and average sales per square foot of space.
# ************************************************************************************

# We have a grocery retailer with 515 stores, spread across the KA and TN. The company wants to create store clusters
# based on mix of sales by category and avg.sales per square foot of space

# Load the dataset
stores <- read.csv("//Volumes/Data/CodeMagic/Data Files/clustering_data-class13.csv", header = TRUE)
# Check the data load
head(stores)
# there is an invalid column which got included .. removing the same from the data set
stores <- stores[,1 : 8]
head(stores)
nrow(stores) # There are 515 observations
str(stores)

# ********** Data Exploration and Data Preparation Block **************************************************** #

# Check for missing values
sum(is.na(stores)) # There are no missing values in the data set

# Summary Statistics
summary(stores)
# get the standard deviation for each column except the categorical column
sapply(stores[,1:7], sd)

# We will take a look at the Sale and Size data graphically

# Store Size data
boxplot(stores$Size)
hist(stores$Size, labels = TRUE, col = "dark green", main = "Histogram for Store Sizes", ylab = "Number of Stores", xlab = "Store Sizes")
plot(stores$Size)
# Looking at the above data, we see majority of the stores sizes are between 2500 and 3500. However there are few stores which are very small in sizes..
# We take a look at those store data size < 2000
nrow(subset.data.frame(stores, stores$Size < 2000)) # There are 10 stores
# looking at details
subset.data.frame(stores, stores$Size < 2000)
# They are distibuted across the states of KA and TN and have sale data across all categories. They are just stores smaller in sizes. We dont have enough reasons
# to believe, that they are outliers.

# Store Sale Data
boxplot(stores$Sale)
hist(stores$Sale, labels = TRUE, col = "dark green", main = "Histogram for Store Sale", ylab = "Index", xlab = "Store Sale")
plot(stores$Sale, col = "blue", pch = 19, xlab = "Index", ylab = "Store Sales", main = "Scatter Plot for Store Sales")
# Looking at the above data, we see there is one store with Sale above 800 behaving differently from the rest of the population.. To take a look into details
subset.data.frame(stores, stores$Sale > 800) # We dont consider it as an outlier

# There is no definite relationship with the size of a store and store sales
plot(stores$Size, stores$Sale, pch = 18, col = "blue", main = "Stores Sizes vs Stores Sales", xlab = "Store Sizes", ylab = "Store Sales")
barplot(tapply(stores$Sale, stores$State, mean), col = "dark green", main = "Avg Sale by State", xlab = "State", ylab = "Avg Sale")
# The average sale across state are more or less the same
barplot(tapply(stores$Size, stores$State, mean), col = "dark green", main = "Avg Store Size by State", xlab = "State", ylab = "Avg Store Size")
# The average Store size across state are also same..

# We need to create cluster based on Sales by Category and Avg.Sales per Square foot of Space
# We will have to create few derived(additional variables)
# Sales per Sq Feet = Total Sale/Total Size of the stores
stores$SalePerSqFt <- stores$Sale / stores$Size
# Share of Sale by Category
# For Proportion of Cat1 Sale = Cat1/Sale - Similarly for other categories. We create this variable for every category for every stores
stores$P_Cat1Sale <- stores$Cat1 / stores$Sale
stores$P_Cat2Sale <- stores$Cat2 / stores$Sale
stores$P_Cat3Sale <- stores$Cat3 / stores$Sale
stores$P_Cat4Sale <- stores$Cat4 / stores$Sale

# Check on the added variables
head(stores)
str(stores)

#********************* Clustering Block ****************************************************************

# Now we will be getting ready for the clustering process. As per the problem statement, for the criteria of clustering
# We will be using only the derived variables i.e - from Columns 9 to 13. (SalePerSqFt to P_Cat4Sale)
# This is because we need to create clusters based on mix of sales by Category and Avg Sales Per Sft..

# Hence we will create a subset from the large data frame
stores.sb <- stores[,9 : 13]
head(stores.sb)
str(stores.sb)

# The first step of clustering is to scale the data
# The scale function converts the data frame to matrix and hence we convert it to data frame and assign it back
stores.sb <- data.frame(scale(stores.sb))
class(stores.sb)
head(stores.sb)
str(stores.sb)

# Next step is we assign the weightages
# As per the case study instruction, we assign a weightage of 3 to the SalePerSqFt variable
# This means that the clusters will be more differentiated based on SalePerSqFt variable, rather than P_CatXSale variable(s)
stores.sb$SalePerSqFt <- stores.sb$SalePerSqFt * 3
head(stores.sb)

# Now we will run the kmeans clustering
# kmeans() performs the k-means clustering. It stores its return in an object, which will have the cluster information
# The first argument is the dataset, on which it has to run
# The second argument gives the number of clusters that have to result from the clustering process

# k-means - Iteration 1 - We set clusters of 4
set.seed(1)
fit_clusters_4 <- kmeans(stores.sb, centers = 4, nstart = 1)
names(fit_clusters_4)
fit_clusters_4
# Number of Stores in each of the clusters
fit_clusters_4$size
# The Cluster averages for each of the clusters
fit_clusters_4$centers

# Now we will have to evaluate the strength of the clusters from the output of the kmeans algorithm
# any clustering output gives two important paramters
# tot.withinss - total within group sum of squares. This provides a measure of the average distance between each member of the cluster
# betweenss - between group sum of squares. This provides a measure of the separation between cluster centroids.
# ratio of the two provides a measure of homogeneity within compared to heterogeneity outside.
# This also means, how much variation has been thrown out of individual clusters.. lower the value means, too much variation exists within
# tot.withinss  - if it is very low, it means clusters are very homogeneous
# betweenss - if they are very high, it means clusters are well separated and they are very heterogeneous
# we can create a ratio of betweenss and withinss - higher the ratio better are the clusters
fit_clusters_4$betweenss/fit_clusters_4$tot.withinss
fit_clusters_4$betweenss/fit_clusters_4$totss
# Shows the strength of the iteration is 1.88 and 65% variation has been thrown outside within each of the clusters

# construct a data frame with all cluster iteration measures for comparison
clusterIterations <- data.frame(1, 4, fit_clusters_4$betweenss/fit_clusters_4$tot.withinss, fit_clusters_4$betweenss/fit_clusters_4$totss)
colnames(clusterIterations) <- c("No_Iteration", "No_Clusters", "BetweenSS/WithinSS", "BetweenSS/TotalSS")

# k-means - Iteration 2 - We set clusters of 5

set.seed(1)
fit_clusters_5 <- kmeans(stores.sb, centers = 5, nstart = 1)
fit_clusters_5
# Number of stores in each of the clusters
fit_clusters_5$size
# The cluster averages for each of the clusters
fit_clusters_5$centers
# to measure the strength
fit_clusters_5$betweenss/fit_clusters_5$tot.withinss
fit_clusters_5$betweenss/fit_clusters_5$totss
# Shows the strength of the iteration is 2.16 and 68% variation has been thrown outside from within each of the clusters
clusterIterations <- rbind(clusterIterations, c(2, 5, fit_clusters_5$betweenss/fit_clusters_5$tot.withinss, fit_clusters_5$betweenss/fit_clusters_5$totss))

# k-means - Iteration 3 - We set clusters of 6

set.seed(1)
fit_clusters_6 <- kmeans(stores.sb, centers = 6, nstart = 1)
fit_clusters_6
# Number of stores in each of the clusters
fit_clusters_6$size
# The cluster averages for each of the clusters
fit_clusters_6$centers
# to measure the strength
fit_clusters_6$betweenss/fit_clusters_6$tot.withinss
fit_clusters_6$betweenss/fit_clusters_6$totss
# Shows the strength of the iteration  is 2.74 and 73% variation has been thrown outside from within each of the clusters
clusterIterations <- rbind(clusterIterations, c(3, 6, fit_clusters_6$betweenss/fit_clusters_6$tot.withinss, fit_clusters_6$betweenss/fit_clusters_6$totss))

# k-means - Iteration 4 - We set clusters of 7

set.seed(1)
fit_clusters_7 <- kmeans(stores.sb, centers = 7, nstart = 1)
fit_clusters_7
# Number of stores in each of the clusters
fit_clusters_7$size
# The cluster averages for each of the clusters
fit_clusters_7$centers
# to measure the strength
fit_clusters_7$betweenss/fit_clusters_7$tot.withinss
fit_clusters_7$betweenss/fit_clusters_7$totss
# Shows the strength of the iteration  is 3.06 and 75% variation has been thrown outside within each of the clusters
clusterIterations <- rbind(clusterIterations, c(4, 7, fit_clusters_7$betweenss/fit_clusters_7$tot.withinss, fit_clusters_7$betweenss/fit_clusters_7$totss))

# k-means - Iteration 5 - We set clusters of 8

set.seed(1)
fit_clusters_8 <- kmeans(stores.sb, centers = 8, nstart = 1)
fit_clusters_8
# Number of stores in each of the clusters
fit_clusters_8$size
# The cluster averages for each of the clusters
fit_clusters_8$centers
# to measure the strength
fit_clusters_8$betweenss/fit_clusters_8$tot.withinss
fit_clusters_8$betweenss/fit_clusters_8$totss
# Shows the strength of the iteration  is 3.33 and 77% variation has been thrown outside within each of the clusters
clusterIterations <- rbind(clusterIterations, c(5, 8, fit_clusters_8$betweenss/fit_clusters_8$tot.withinss, fit_clusters_8$betweenss/fit_clusters_8$totss))


# Take a look at ClusterIteration dataset for comparison between various iterations
clusterIterations
# Looking at the above table, we can see variations between 7 and 8 clusters are very minimal in strength and within cluster sum of squares.
# The optimum number of clusters hence chosen to be 8. 

# We consider 8 to be the optimum number of clusters with ratio of betweenSS and WithinSS is 3.2 and 76% variations are outside the inidividual
# clusters

#********************* Clustering Profiling ****************************************************************

# Now we will pass this information back to the original dataset so that we can start doing profiling
stores.final <- data.frame(stores, fit_clusters_8$cluster)
head(stores.final)
# aggregate the cluster means based on the cluster they belong to
cluster_means <- aggregate(stores.final[, 9 : 13], list(stores.final$fit_clusters_8.cluster), mean)
cluster_means
# Dropping the Group.1 variable
cluster_means <- cluster_means[,2 : 6]

# Now for profiling, we would need the mean of the population and that is
population_means <- colMeans(stores.final[,9:13])
population_means
# Now if we have to compare the means of individual clusters with the means of the population, we can add the population mean data as the last
# row in the cluster_means dataset
cluster_means <- rbind(cluster_means, population_means)

cluster_means
# The first 8 rows are the averages for the 8 clusters and the 9th row is the population data

# Now in business terms if we need to interpret the cluster_mean dataset we say
# The average PerSftSale for the stores in the first cluster is Rs 172 and whereas the population is Rs 210. So the stores in the first cluster
# are doing lower than the general population

# Similarly for cluster 6, the Avg Per SQ Ft sale is way higher than the general population

# if we need to see the details of those stores in cluster 6
nrow(subset.data.frame(stores.final, stores.final$fit_clusters_8.cluster == 7))

# we will now construct an insight table and distinguish clusters based on Avg PerSqftSale and Percentage sales by Category

population_AvgSqftSale <- cluster_means$SalePerSqFt[9]
cluster_MaxSalePerSqFt <- subset.default(cluster_means$SalePerSqFt, cluster_means$SalePerSqFt == max(cluster_means$SalePerSqFt))
clusterNo_MaxSalePerSqFt <- which(cluster_means$SalePerSqFt == max(cluster_means$SalePerSqFt))
clusterSize_MaxSalePerSqFt <- nrow(subset.data.frame(stores.final, stores.final$fit_clusters_8.cluster == clusterNo_MaxSalePerSqFt))
insightsDataset <- data.frame("Max_AvgPerSqftSale",population_AvgSqftSale,cluster_MaxSalePerSqFt,clusterNo_MaxSalePerSqFt,clusterSize_MaxSalePerSqFt, stringsAsFactors = FALSE)
colnames(insightsDataset) <- c("Items", "Population Data", "Cluster Data", "Cluster Number", "Size of the Cluster")

cluster_MinSalePerSqFt <- subset.default(cluster_means$SalePerSqFt, cluster_means$SalePerSqFt == min(cluster_means$SalePerSqFt))
clusterNo_MinSalePerSqFt <- which(cluster_means$SalePerSqFt == min(cluster_means$SalePerSqFt))
clusterSize_MinSalePerSqFt <- nrow(subset.data.frame(stores.final, stores.final$fit_clusters_8.cluster == clusterNo_MinSalePerSqFt))
insightsDataset <- rbind(insightsDataset, c("Min_AvgPerSqftSale",population_AvgSqftSale,cluster_MinSalePerSqFt,clusterNo_MinSalePerSqFt,clusterSize_MinSalePerSqFt))

# Use this and populate the table instanteneously in PPT