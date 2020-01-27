# Library imports
library(stats)
library(cluster)
library(clusteval)
library(ggplot2)

# 1. Importing the dataset
# Remember to set the working directory first
dataset <- read.csv("wine.csv")

# 2. Normalize the attributes. Explain your approach.
# dataset.n <- subset(dataset, select= -Wine)
dataset.n <- scale(dataset[-1])

# 3. Perform K-means clustering for k = 4. Report the silhouette score of the resulting clustering

clustered_data <- kmeans(dataset.n, 4, iter.max=300, nstart=10)
clusplot(dataset.n, clustered_data$cluster, lines=0, shade=TRUE, color=TRUE, plotchar=FALSE,
         main=paste("Clusters of wine"))
# get the dissimilarity matrix from the clustered data
dis <- dist(dataset)^2
plot(silhouette(clustered_data$cluster, dis))

# 4. Try out several pairs of attributes and produce scatter plots of the clustering from task 3
# for these pairs of attributes. By inspecting these plots, determine a pair of attributes for 
# which the clusters are relatively well-separated and submit the corresponding scatter plot.

data_subset <- subset(dataset.n, select=c("Malic.acid", "Proline"))
clustered_data <- kmeans(data_subset, 4, iter.max=50, nstart=10)
clusplot(data_subset, clustered_data$cluster, lines=0, shade=TRUE, color=TRUE, plotchar=FALSE,
         main=paste("Clusters of Malic acid vs. Proline"))

# 5. Find the best number of clusters for K-means clustering, based on the silhouette score. 
# Report the best number of clusters and the silhouette score for the corresponding clustering. 
# How strong is the discovered cluster structure?

plotsilhouettes <- function(c, data) {
  clusters <- kmeans(data, c, iter.max=50, nstart=10)
  dis <- dist(data)^2
  plot(silhouette(clusters$cluster, dis))
}

#silhouettes for pair of selected attributes
for(i in 2:10) {
  plotsilhouettes(i, data_subset)
}

# 6.Perform hierarchical cluster analysis on the dataset using the algorithms complete linkage, 
# average linkage and single linkage. Plot the dendrograms resulting from the different methods. 
# Discuss the commonalities and differences between the three dendrograms and try to explain the
# reasons leading to the differences.


# Single linkage
single_link <- hclust(dist(dataset.n), method="single")
plot(single_link, main=paste("Single Linkage"))

# Complete linkage
complete_link <- hclust(dist(dataset.n), method="complete")
plot(complete_link, main=paste("Complete Linkage"))

# Average linkage
average_link <- hclust(dist(data_subset), method="average")
plot(average_link, main=paste("Average Linkage"))

# 7. Cut all of the three dendrograms from task 6 to obtain a flat clustering with the number of
# clusters determined as the best number in task 5.

# Single Link cut
single_cut <- cutree(single_link, 3)
clusplot(dataset.n, single_cut, lines=0, shade=TRUE, color=TRUE, labels=0, plotchar=FALSE,
         span=TRUE, main=paste("Single Link Clusters"))

# Complete Link cut
complete_cut <- cutree(complete_link, 3)
clusplot(dataset.n, complete_cut, lines=0, shade=TRUE, color=TRUE, labels=0, plotchar=FALSE,
         span=TRUE, main=paste("Complete Link Clusters"))

# Average Link cut
average_cut <- cutree(average_link, 3)
clusplot(dataset.n, average_cut, lines=0, shade=TRUE, color=TRUE, labels=0, plotchar=FALSE,
         span=TRUE, main=paste("Average Link Clusters"))

# 8. To perform an external validation of the clustering results, use the class label (attribute Wine).
# What is the Rand Index for the best K-means clustering? What are the values of the Rand Index for
# the flat clusterings obtained in task 7 from complete linkage, average linkage and single linkage?
# Discuss the results.

label <- as.numeric(dataset$Wine)
kmeans_rand <- cluster_similarity(label, clustered_data$cluster, similarity="rand", method="independence")
single_rand <- cluster_similarity(label, single_cut, similarity="rand", method="independence")
average_rand <- cluster_similarity(label, average_cut, similarity="rand", method="independence")
complete_rand <- cluster_similarity(label, complete_cut, similarity="rand", method="independence")




