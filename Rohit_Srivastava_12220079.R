
# Step 1

data <- read.csv('C:\\Users\\Rohit Srivastava\\OneDrive\\Desktop\\ISB\\2nd Term\\Assignments\\ML UL\\wine-data.csv', header = TRUE)

mydata <- data[1:178, 2:14]

# Checking for missing or non-finite values in mydata
missing_values <- sum(is.na(mydata))
non_finite_values <- sum(!is.finite(as.matrix(mydata)))

if (missing_values > 0 || non_finite_values > 0) {
  # Handling missing or non-finite values
  clean_data <- mydata[complete.cases(mydata),]
} else {
  clean_data <- mydata
}

# Step 2

# Performing principal component analysis
wine_pca <- princomp(clean_data, cor = TRUE, scores = TRUE, covmat = NULL)

summary(wine_pca)
plot(wine_pca)

# Step 3

normalized_data <- scale(clean_data)

# Elbow method
Cluster_Variability <- matrix(nrow=8, ncol=1)
for (i in 1:8) {
  Cluster_Variability[i] <- kmeans(normalized_data, centers=i)$tot.withinss
}

# Plot the elbow curve
plot(1:8, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares")


# Suppressing the warning message when loading the 'fpc' package
suppressWarnings(library("fpc"))

# Performing k-means clustering on the cleaned data using all chemical measurements
clust_all <- kmeans(normalized_data, centers=3, iter.max=10)
fit <- clust_all  # Assigning the kmeans object to 'fit'

# Clustering analysis using all chemical measurements
Cluster_Variability_all <- matrix(nrow=8, ncol=1)
for (i in 1:8) {
  Cluster_Variability_all[i] <- kmeans(normalized_data, centers=i)$tot.withinss
}

# Performing k-means clustering on the two most significant PC scores
pc_scores <- wine_pca$scores[, 1:2]
clust_pca <- kmeans(pc_scores, centers=3, iter.max=10)

# Clustering analysis using two most significant PC scores
Cluster_Variability_pca <- matrix(nrow=8, ncol=1)
for (i in 1:8) {
  Cluster_Variability_pca[i] <- kmeans(pc_scores, centers=i)$tot.withinss
}

# Loading the 'MASS' package for the parcoord function
library(MASS)

# Cluster Centers for All Chemical Measurements
cluster_centers_all <- t(fit$centers)

# Printing cluster centers for all chemical measurements
print(cluster_centers_all)

# Visualizing cluster centers using parallel coordinates plot (All Chemical Measurements)
parcoord(cluster_centers_all, col=c('red', 'green', 'blue'))

# Visualizing cluster assignments using all chemical measurements
plotcluster(normalized_data, clust_all$cluster, main="Clustering Results - All Chemical Measurements")

# Visualizing cluster centers using parallel coordinates plot (PC scores)
parcoord(clust_pca$centers, col=c('red', 'green', 'blue'))

# Visualizing cluster assignments using two most significant PC scores
plotcluster(pc_scores, clust_pca$cluster, main="Clustering Results - PC Scores")

# Additional code for suggesting subsets
# Step 1: Read and preprocess the data (same as before)

# Step 2: Performing principal component analysis
wine_pca <- princomp(clean_data, cor=TRUE, scores=TRUE, covmat=NULL)

# Extracting the loadings from PCA
loadings <- wine_pca$loadings

# Specifying the threshold for variable importance
threshold <- 0.2

# Selecting the chemical measurements with higher absolute loadings
subset_measurements <- colnames(clean_data)[abs(loadings[, 1]) > threshold]

# Performing k-means clustering on the subset of measurements
clust_subset <- kmeans(clean_data[, subset_measurements], centers=3, iter.max=10)

# Analyzing the rest of the measurements that were not included in the clustering process
rest_measurements <- colnames(clean_data)[!(colnames(clean_data) %in% subset_measurements)]
rest_data <- clean_data[, rest_measurements]

# Calculating the mean values of the rest of the measurements for each cluster
rest_means <- aggregate(rest_data, by=list(clust_subset$cluster), FUN=mean)

# Printing the subset of chemical measurements for distinct separation
print(subset_measurements)

# Printing the mean values of the rest of the measurements for each cluster
print(rest_means)

