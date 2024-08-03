#name:
#number:
# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(factoextra)
library(cluster)
library(stats)
library(data.table)
library(NbClust)  # Add this line to load the NbClust package

# Load the dataset
wine_data <- read_excel("Whitewine_v6.xlsx")

# Selecting the first 11 chemical attributes
wine_data <- select(wine_data, 1:11)

# Data Preprocessing
## Scaling the data
wine_scaled <- scale(wine_data)

# Outlier Detection
outliers <- c()
for (i in 1:11) {
  quantile1 <- quantile(wine_scaled[, i], probs = 0.25)
  quantile3 <- quantile(wine_scaled[, i], probs = 0.75)
  iqr <- quantile3 - quantile1
  bottom_outlier_rows <- which(wine_scaled[, i] < quantile1 - 1.5 * iqr)
  top_outlier_rows <- which(wine_scaled[, i] > quantile3 + 1.5 * iqr)
  outliers <- c(outliers, top_outlier_rows, bottom_outlier_rows)
}
outliers <- unique(outliers)
wine_cleaned <- wine_scaled[-outliers, ]

# PCA Analysis
pca_result <- prcomp(wine_cleaned, center = TRUE, scale. = TRUE)

# Cumulative Variance
cum_var <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100

# Select PCs with cumulative variance > 85%
pca_selected <- pca_result$x[, cum_var <= 85]

# Determining the number of clusters using silhouette method
silhouette_plot <- fviz_nbclust(pca_selected, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method for Optimal Clusters",
       subtitle = "Silhouette analysis used to determine the optimal number of clusters")
print(silhouette_plot)

# Elbow Method for Optimal Clusters
total_within_ss <- function(k, data) {
  kmeans_model <- kmeans(data, centers = k, nstart = 25)
  return(kmeans_model$tot.withinss)
}

# Compute total within-cluster sum of squares for different values of k
k_values <- 1:10  # Try different values of k
wss_values <- sapply(k_values, function(k) total_within_ss(k, pca_selected))

# Plotting the Elbow Method
elbow_plot <- ggplot(data = data.frame(k = k_values, WSS = wss_values), aes(x = k, y = WSS)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow Method for Optimal Clusters",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Sum of Squares (WSS)") +
  theme_minimal()
print(elbow_plot)

# Assuming optimal number of clusters from visual inspection or prior analysis
optimal_clusters <- 3  # Example, adjust based on actual analysis

# Clustering on PCA-reduced dataset
set.seed(123)
pca_kmeans_result <- kmeans(pca_selected, centers = optimal_clusters, nstart = 25)
print(pca_kmeans_result$centers)

# Enhanced cluster visualization to avoid overlap on PCA-reduced data
cluster_plot_pca <- fviz_cluster(pca_kmeans_result, data = pca_selected, stand = FALSE,
                                 geom = "point", pointsize = 2, alpha.point = 0.8) +
  geom_text(aes(label = 1:nrow(pca_selected), color = factor(pca_kmeans_result$cluster)), 
            check_overlap = TRUE, vjust = -0.5, hjust = -0.5) +
  labs(title = "Cluster Visualization on PCA-Reduced Data",
       subtitle = "K-means clustering on PCA-reduced dataset") +
  theme_minimal()
print(cluster_plot_pca)

# Silhouette plot for PCA-based clustering
pca_silhouette_scores <- silhouette(pca_kmeans_result$cluster, dist(pca_selected))
silhouette_scores_plot <- fviz_silhouette(pca_silhouette_scores) +
  labs(title = "Silhouette Plot for PCA-Based Clustering",
       subtitle = "Silhouette width scores for each cluster")
print(silhouette_scores_plot)

# Clustering directly on the scaled data without PCA
kmeans_result_scaled <- kmeans(wine_cleaned, centers = optimal_clusters, nstart = 25)

# Enhanced cluster visualization for the scaled data without PCA
cluster_plot_scaled <- fviz_cluster(kmeans_result_scaled, data = wine_cleaned, stand = FALSE,
                                    geom = "point", pointsize = 2, alpha.point = 0.8) +
  geom_text(aes(label = 1:nrow(wine_cleaned), color = factor(kmeans_result_scaled$cluster)), 
            check_overlap = TRUE, vjust = -0.5, hjust = -0.5) +
  labs(title = "Cluster Visualization on Scaled Data",
       subtitle = "K-means clustering directly on the scaled dataset without PCA reduction") +
  theme_minimal()
print(cluster_plot_scaled)

# Additional Plots
# Silhouette plot for PCA-based clustering on the scaled data
silhouette_scores_scaled <- silhouette(kmeans_result_scaled$cluster, dist(wine_cleaned))
silhouette_scores_plot_scaled <- fviz_silhouette(silhouette_scores_scaled) +
  labs(title = "Silhouette Plot for Clustering on Scaled Data",
       subtitle = "Silhouette width scores for each cluster")
print(silhouette_scores_plot_scaled)

# Gap Statistic Plot
gap_stats <- clusGap(wine_cleaned, FUN = kmeans, K.max = 10, B = 100)
gap_stat_plot <- fviz_gap_stat(gap_stats) +
  labs(title = "Gap Statistic Plot for Optimal Clusters",
       subtitle = "Gap statistic analysis used to determine the optimal number of clusters")
print(gap_stat_plot)

# NbClust Plot
nb <- NbClust(wine_cleaned, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
nbclust_plot <- barplot(table(nb$Best.nc), xlab = "Number of Clusters", ylab = "Votes", main = "NbClust Results")
print(nbclust_plot)

# PCA Scree Plot
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100))
