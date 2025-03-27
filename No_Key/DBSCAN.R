#DBSCAN
#This file contains script for running the spatial-temporal clustering method, DBSCAN
#DataPrep.R MUST be ran before this script.

#-------------------------------------------------------------------------------
#Install packages (if necessary)
install.packages(c("dbscan", "dplyr", "ggplot2", "ggmap", "tidyr"))

# Load libraries
library(dbscan)
library(dplyr)
library(ggplot2)
library(ggmap)
library(tidyr) 

#-------------------------------------------------------------------------------
#Data preparation

#Ensuring K-means and DBSCAN can be ran interchangeably after initial data prep
boardings <- boardings_cleaned

# Convert date to numeric format (1-366)
boardings <- boardings %>%
  mutate(DateNumeric = as.numeric(as.Date(Date) - as.Date("2024-01-01")) + 1)

# Normalize boardings for dbscan
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

boardings <- boardings %>%
  mutate(NormalizedBoardings = normalize(Boardings))

#-------------------------------------------------------------------------------
#DBSCAN Parameters

# Determine minPts to define minimum number of data entries per cluster
minPts <- 12

# Scale date for spatial-temporal input
boardings <- boardings %>%
  mutate(DateScaled = scale(DateNumeric))

#Compute kNN distance
knn_distances <- kNNdist(boardings[, c("Lon", "Lat", "DateScaled")], k = minPts)

# Plot kNN distance to find optimal S-Eps 
plot(sort(knn_distances), type = "l", 
     main = paste("kNN Distance Plot (minPts =", minPts, ")"),
     xlab = "Sorted Points", ylab = "kNN Distance")
abline(h = median(knn_distances), col = "red", lty = 2)

#-------------------------------------------------------------------------------
#Apply DBSCAN

# Spatial threshold determined from kNN
S_Eps <- 0.025  

stdbscan_result <- dbscan(boardings[, c("Lon", "Lat", "DateScaled")], eps = S_Eps, minPts)
boardings$ST_DBSCAN_Cluster <- stdbscan_result$cluster

#-------------------------------------------------------------------------------
#Identify most frequent cluster per station & prep for plotting

#Identifying most frequent cluster per station
most_frequent_clusters <- boardings %>%
  group_by(StationName, ST_DBSCAN_Cluster) %>%
  tally() %>%
  arrange(StationName, desc(n)) %>%
  slice_head(n = 1) %>%
  select(StationName, ST_DBSCAN_Cluster)

# Merging with spatial data
boardings_avg_stdbscan <- boardings %>%
  select(StationName, Lat, Lon) %>%
  distinct(StationName, .keep_all = TRUE) %>%
  left_join(most_frequent_clusters, by = "StationName")

# Color scheme for clusters
cluster_colors <- c(
    "0" = "black",  
    "1" = "#1f77b4",
    "2" = "#ff7f0e",
    "3" = "#2ca02c",
    "4" = "#d62728"
  )

# Labels for clusters
cluster_labels <- c(
  "0" = "Noise", 
  "1" = "Cluster 1", 
  "2" = "Cluster 2", 
  "3" = "Cluster 3", 
  "4" = "Cluster 4"
)

#-------------------------------------------------------------------------------
# Plot DBSCAN cluster map (No Basemap)

# Plot most frequent cluster per station
plot_stdbscan_clusters <- function(data, title) {
  ggplot(data, aes(x = Lon, y = Lat, color = factor(ST_DBSCAN_Cluster))) +
    geom_point(size = 3) +
    scale_color_manual(values = cluster_colors, labels = cluster_labels, drop = FALSE) +
    labs(title = title, x = "Longitude", y = "Latitude", color = "Cluster") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Convert NA clusters to noise (0)
boardings_avg_stdbscan$ST_DBSCAN_Cluster[is.na(boardings_avg_stdbscan$ST_DBSCAN_Cluster)] <- 0
boardings_avg_stdbscan$ST_DBSCAN_Cluster <- factor(boardings_avg_stdbscan$ST_DBSCAN_Cluster, levels = c(0, 1, 2, 3, 4))

# Generate & print the map
avg_stdbscan_map <- plot_stdbscan_clusters(boardings_avg_stdbscan, "Most Frequent DBSCAN Cluster per Station in 2024")
print(avg_stdbscan_map)

#-------------------------------------------------------------------------------
#Time series plot to identify temporal ridership trends
ggplot(boardings, aes(x = Date, y = Boardings, group = StationName, color = factor(ST_DBSCAN_Cluster))) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = cluster_colors, labels = cluster_labels, drop = FALSE) +
  theme_minimal() +
  labs(title = "Metro Station Ridership Trends by DBSCAN Cluster", 
       x = "Date", y = "Daily Boardings", color = "Cluster") +
  theme(legend.position = "bottom")

