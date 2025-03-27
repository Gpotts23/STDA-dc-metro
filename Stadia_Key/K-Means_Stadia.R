#K-Means
#This file contains script for running the spatial-temporal clustering method, k-means
#DataPrep_Stadia.R MUST be ran before this script.

#-------------------------------------------------------------------------------
#Install packages (if necessary)
install.packages(c("dplyr", "tidyr", "ggplot2", "ggmap"))

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)

#-------------------------------------------------------------------------------
#Pivot & normalize

#Ensuring K-means and DBSCAN can be ran interchangeably after initial data prep
boardings <- boardings_cleaned

# Pivot data: stations as rows, dates as columns
boardings_pivot <- boardings %>%
  select(StationName, Date, Boardings) %>%
  pivot_wider(names_from = Date, values_from = Boardings, values_fill = list(Boardings = 0)) %>%
  as.data.frame() %>%
  tibble::column_to_rownames(var = "StationName")

# Normalize boardings per day
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
boardings_pivot <- as.data.frame(lapply(boardings_pivot, normalize))

#-------------------------------------------------------------------------------
#Determining the optimal k using the elbow method
compute_wss <- function(data, max_k = 10) {
  wss <- numeric(max_k)
  for (k in 1:max_k) {
    kmeans_result <- kmeans(data, centers = k, nstart = 25)
    wss[k] <- kmeans_result$tot.withinss
  }
  return(wss)
}

# Elbow method plot
max_k <- 10
wss_values <- compute_wss(boardings_pivot, max_k)
plot(1:max_k, wss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)", ylab = "WSS",
     main = "Optimal Number of Clusters")
abline(v = 4, col = "red", lty = 2)

#-------------------------------------------------------------------------------
#Applying k-means

#Set seed for reproducibility
set.seed(123)
k <- 4
kmeans_result <- kmeans(boardings_pivot, centers = k, nstart = 25)

# Attach clusters to station coordinates
boardings_clustered <- boardings %>%
  select(StationName, Lat, Lon) %>%
  distinct(StationName, .keep_all = TRUE) %>%
  mutate(Cluster = kmeans_result$cluster)

# Merge clusters with original boardings
boardings_long <- boardings %>%
  left_join(boardings_clustered, by = "StationName")

#-------------------------------------------------------------------------------
#Plotting temporal trends

# Color scheme for clusters
cluster_colors <- c(
  "1" = "#1f77b4",
  "2" = "#ff7f0e",
  "3" = "#2ca02c",
  "4" = "#d62728"
)

# Time series plot
ggplot(boardings_long, aes(x = Date, y = Boardings, group = StationName, color = factor(Cluster))) +
  geom_line(alpha = 1) +
  scale_color_manual(values = cluster_colors) +
  labs(title = "Metro Station Ridership Trends by Cluster",
       x = "Date", y = "Daily Boardings", color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "bottom")

#-------------------------------------------------------------------------------
# Plotting the stations, symbolized into their cluster

# Define bounding box
dc_bbox <- c(left = -77.50, bottom = 38.70, right = -76.80, top = 39.15)

# ENTER YOUR PERSONAL STADIA MAPS API KEY
register_stadiamaps(key = "<INSERT YOUR STADIA MAPS API KEY HERE")

# Get basemap
dc_basemap <- get_stadiamap(bbox = dc_bbox, zoom = 11, maptype = "stamen_toner_lite")

# Plot stations
plot_metro_avg_clusters <- function(data, title) {
  ggmap(dc_basemap) +
    geom_point(data = data, aes(x = Lon, y = Lat, color = factor(Cluster)), size = 3) +
    scale_color_manual(values = cluster_colors) +
    labs(title = title, color = "Cluster") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

avg_cluster_map <- plot_metro_avg_clusters(boardings_clustered, "K-means Assigned Station Clusters in 2024")
print(avg_cluster_map)

