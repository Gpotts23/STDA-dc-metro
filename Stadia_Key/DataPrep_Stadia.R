#Data Preparation
#This file contains script for initial import, cleaning, and analysis plots of the DC boardings data in 2024

#-------------------------------------------------------------------------------
# Load Libraries
library(dplyr)
library(ggplot2)
library(sf)
library(spdep)
library(ggrepel)
library(metro)
library(ggmap)

#-------------------------------------------------------------------------------
# Load metro boardings data from GitHub repository
boardings <- read.csv("Data/boardings_by_station.csv")
boardings$Date <- as.Date(boardings$Date, format = "%Y-%m-%d")
colnames(boardings)[colnames(boardings) == "Station.Name"] <- "StationName"

# Standardization of station names
boardings <- boardings %>%
  mutate(StationName = recode(StationName,
                              "West Falls Church" = "West Falls Church-VT/UVA",
                              "Eisenhower Ave" = "Eisenhower Avenue",
                              "Addison Road" = "Addison Road-Seat Pleasant",
                              "Dunn Loring" = "Dunn Loring-Merrifield",
                              "Southern Ave" = "Southern Avenue",
                              "Virginia Sq-GMU" = "Virginia Square-GMU",
                              "Downtown Largo" = "Largo Town Center",
                              "North Bethesda" = "White Flint",
                              "Vienna" = "Vienna/Fairfax-GMU",
                              "Tysons" = "Tysons Corner",
                              "Archives" = "Archives-Navy Memorial-Penn Quarter",
                              "Woodley Park" = "Woodley Park-Zoo/Adams Morgan",
                              "Mt Vernon Sq" = "Mt Vernon Sq 7th St-Convention Center",
                              "Rhode Island Ave" = "Rhode Island Ave-Brentwood",
                              "McPherson Sq" = "McPherson Square",
                              "U Street" = "U Street/African-Amer Civil War Memorial/Cardozo",
                              "Gallery Place" = "Gallery Pl-Chinatown",
                              "Hyattsville Crossing" = "Prince George's Plaza"
  ))

# Add missing stations with coordinates
missing_stations <- data.frame(
  StationName = c("Ashburn", "Dulles Airport", "Herndon", "Innovation Center", 
                  "Loudoun Gateway", "Reston Town Center", "Potomac Yard"),
  Lat = c(39.0166, 38.9478, 38.9844, 38.9611, 39.0143, 38.9598, 38.8272),
  Lon = c(-77.4875, -77.4481, -77.3896, -77.4270, -77.4601, -77.3590, -77.0515)
)

# Combine with known stations
metro_stations <- bind_rows(metro_stations, missing_stations) %>%
  distinct(StationName, .keep_all = TRUE)

# Merge lat/lon into boardings
boardings <- boardings %>%
  left_join(metro_stations %>% select(StationName, Lat, Lon), by = "StationName") %>%
  select(Date, Day.of.Week, StationName, Entries.Or.Boardings, Lat, Lon) %>%
  rename(
    DayOfWeek = Day.of.Week,
    Boardings = Entries.Or.Boardings
  )

# Final cleaned version of boardings that will be referenced in other scripts
boardings_cleaned <- boardings

#-------------------------------------------------------------------------------
# Initial Map of Metro Stations

# Define bounding box
dc_bbox <- c(left = -77.50, bottom = 38.70, right = -76.80, top = 39.15)

# ENTER YOUR PERSONAL STADIA MAPS API KEY
register_stadiamaps(key = "<INSERT YOUR STADIA MAPS API KEY HERE")

# Get basemap
dc_basemap <- get_stadiamap(bbox = dc_bbox, zoom = 11, maptype = "stamen_toner_lite")

# Plot stations
station_map <- ggmap(dc_basemap) +
  geom_point(data = boardings_cleaned %>% distinct(StationName, Lat, Lon), 
             aes(x = Lon, y = Lat), color = "red3", size = 3, alpha = 0.7) +
  labs(title = "DC Metro Network Stations After Initial Data Preparation", 
       x = "Longitude", y = "Latitude") +
  theme_minimal()

print(station_map)

#-------------------------------------------------------------------------------
# Plot of initial temporal trends

# 3 stations are selected for comparison
selected_stations <- c("Navy Yard-Ballpark", "Ashburn", "College Park-U of Md")

# Filter for these stations
stations_data <- boardings_cleaned %>% filter(StationName %in% selected_stations)

# Color scheme for temporal chart
station_colors <- c("Navy Yard-Ballpark" = "lightgreen",
                    "Ashburn" = "black", 
                    "College Park-U of Md" = "red")

# Plot daily boardings
ggplot(data = stations_data, aes(x = Date, y = Boardings, color = StationName)) +
  geom_line(size = 1) +
  scale_color_manual(name = "Station:", values = station_colors) +
  labs(title = "Comparison of Boardings at Select Stations in 2024",
       x = "Date", y = "Daily Boardings") +
  theme_minimal() +
  theme(legend.position = "bottom")

#-------------------------------------------------------------------------------
#Moran’s I: Spatial Autocorrelation

#Compute average boardings per station
station_avg <- boardings_cleaned %>%
  group_by(StationName, Lon, Lat) %>%
  summarise(AverageBoardings = mean(Boardings), .groups = "drop")

#Convert to sf and adjust projection
station_sf <- st_as_sf(station_avg, coords = c("Lon", "Lat"), crs = 4326) %>%
  st_transform(crs = 26918)  # UTM zone 18N

coords <- st_coordinates(station_sf)

# Neighbor list and spatial weights
nb <- dnearneigh(coords, d1 = 0, d2 = 4000)
listw <- nb2listw(nb, style = "W", zero.policy = TRUE)

#Global Moran's I test & results
moran_result <- moran.test(station_avg$AverageBoardings, listw, zero.policy = TRUE)
print(moran_result)

#-------------------------------------------------------------------------------
# Moran’s I Scatterplot

# Spatial lag
station_avg$SpatialLag <- lag.listw(listw, station_avg$AverageBoardings, zero.policy = TRUE)

# Define means
mean_boardings <- mean(station_avg$AverageBoardings)
mean_lag <- mean(station_avg$SpatialLag)

# Classify quadrant
station_avg$Category <- with(station_avg, ifelse(
  SpatialLag == 0, "No Neighbours",
  ifelse(AverageBoardings >= mean_boardings & SpatialLag >= mean_lag, "High-High",
         ifelse(AverageBoardings < mean_boardings & SpatialLag < mean_lag, "Low-Low",
                ifelse(AverageBoardings >= mean_boardings & SpatialLag < mean_lag, "High-Low", "Low-High")
         )
  )
))

# Creating label for labeled stations
station_avg$Label <- ifelse(
  station_avg$StationName %in% selected_stations,
  station_avg$StationName, "")

# Color scheme for quadrants & stations with no neighbors within 4km
category_colors <- c(
  "High-High" = "#1f77b4", 
  "Low-Low" = "#ff7f0e", 
  "High-Low" = "#d62728", 
  "Low-High" = "#2ca02c", 
  "No Neighbours" = "black"
)

# Moran's I Scatterplot
ggplot(station_avg, aes(x = AverageBoardings, y = SpatialLag)) +
  geom_point(aes(color = Category), size = 3) +
  scale_color_manual(values = category_colors) +
  geom_vline(xintercept = mean_boardings, linetype = "dashed", color = "black") +
  geom_hline(yintercept = mean_lag, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dotted") +
  ggrepel::geom_text_repel(
    aes(label = Label),
    size = 3, nudge_x = -20, nudge_y = 20,
    segment.color = "black", max.overlaps = Inf
  ) +
  labs(
    title = "Moran’s I Scatterplot",
    x = "Average Boardings per Station",
    y = "Spatial Lag",
    color = "Cluster"
  ) +
  theme_minimal() +
  theme(
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom"
  )
