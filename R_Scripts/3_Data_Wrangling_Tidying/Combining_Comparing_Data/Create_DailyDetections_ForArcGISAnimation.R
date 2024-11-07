#### Format Data for ArcGIS Animation ####
library(tidyverse)
# Create separate 2023 and 2022 data, create separate GIS for YBCU and BBCU


# Data to use
bbcu_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_5-27.csv")
bbcu_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPBYBCU_5-27.csv")
bbcu_21 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2021_AllCollab_topclips_filteredPBYBCU_5-27.csv")

# YBCU
ybcu_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_YBCU_Clips_4-24.csv")
ybcu_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_YBCU_Clips_4-24.csv")


# Read in monitoring points
points <- read.csv("./Data/Monitoring_Points/21-23_AllMonitoringPoints_forLiDARExtractions.csv")
points <- points %>% separate(alt_point_id, into = c("point","point_type"), sep = "_", remove = FALSE)

# Format the data for animation
# Columns:
  # Point_ID (or site ID?)
  # Occupancy 
  # Date
  # Active - 1 or 5?
  # add on the coordinates from monitoring points 

# Start with 2023 first 
dat_23 <- bbcu_23 %>% group_by(point_id, date) %>% summarize(bbcu = max(annotation))
points_23 <- points %>% filter(point_type == "23") %>% select(point_id, long, lat)
dat_23 <- left_join(dat_23, points_23, by = "point_id")
dat_23$date <- as.character(dat_23$date)
dat_23$date <- as.Date(dat_23$date, format = "%Y%m%d")

# Define the date range
start_date <- as.Date("2023-06-01")
end_date <- as.Date("2023-08-15")
date_range <- seq(start_date, end_date, by = "day")

# Get unique Point IDs
point_ids <- unique(dat_23$point_id)

# Create a data frame with all combinations of Point_ID and Time (date range)
expanded_df <- expand.grid(point_id = point_ids, date = date_range)

# Left join expanded_df with the original data to retain all dates for each point and fill missing dates with "inactive".
full_df <- expanded_df %>%
  left_join(dat_23, by = c("point_id", "date")) %>%
  mutate(Active = ifelse(is.na(bbcu), "inactive", "active")) %>% mutate(bbcu = ifelse(is.na(bbcu),0,bbcu))

#full_df$bbcu[is.na(full_df$bbcu)] <- "No Data"

full_df <- full_df %>% arrange(point_id, date)

# Create an active column in this data
#write.csv(full_df,"./Data/Detections_OverTime_ForArcGIS/Data_2023_test2.csv", row.names = FALSE)
