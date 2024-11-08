#### Format Data for ArcGIS Animation ####
library(tidyverse)
# Create separate 2023 and 2022 data, create separate GIS for YBCU and BBCU

# To do: group these by site and include them as an animation

# Data to use
bbcu_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_5-27.csv")
bbcu_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPBYBCU_5-27.csv")
bbcu_21 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2021_AllCollab_topclips_filteredPBYBCU_5-27.csv")

# YBCU
ybcu_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_YBCU_Clips_4-24.csv")
ybcu_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_YBCU_Clips_4-24.csv")
#sum(ybcu_23$ybcu)

# Read in monitoring points
points <- read.csv("./Data/Monitoring_Points/21-23_AllMonitoringPoints_forLiDARExtractions.csv")
points <- points %>% separate(alt_point_id, into = c("point","point_type"), sep = "_", remove = FALSE)

# Troubleshooting
#setdiff(points$point_id, points$point) # No lack of overlap here

# Format the data for animation #####
# Columns:
  # Point_ID (or site ID?)
  # Occupancy 
  # Date
  # Active - 1 or 5?
  # add on the coordinates from monitoring points 

# # Start with 2023 first 
dat_23 <- bbcu_23 %>% group_by(point_id, date) %>% summarize(bbcu = max(annotation))
points_23 <- points %>% filter(point_type == "23") %>% select(point_id, long, lat)
dat_23 <- left_join(dat_23, points_23, by = "point_id")
#na_test <- dat_23[is.na(dat_23$long) | is.na(dat_23$lat), ]
dat_23$date <- as.character(dat_23$date)
dat_23$date <- as.Date(dat_23$date, format = "%Y%m%d")
#
#na_test <- dat_23[is.na(dat_23$long) | is.na(dat_23$lat), ]
# # Define the date range
start_date <- as.Date("2023-06-01")
end_date <- as.Date("2023-08-15")
date_range <- seq(start_date, end_date, by = "day")
#
# # Get unique Point IDs
point_ids <- unique(dat_23$point_id)

# # Create a data frame with all combinations of Point_ID and Time (date range)
expanded_df <- expand.grid(point_id = point_ids, date = date_range)
#
# # Left join expanded_df with the original data to retain all dates for each point and fill missing dates with "inactive".
full_df <- expanded_df %>% left_join(points_23, by = "point_id")
full_df <- full_df %>%
 left_join(dat_23, by = c("point_id", "date")) %>%
 mutate(Active = ifelse(is.na(bbcu), "inactive", "active")) %>% mutate(bbcu = ifelse(is.na(bbcu),0,bbcu))
#na_test <- full_df[is.na(full_df$long) | is.na(full_df$lat), ]
non_df <- anti_join(expanded_df, dat_23, by = c("point_id","date"))
test_join <- left_join(dat_23, points_23, by = "point_id")
#na_test <- test_join[is.na(test_join$long.x) | is.na(test_join$lat.x), ]
unique(na_test$point_id)

# # sort to manually check
full_df1 <- full_df %>% arrange(point_id, date)

t1 <- full_df1[is.na(full_df1$long)==TRUE,]
unique(t1$point_id) # not an issue with the actual function

# Create an active column in this data
#write.csv(full_df,"./Data/Detections_OverTime_ForArcGIS/Data_2023_test2.csv", row.names = FALSE)




#### Make this a function for BBCU ####
# Inputs:
# Dataframe: cleaned clips output from classifier
# year_small: character of last two values of the year ex "23"
# year: full character of year ex "2023" (yeah the data is formatted weird)
format_bbcu_forarc <- function(dataframe, year_small, year) {
  # Filter and summarize the input dataframe based on the specified year
  dat <- dataframe %>%
    group_by(point_id, date) %>%
    summarize(bbcu = max(annotation))
  
  # Filter points data for the specified point type
  points_filtered <- points %>%
    filter(point_type == year_small) %>%
    select(point_id, long, lat)
  points_filtered$long <- as.numeric(points_filtered$long)
  points_filtered$lat <- as.numeric(points_filtered$lat)
  
  # Convert date to Date format
  dat$date <- as.character(dat$date)
  dat$date <- as.Date(dat$date, format = "%Y%m%d")
  
  # Define the date range for the specified year
  start_date <- as.Date(paste0(year, "-06-01"))
  end_date <- as.Date(paste0(year, "-08-15"))
  date_range <- seq(start_date, end_date, by = "day")
  
  # Get unique Point IDs
  point_ids <- unique(dat$point_id)
  
  # Create a data frame with all combinations of Point_ID and Time (date range)
  expanded_df <- expand.grid(point_id = point_ids, date = date_range)
  
  # Left join expanded_df with the original data to retain all dates for each point
  # and fill missing dates with "inactive"
  full_df <- expanded_df %>% left_join(points_filtered, by = "point_id")
  full_df <- full_df %>%
    left_join(dat, by = c("point_id", "date")) %>%
    mutate(Active = ifelse(is.na(bbcu), "inactive", "active"),
           bbcu = ifelse(is.na(bbcu), 0, bbcu))
  
  # Sort by point_id and date
  full_df <- full_df %>% arrange(point_id, date)
  
  return(full_df)
}


# Run on 2023 data
full_dat_23 <- format_bbcu_forarc(bbcu_23, "23", "2023")
write.csv(full_dat_23,"./Data/Detections_OverTime_ForArcGIS/Data_2023_BBCUDaily_Try2.csv", row.names = FALSE)

# Run on 2022 data
full_dat_22 <- format_bbcu_forarc(bbcu_22, "22", "2022")
write.csv(full_dat_22,"./Data/Detections_OverTime_ForArcGIS/Data_2022_BBCUDaily.csv", row.names = FALSE)

# Run on 2021 data
full_dat_21 <- format_bbcu_forarc(bbcu_21, "21", "2021")
write.csv(full_dat_21,"./Data/Detections_OverTime_ForArcGIS/Data_2021_BBCUDaily.csv", row.names = FALSE)

# Check for NAs
# t23 <- full_dat_23[is.na(full_dat_23$long)==TRUE,]
# unique(t23$point_id)
# t22 <- full_dat_22[is.na(full_dat_22$long)==TRUE,]
# unique(t22$point_id)
# t21 <- full_dat_21[is.na(full_dat_21$long)==TRUE,]
# unique(t21$point_id)

#### Make this a function for YBCU ####
# Inputs:
# Dataframe: cleaned clips output from classifier
# year_small: character of last two values of the year ex "23"
# year: full character of year ex "2023" (yeah the data is formatted weird)

format_ybcu_forarc <- function(dataframe, year_small, year) {
  # Filter and summarize the input dataframe based on the specified year
  dat <- dataframe %>%
    group_by(point_id, date) %>%
    summarize(ybcu = max(ybcu))
  
  # Filter points data for the specified point type
  points_filtered <- points %>%
    filter(point_type == year_small) %>%
    select(point_id, long, lat)
  points_filtered$long <- as.numeric(points_filtered$long)
  points_filtered$lat <- as.numeric(points_filtered$lat)
  
  # Convert date to Date format
  dat$date <- as.character(dat$date)
  dat$date <- as.Date(dat$date, format = "%Y%m%d")
  
  # Define the date range for the specified year
  start_date <- as.Date(paste0(year, "-06-01"))
  end_date <- as.Date(paste0(year, "-08-15"))
  date_range <- seq(start_date, end_date, by = "day")
  
  # Get unique Point IDs
  point_ids <- unique(dat$point_id)
  
  # Create a data frame with all combinations of Point_ID and Time (date range)
  expanded_df <- expand.grid(point_id = point_ids, date = date_range)
  
  # Left join expanded_df with the original data to retain all dates for each point
  # and fill missing dates with "inactive"
  full_df <- expanded_df %>% left_join(points_filtered, by = "point_id")
  full_df <- full_df %>%
    left_join(dat, by = c("point_id", "date")) %>%
    mutate(Active = ifelse(is.na(ybcu), "inactive", "active"),
           bbcu = ifelse(is.na(ybcu), 0, ybcu))
  
  # Sort by point_id and date
  full_df <- full_df %>% arrange(point_id, date)
  
  return(full_df)
}

# Run on 2023 YBCU data 
ybcu_dat_23 <- format_ybcu_forarc(ybcu_23, "23", "2023")
write.csv(ybcu_dat_23,"./Data/Detections_OverTime_ForArcGIS/Data_2023_YBCUDaily.csv", row.names = FALSE)

# Go through and do the same for the YBCU data 
ybcu_dat_22 <- format_ybcu_forarc(ybcu_22, "22", "2022")
write.csv(ybcu_dat_22,"./Data/Detections_OverTime_ForArcGIS/Data_2022_YBCUDaily.csv", row.names = FALSE)


# Check for NAs
# y23 <- ybcu_dat_23[is.na(ybcu_dat_23$long)==TRUE,]
# unique(y23$point_id)
