#### Create a Sheet for ArcGIS with all Monitoring Points #####

# this is a script to read in datasheets for the monitoring points across different years and output one master datasheet that has the locations of all the points 
# Created 6/17/2023

# Last updated 6/17/2023

#### Install and load packages #####
packages <- c("tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_SiteColumn_fromPointID.R")
load_packages(packages)

# FOR LATER: Code to create a column for how the site was used:
# veg <- veg %>%
#   mutate(sampling_design = case_when(
#     grepl("^[[:alpha:]]{4}-[[:digit:]]{3}$", point_id) ~ "habitat_grts",
#     grepl("^[[:digit:]]{2,3}-[[:digit:]]{1}$", point_id) ~ "mmr_grts",
#     grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$", point_id) ~ "selectedcu_nonrand",
#     TRUE ~ NA_character_  # Default case if none of the above conditions match
#   ))

#### 2023 Data #######
# Read in the actual locations
aru_23 <- read.csv("./Data/Monitoring_Points/2023_AllARUPoints_FromDeploymentData.csv")
aru_23 <- aru_23  %>% select(point_id,x,y) %>% rename(long = x, lat = y)
aru_23 <- aru_23 %>% mutate(aru = "Y")
# Playback locations
#pb_23 <- read.csv("./Data/Monitoring_Points/2023_PlaybackPoints_FWP_UMBEL.csv")
pb_23 <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackSiteLocations_All_3-22-24.csv")
pb_23 <- pb_23 %>% mutate(playback = "Y")

# Which of these datasheets have different long and lat values?
# Merge the data frames on point_id
merged_df <- aru_23 %>%
  inner_join(pb_23, by = "point_id", suffix = c("_df1", "_df2"))
# Check if they're approximately equal (using != or == will return nearly all of them because of difference in floating point numbers)
all.equal(merged_df$long_df1,merged_df$long_df2)
all.equal(merged_df$lat_df1,merged_df$lat_df2)
# These aren't all the same but mean difference is very very small
merged_df[all.equal(merged_df$lat_df1,merged_df$lat_df2)==FALSE,]
# These all look good

# Combine them into the actual coordinates
joined_23 <- full_join(aru_23,pb_23, by = "point_id")
# Coalesce these into just one lat and long column
actual_23 <- joined_23 %>%
  mutate(lat = coalesce(lat.x, lat.y),
         long = coalesce(long.x, long.y),
         point_loc = "actual",
         alt_point_id = paste0(point_id,"_23")) %>%
  select(point_id, alt_point_id, long, lat, point_loc,aru, playback)
# Leave the old point_id in one dataframe for use later in cleaning the 2022 data
tofill <- joined_23 %>%
  mutate(lat = coalesce(lat.x, lat.y),
         long = coalesce(long.x, long.y),
         point_loc = "actual",
         alt_point_id = point_id) %>%
  select(point_id, alt_point_id, long, lat, point_loc,aru, playback)

# Read in the veg locations
veg_23 <- read.csv("./Data/Vegetation_Data./Outputs/2023_VegSurvey_MainData_Cleaned6-19.csv")
veg_23 <- veg_23 %>% 
  mutate(playback = ifelse(point_id %in% pb_23$point_id, "Y",NA),
  aru = ifelse(point_id %in% aru_23$point_id, "Y",NA))
#test <- veg_23 %>% slice(-15) %>% select(point_id,long, lat) %>% mutate(point_id = paste0(point_id,"_veg")) GO BACK AND FINISH CLEANING THE VEG POINTS
veg_23 <- veg_23 %>% 
  mutate(point_loc = "veg", 
         alt_point_id = paste0(point_id,"_veg")) %>% 
  select(point_id, alt_point_id, lat, long, point_loc, aru, playback)

allpts_23 <- rbind(actual_23, veg_23)

# Need to make an averaged point location for the repeat points
sites_23 <- allpts_23 %>% create_site_col()
sites_23 <- sites_23 %>% filter(grepl("^[[:digit:]]{2,3}-[[:digit:]]{1}$",point_id)|grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$",point_id))
sites_23 <- sites_23 %>% group_by(site_id) %>% summarize(long = mean(long), 
                                             lat = mean(lat),
                                             point_loc = "averaged_site",
                                             aru = first(aru),
                                             playback = first(playback))
sites_23 <- sites_23 %>% rename(point_id = site_id) %>% mutate(alt_point_id = paste0(point_id,"_23avg")) %>% select(point_id, alt_point_id, long, lat, point_loc, aru, playback)

all_23 <- rbind(allpts_23,sites_23)
# write this for use in ArcGIS
#write.csv(all_23, "./Data/Monitoring_Points/2023_AllMonitoringPoints_forLiDARExtractions.csv", row.names = FALSE)



#### 2022 Data ####
pts_22 <- read.csv("./Data/Monitoring_Points/2022_ALLPoints.csv")
pts_22 <- pts_22 %>% mutate(point_loc = "actual",
                            alt_point_id = paste0(point_id,"_22"),
                            aru = "Y",
                            playback = "Y")
# Fill in the missing data
# First pull out the sites from the 2023 data that you could use to fill in
pts_22_miss <- pts_22 %>% filter(is.na(lat)==TRUE | is.na(long)==TRUE) %>% select(point_id)
fill_dat <- tofill %>% filter(point_id %in% pts_22_miss$point_id) %>% select(point_id, long,lat)
# Join the data together
pts_22_full <- left_join(pts_22, fill_dat, by = "point_id")
pts_22_full <- pts_22_full %>% mutate(lat = coalesce(lat.x, lat.y),
                                     long = coalesce(long.x, long.y)) 
pts_22_full <- pts_22_full %>% select(point_id, alt_point_id, lat, long, point_loc, aru, playback)
  
# Make averaged locations
pts_22_avg <- pts_22_full %>% 
  create_site_col() %>%
  group_by(site_id) %>% 
  summarize(long = mean(long),
            lat = mean(lat),
            point_loc = "averaged_site",
            point_id = first(site_id),
            aru = first(aru),
            playback = first(playback)) %>%
  mutate(alt_point_id = paste0(point_id, "_22avg")) %>%
  select(point_id, alt_point_id, lat, long, point_loc, aru, playback)

# Combine them
all_22 <- rbind(pts_22_full,pts_22_avg)


# Figure out how to combine these - could do this in a secondary processing step?
# see if the points are the same or close
merge2 <- inner_join(actual_23,pts_22, by = "point_id")
# Check if they're approximately equal 
all.equal(merge2$long.x,merge2$long.y)
all.equal(merge2$lat.x,merge2$lat.y)
# I manually checked the data to see if there were differences within 3 decimal points. There were in 18 points.
# AME-2, JDO-1, JDO-2, JDO-3, 82-3, 8-1, 104-5, 101-2, 103-3, 102-5, 83-2, 83-3, 203-3, SIP-1, SIP-2, SIP-3, KNB-1, KNB-3
# Going to go back and process this data for 2022 separately 

#### 2021 Data ####
# Same here, go back and process these later 
pts_21 <- read.csv("./Data/Metadata/Outputs/2021_ARUDeployment_Retrieval_Metadata_UMBEL_Cleaned2-21.csv")
pts_21 <- pts_21 %>% 
  select(point_id, long, lat) %>% 
  mutate(point_loc = "actual",
         alt_point_id = paste0(point_id,"_21"),
         aru = "Y",
         playback = "Y")


# Make averaged locations
pts_21_avg <- pts_21 %>% 
  create_site_col() %>%
  group_by(site_id) %>% 
  summarize(long = mean(long),
            lat = mean(lat),
            point_loc = "averaged_site",
            point_id = first(site_id),
            aru = first(aru),
            playback = first(playback)) %>%
  mutate(alt_point_id = paste0(point_id, "_21avg")) %>%
  select(point_id, alt_point_id, lat, long, point_loc, aru, playback)

all_21 <- rbind(pts_21,pts_21_avg)


# COMBINE THEM ALL FOR USE
all_pts <- rbind(all_23,all_22, all_21)
# Write this for use in arcgis
write.csv(all_pts, "./Data/Monitoring_Points/21-23_AllMonitoringPoints_forLiDARExtractions.csv", row.names = FALSE)


### CODE GRAVEYARD #####
# "^[[:digit:]]{2,3}-[[:digit:]]{1}$", point_id) ~ "mmr_grts",
# #     grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$"
# avg_only <- wsite_23 %>%
#   filter(grepl("^[A-Za-z]{3}$", site_id))
# 
# pts_22_avg <- pts_22_full %>% 
#   mutate(playback = ifelse(point_id %in% pb_23$point_id, "Y",NA),
#          aru = ifelse(point_id %in% aru_23$point_id, "Y",NA))

# pbu_23 <- pbu_23 %>% select(point_id, longitude, latitude) %>% rename(long = longitude, lat = latitude)
# # Filter out only the ones that are in the veg points 
# pbf_23 <- read.csv("./Data/Monitoring_Points/2023_PlaybackPoints_FWP.csv")
# pbf_23 <- pbf_23 %>% select(point_id,x,y) %>% rename(long = x, lat = y)


# # Check for discrepancies in long and lat
# discrepancies <- merged_df %>%
#   filter(long_df1 != long_df2 | lat_df1 != lat_df2)
# merged_df[,all.equal(merged_df$lat_df1,merged_df$lat_df2)==FALSE]
# 
# # This is throwing out the ones only in playbacks
# test <- rbind(aru_23, pb_23)
# test <- test %>% distinct(point_id)
# test[distinct(test$point_id),]
