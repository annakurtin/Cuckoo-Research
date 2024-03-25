####### Clean 2022 Playback Data ########


# A script to read in playback data from 2021 and export a clean file for use in analysis and visualization
# First formatting them for use in ARCGIS - summarizing cuckoo detection for each site 
# Next formatting them for us in modeling (later)

# Need to account for variety in points at each site, need to account for same site conducted at different days

# Created 11/29/2023

# Last updated 3/25/2023


#### Setup ###############
packages <- c("stringr","tidyverse","janitor","lubridate")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)
# Load custom cleaning functions
source("./R_Scripts/6_Function_Scripts/Create_Functions_For_Cleaning_Playback_Data.R")

#### Old: Initial Cleaning Code ######

pb_21 <- read.csv("./Data/Playback_Results/2021/Raw_Data/2021_BBCUPlaybackResults_UMBEL_AKEdits_1129.csv")
# Create columns for site_id and survey_id
pb_21_dat <- separate_site_make_survey_id(pb_21)
# replace NA in the playback_detection column with "no_data"
#pb_21_dat <- pb_21_dat %>% mutate(playback_detection = ifelse(is.na(playback_detection),"no_data",playback_detection))

# Summarize by site
pb_21_summed <- pb_21_dat %>% group_by(site_id) %>% summarize(lat_avg = mean(lat, na.rm = TRUE), long_avg = mean(long, na.rm = TRUE),detection_hist_bbcu = max(playback_detection, na.rm = TRUE)) 
# replace -Inf with no_data
#pb_21_summed %>% str_replace(-Inf, "no_data")
# NEED TO FIX THIS LATER

# write to .csv for ArcGIS
#write.csv(pb_21_summed,"./Data/Playback_Results/2021/Outputs/2021_PlaybackResultsSummarized_11-29.csv", row.names = FALSE)


#### Secondary Playback Data Cleaning ######
pb_21 <- read.csv("./Data/Playback_Results/2021/Raw_Data/2021_BBCUPlaybackSessionResults_UMBEL.csv") %>% clean_names()
# didn't manually go through and add in M1-M5 intervals
# change the way the point_id is formatted
pb_21$point_id <- str_replace(pb_21$point_id, "_","-")
# Add in columns for number of observers and binary cuckoo detections
pb_21 <- pb_21 %>% mutate(num_obs = 1) %>% create_binary_cuckoo() %>% make_date_format() %>% separate_site_make_survey_id()
# Rename the columns
pb_21 <- pb_21 %>% rename(observer = obs, 
                          start_time = time,
                          cluster = cluster_sz,
                          bearing = cuckoo_bearing, 
                          notes = bird_notes)
# Create columns for visual and call instead of how
pb_21 <- pb_21 %>% mutate(visual = case_when(how == "V" ~ "Y",
                                                   how == "C" ~ "N",
                                                   how == "S"~ "N",
                                                   how == NA ~ NA),
                                call = case_when(how == "V" ~ "N",
                                                 how == "C" ~ "Y",
                                                 how == "S" ~ "Y",
                                                 how == NA ~ NA))
pb_21 <- pb_21 %>% mutate(across(c(distance, bearing, visual, call, cluster), ~ifelse(. == "", NA, .)))
pb_21_dat <- pb_21 %>% select_final_pbdata_cols()

# Write this to a csv
#write.csv(pb_21_dat, "./Data/Playback_Results/2021/Outputs/2021_PlaybackData_3-25-24.csv",row.names = FALSE)

# Make metadata
## NEED TO CHECK THIS FOR DUPLICATES, MULTIPLE SURVEYS ________________________________________________
# read in coordinates
lt_pt <- read.csv("./Data/Monitoring_Points/UMBEL_LongTermSites.csv") %>% clean_names()
lt_pt <- lt_pt %>% rename(point_id = gps_id, lat = lat_wgs84, long = long_wgs84) %>% select(point_id,lat,long)
lt_pt$point_id <- str_replace(lt_pt$point_id, "_","-") 
nam_pt <- read.csv("./Data/Monitoring_Points/UMBEL_LetterNamedPoints2022.csv")  %>% clean_names() 
nam_pt <- nam_pt %>% rename(point_id = gps_id) %>% select(point_id, lat, long)

pb_21_metad <- pb_21 %>% mutate(sky_start = "no_data",
                                    sky_end = "no_data",
                                    wind_start = "no_data",
                                    wind_end = "no_data",
                                    temp_start = "no_data",
                                    temp_end = "no_data")

# Initial combo to coordinates
pb_21_init <- left_join(pb_21_metad,lt_pt, by = "point_id")
# pull out of nam_pt those that weren't already assigned a coordinate
test <- pb_21_init %>% select(point_id, lat,long)
missing <- test %>% filter(is.na(lat) == TRUE)
points <- missing$point_id
nam_pts <- nam_pt %>% filter(point_id %in% points)
# Create the full list of coordinates to join with the data
coords <- rbind(nam_pts,lt_pt)
# Combine metadata with coordinates
pb_21_metad_coord1 <- left_join(pb_21_metad,coords, by = "point_id")


pb_21_metad_coord <- pb_21_metad_coord1 %>% group_by(survey_id,
                                       date) %>% 
  summarize(survey_site_start_time = min(start_time), 
            num_obs = max(num_obs),
            num_points = length(unique(point_id)),
            sky_start = paste(unique(sky_start)),
            sky_end = paste(unique(sky_end)),
            wind_start = paste(unique(wind_start)),
            wind_end = paste(unique(wind_end)),
            temp_start = paste(unique(temp_start)),
            temp_end = paste(unique(temp_end)),
            lat_avg = mean(lat),
            long_avg = mean(long),
            detection_hist_bbcu = max(bbcu_detection, na.rm = TRUE),
            detection_hist_ybcu = max(ybcu_detection, na.rm = TRUE),
            observer = paste(unique(observer), collapse = ", "),
            notes = paste(notes, collapse = ""))
pb_21_metad_fin <- pb_21_metad_coord %>% select_final_metad_cols()
# write this to a csv file
#write.csv(pb_21_metad_fin, "./Data/Playback_Results/2021/Outputs/2021_PlaybackSurveyMetadataWDetectionsCoords_3-25-24.csv", row.names = FALSE)


# Make a sheet for ArcGIS that has all of the survey locations and detections
site_detections_21 <- pb_21_metad_coord1 %>% separate(survey_id, into = c("site","survey_num"), sep = "#")
site_detections_21 <- site_detections_21 %>% group_by(site) %>% summarize(bbcu_detected = max(bbcu_detection),ybcu_detected = max(ybcu_detection), lat_avg = mean(lat), long_avg = mean(long)) %>% select(site,bbcu_detected,ybcu_detected,lat_avg,long_avg)
# Write this to a .csv file
#write.csv(site_detections_21, "./Data/Playback_Results/2021/Outputs/2021_PlaybackSiteLocationsAvgWithDetections_3-25-24.csv", row.names = FALSE)


# Make a datasheet for all the locations
site_locs_all_21 <- left_join(pb_21_metad,coords, by = "point_id")
site_locs_all_21 <- site_locs_all_21 %>% group_by(point_id) %>% summarize(lat = mean(lat),long = mean(long))
site_locs_all_21 <- site_locs_all_21 %>% separate(point_id, into = c("site","id"), remove = FALSE) 
# Write this to a csv
write.csv(site_locs_all_21, "./Data/Playback_Results/2021/Outputs/2021_PlaybackSiteLocations_All_3-25-2023.csv",row.names = FALSE)
