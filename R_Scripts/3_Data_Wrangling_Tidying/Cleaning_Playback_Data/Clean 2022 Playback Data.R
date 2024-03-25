####### Clean 2022 Playback Data ########


# A script to read in playback data from 2022 and export a clean file for use in analysis and visualization
# First formatting them for use in ARCGIS - summarizing cuckoo detection for each site 
# Next formatting them for us in modeling (later)

# STATUS need to run the UMBEL part of the code again with the completed monitoring_points data
# NEED TO ADD ON A COLUMN FOR NUM OBSERVERS FOR DETECTION
# NEED TO GO AND FIX THE TIME COLUMN
# Need to create datasheets that match the 2023 datasheets

# Created 10/6/2023

# Last updated 11/29/2023

#### Setup ###############
packages <- c("stringr","tidyverse","janitor","lubridate")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)
# Load custom cleaning functions
source("./R_Scripts/6_Function_Scripts/Create_Functions_For_Cleaning_Playback_Data.R")


#### Initial Playback Cleaning ############

# Region 7 #################
# Need to create a new script for removing the U and space from the Region 7 playback data and joining it to the right name from the metadata, then writing it to outputs 

# Read in playback data
r7PB_22 <- read.csv("./Data/Playback_Results/2022/Raw_Data/2022_R7_PlaybackSurveyData.csv") %>% clean_names() %>% rename(aru_id = point)
# CHECKPOINT: REGION 7 #####
# sum the number of BBCU and YBCU for each site
check1 <- r7PB_22 %>% group_by(aru_id, species) %>% summarize(count = n())
sum(check1$species == "BBCU") # 3 sites with bbcu
sum(check1$species == "YBCU") # 1 sites with bbcu

### R7 CONT #####
# Change the first row with "NOBI " to remove the space afterwards
r7PB_22[1,8] <- "NOBI"
# changing data entry error based off of the ARU inventory from 2023 - SMM04965 exists, SMM04956 doesn't
r7PB_22[31:40,4] <- "SMM04965"

# create a column for the cuckoo detections
r7PB_22 <- create_binary_cuckoo(r7PB_22)
# Clean the interval column
r7PB_22 <- clean_intervals(r7PB_22)
# Clean the ARU_ID column
r7PB_22 <- clean_arus(r7PB_22)
# Make the date column a date
r7PB_22 <- make_date_format(r7PB_22)

# Read in metadata
r7_metadat <- read_csv("./Data/Metadata/Raw_Data/2022_ARUDeployment_MetadataSpacesRemoved_FWPR7.csv") %>% clean_names()
# rename columns and select only the necessary
r7_metadat <- rename_cols_and_select(r7_metadat)

# join the playback and the metadata
r7_22 <- join_playback_metadata(r7PB_22,r7_metadat)

# separate out the site and make a survey_id column
r7_22 <- separate_site_make_survey_id(r7_22)

# Now check which columns are missing and add them in or rename
r7_22 <- r7_22 %>% rename(bearing = cuckoo_bearing)
r7_22 <- r7_22 %>% rename(cluster = cluster_sz) # not sure what cluster code is
r7_22 <- r7_22 %>% rename(notes = bird_notes) 
r7_22 <- make_unentered_columns(r7_22,"call")

#choose the final columns to include in playback data
r7_22_final <- reorder_final_cols(r7_22)

# CHECKPOINT: REGION 7 #####
# sum the number of BBCU and YBCU for each site
check72 <- r7_22_final %>% group_by(point_id) %>% summarize(count_bbcu = ifelse(sum(bbcu, na.rm = TRUE)>=1,1,0)) 
sum(check72$count_bbcu, na.rm = TRUE) # 3 sites with bbcu
check73 <- r7_22_final %>% group_by(point_id) %>% summarize(count_ybcu = ifelse(sum(ybcu, na.rm = TRUE)>=1,1,0)) 
sum(check73$count_ybcu, na.rm = TRUE) # 1 sites with ybcu
### GOOD TO GO


# WRITE REGION 7 DATA ######
#write.csv(r7_22_final,"./Data/Playback_Results/2022/Outputs/2022_PlaybackSurveys_FWPR7_Cleaned10-6.csv", row.names = FALSE)



#### REGION 5 ########
# Region 5 is fine, will need to get this data and clean it later but for now it's good
r5_22 <- read.csv("./Data/Metadata/Raw_Data/2022_ARUDeployment_Metadata_FWPR5.csv") %>% 
  clean_names()
### NEW RAW DATA
r5_22 <- read.csv('./Data/Playback_Results/2022/Raw_Data/2022_BBCUPlaybackSessionResults_FWPR5.csv')
#r5_22 <- r5_22 %>% rename(species = playback_cuckoo_detection)
# create a new column for YBCU and BBCU detected
r5_PB_22 <- r5_22 %>% rename(obs = observer, date = date_deployed, lat = latitude, long = longitude) %>% mutate(time = "UNK", bbcu = 0, ybcu = 0, interval = "PBUNK", distance = "UNK", bearing = "UNK", how = "UNK", visual = "UNK", call = "UNK", sex = "UNK", cluster = "UNK", notes = "waiting on full data from Megan" )
r5_PB_22 <- separate_site_make_survey_id(r5_PB_22)
r5_PB_22 <- make_date_format(r5_PB_22)
r5_22_final <- reorder_final_cols(r5_PB_22)
#test <- clean_arus(r5_22)
# works well





#### REGION 6 #####
r6PB_22 <- read.csv("./Data/Playback_Results/2022/Raw_Data/2022_BBCUPlaybackSessionResults_FWPR6.csv") %>% clean_names() 

# create a column for the cuckoo detections
r6PB_22 <- create_binary_cuckoo(r6PB_22)
# Clean the interval column
r6PB_22 <- clean_intervals(r6PB_22)
# Clean the ARU_ID column
r6PB_22 <- clean_aru_r622(r6PB_22)
# Make the date column a date
r6PB_22 <- make_date_format(r6PB_22)

# CHECKPOINT: REGION 6 #####
# sum the number of BBCU and YBCU for each site
check61 <- r6PB_22 %>% group_by(aru_id, species) %>% summarize(count = n())
sum(check61$species == "BBCU") # 1 point with bbcu
sum(check61$species == "YBCU") # 0 point with ybcu


## Region 6 CONT #######
# Read in metadata
r6_metadat <- read_csv("./Data/Metadata/Raw_Data/2022_ARUDeployment_Metadata_FWPR6.csv") %>% clean_names()
# rename columns and select only the necessary
r6_metadat <- rename_cols_and_select(r6_metadat)



# join the playback and the metadata
r6_22 <- join_playback_metadata(r6PB_22,r6_metadat)

# separate out the site and make a survey_id column
r6_22 <- separate_site_make_survey_id(r6_22)

# Now check which columns are missing and add them in or rename
r6_22 <- r6_22 %>% rename(obs = observer)
r6_22 <- r6_22 %>% rename(time = start)
r6_22 <- make_unentered_columns(r6_22,"how")
# Clean the time column 
test <- clean_time(r6_22)

#choose the final columns to include in playback data
r6_22_final <- reorder_final_cols(r6_22)

##### CHECKPOINT: REGION 6 #####
# sum the number of BBCU and YBCU for each site
check62 <- r6_22_final %>% group_by(point_id) %>% summarize(count_bbcu = ifelse(sum(bbcu, na.rm = TRUE)>=1,1,0)) 
sum(check62$count_bbcu, na.rm = TRUE) # 1 sites with bbcu
check63 <- r6_22_final %>% group_by(point_id) %>% summarize(count_ybcu = ifelse(sum(ybcu, na.rm = TRUE)>=1,1,0)) 
sum(check63$count_ybcu, na.rm = TRUE) # 0 sites with bbcu
## GOOD TO GO



# WRITE REGION 6 DATA #####
#write.csv(r6_22_final,"./Data/Playback_Results/2022/Outputs/2022_PlaybackSurveys_FWPR6_Cleaned10-9.csv", row.names = FALSE)





### UMBEL Playback Data ############
umbel_22 <- read.csv("./Data/Playback_Results/2022/Raw_Data/2022MMR_CuckooPlaybackData_UMBEL.csv") %>% clean_names()
# fix the point_id
umbel_22$point_id <- str_replace(umbel_22$point_id,"_", "-")

#### UMBEL CHECK POINT #####
# sum the number of BBCU and YBCU for each site
checku1 <- umbel_22 %>% group_by(point_id, species) %>% summarize(count = n())
sum(checku1$species == "BBCU") # 2 point with bbcu
sum(checku1$species == "YBCU") # 0 point with bbcu

### UMBEL CONT ######
# create a column for the cuckoo detections
umbelPB_22 <- create_binary_cuckoo(umbel_22)
# Clean the interval column
umbelPB_22 <- clean_intervals(umbelPB_22)
# Make the date column a date
umbelPB_22 <- make_date_format(umbelPB_22)

# Read in metadata
umbel_metadat <- read_csv("./Data/Metadata/Raw_Data/2022_ARUDeployment_Metadata_UMBEL.csv") %>% clean_names() %>% rename(lat_2022 = lat_if_new_or_from_2021, long_2022 = long_if_new_or_from_2021)
# read in monitoring points
umbel_points <- read.csv("./Data/Monitoring_Points/UMBEL_LetterNamedPoints2022.csv") %>% clean_names() %>% select(gps_id,lat,long) %>% rename(point_id = gps_id) 


# Fill in missing lat and long values in 'umbel_metadat' with the values from umbel_points
umbel_metadat <- umbel_metadat %>%
  mutate(
    lat = ifelse(is.na(lat_2022), umbel_points$lat[match(point_id, umbel_points$point_id)], lat_2022),
    long = ifelse(is.na(long_2022), umbel_points$long[match(point_id, umbel_points$point_id)], long_2022)
  )
# Write this metadata for use elsewhere
#write.csv(umbel_metadat, "./Data/Metadata/Outputs/2022_ARUDeployment_Metadata_UMBEL_Cleaned10-9.csv", row.names = FALSE)
# Filled in missing values from UMBEL_LongTermSites datasheet, sent email to Anna N to double chck

# need to left join this with monitoring points 
# rename columns and select only the necessary
umbel_metadat <- rename_cols_and_select(umbel_metadat)

# join the playback and the metadata
umbel_22 <- join_playback_metadata_bypoint(umbelPB_22,umbel_metadat)

# separate out the site and make a survey_id column
umbel_22 <- separate_site_make_survey_id(umbel_22)

# Now check which columns are missing and add them in or rename
# Now check which columns are missing and add them in or rename
umbel_22 <- umbel_22 %>% rename(bearing = cuckoo_bearing)
umbel_22 <- umbel_22 %>% rename(cluster = cluster_sz) # not sure what cluster code is
umbel_22 <- umbel_22 %>% rename(notes = bird_notes)
umbel_22 <- make_unentered_columns(umbel_22,"call")

#choose the final columns to include in playback data
umbel_22_final <- reorder_final_cols(umbel_22)

# CHECKPOINT: UMBEL #####
# sum the number of BBCU and YBCU for each site
checku2 <- umbel_22_final %>% group_by(point_id) %>% summarize(count_bbcu = ifelse(sum(bbcu, na.rm = TRUE)>=1,1,0)) 
sum(check62$count_bbcu, na.rm = TRUE) # 1 sites with bbcu
checku3 <- umbel_22_final %>% group_by(point_id) %>% summarize(count_ybcu = ifelse(sum(ybcu, na.rm = TRUE)>=1,1,0)) 
sum(checku3$count_ybcu, na.rm = TRUE) # 0 sites with bbcu
## GOOD TO GO

# WRITE UMBEL DATA #######
#write.csv(umbel_22_final,"./Data/Playback_Results/2022/Outputs/2022_PlaybackSurveys_UMBEL_Cleaned10-9.csv", row.names = FALSE)



##### Create a detection history for all orgs ####
all_cuckoo_22 <- rbind(umbel_22_final,r7_22_final,r6_22_final, r5_22_final)
cuckoo_summed <- all_cuckoo_22 %>% group_by(survey_id) %>% summarize(detection_hist_bbcu = max(bbcu),detection_hist_ybcu = max(ybcu)) %>% arrange()
# Summarize by site 
pb_22_summed <- all_cuckoo_22 %>% group_by(site_id) %>% summarize(lat_avg = mean(lat), long_avg = mean(long),detection_hist_bbcu = max(bbcu, na.rm = TRUE),detection_hist_ybcu = max(ybcu, na.rm = TRUE)) 
# Make a new column for visualization in ARCGIS
pb_22_summed <- pb_22_summed %>%
  mutate(
    species_detection = ifelse(detection_hist_bbcu == 1, "BBCU",
                        ifelse(detection_hist_ybcu == 1, "YBCU", "NOBI"))
  )
# Write this to a new dataframe
write.csv(pb_22_summed, "./Data/Playback_Results/2022/Outputs/2022_PlaybackSurveys_SiteLevelCuckoo_11-29.csv", row.names = FALSE)





#### Secondary Playback Cleaning 3/2024 ###################

# Manual edits to 22 UMBEL data 3/25/24:
## Added in M1-M5 intervals, since they didn't record it unless they heard a cuckoo during it. Start time for intervals was calculated as 5 min before the start of the playback period

# Read in all of the playback data
r5_22 <- read.csv("./Data/Playback_Results/2022/Raw_Data/2022_BBCUPlaybackSessionResults_FWPR5.csv")
r6_22 <- read.csv("./Data/Playback_Results/2022/Raw_Data/Output_After_First_Cleaning/2022_PlaybackSurveys_FWPR6_Cleaned10-9.csv")
r7_22 <- read.csv("./Data/Playback_Results/2022/Raw_Data/Output_After_First_Cleaning/2022_PlaybackSurveys_FWPR7_Cleaned10-6.csv")
umbel_22 <- read.csv("./Data/Playback_Results/2022/Raw_Data/Output_After_First_Cleaning/2022_PlaybackSurveys_UMBEL_Cleaned10-9_addedintervals.csv")

# create a metadata sheet
# Add on the number observers column
r6_22 <- r6_22 %>% mutate(num_obs = 1) %>% reorder_cols
# Add on the number of observers column
r7_22 <- r7_22 %>% mutate(num_obs = 1) %>% reorder_cols
# Add on the number of observers column, assign more than one observer to my data
# unique(umbel_22$obs)
umbel_22 <- umbel_22 %>% mutate(num_obs = case_when(obs == "AK" ~ 2,
                                                    obs == "BOC" ~ 1,
                                                    obs == "RPD" ~ 1,
                                                    obs == "ACN" ~ 1)) %>%
  reorder_cols

pb_22_all <- rbind(r5_22,r6_22,r7_22,umbel_22)
# Write this to the outputs as unfiltered data
#write.csv(pb_22_all, "./Data/Playback_Results/2022/Outputs/2022_PlaybackData_AllCollaborators.csv",row.names = FALSE)



# Create a document that has both the coordinates for each site and the coordinates averaged
site_locs_all_22 <- pb_22_all %>% group_by(point_id) %>% summarize(lat = mean(lat),long = mean(long))
site_locs_all_22 <- site_locs_all_22 %>% separate(point_id, into = c("site","id"), remove = FALSE) 
# Write this to a csv
#write.csv(site_locs_all_22, "./Data/Playback_Results/2022/Outputs/2022_PlaybackSiteLocations_All_3-25-2023.csv",row.names = FALSE)



# Create a playback sheet by removing metadata columns
pb_22_all <- pb_22_all %>% clean_time %>% rename(start_time = time, bbcu_detection = bbcu, ybcu_detection = ybcu, observer = obs)
# Select columns of interest
pb_22_all <- pb_22_all %>% mutate(across(c(distance, bearing, visual, call, cluster), ~ifelse(. == "", NA, .)))
pb_22_data <- pb_22_all %>% select_final_pbdata_cols
# write this to outputs
#write.csv(pb_22_data, "./Data/Playback_Results/2022/Outputs/2022_PlaybackData_3-25-24.csv",row.names = FALSE)



# Create a metadata sheet by grouping by survey ID and selecting the relevant columns
pb_22_metad <- pb_22_all %>% mutate(sky_start = "no_data",
                                             sky_end = "no_data",
                                             wind_start = "no_data",
                                             wind_end = "no_data",
                                             temp_start = "no_data",
                                             temp_end = "no_data")
pb_22_meta <- pb_22_metad %>% group_by(survey_id,
                                      date) %>% 
  summarize(survey_site_start_time = min(start_time), 
            observer = paste(unique(observer), collapse = ", "),
            num_obs = max(num_obs),
            num_points = length(unique(point_id)),
            lat_avg = mean(lat),
            long_avg = mean(long),
            sky_start = paste(unique(sky_start)),
            sky_end = paste(unique(sky_end)),
            wind_start = paste(unique(wind_start)),
            wind_end = paste(unique(wind_end)),
            temp_start = paste(unique(temp_start)),
            temp_end = paste(unique(temp_end)),
            detection_hist_bbcu = max(bbcu_detection, na.rm = TRUE),
            detection_hist_ybcu = max(ybcu_detection, na.rm = TRUE),
            notes = paste(notes, collapse = ""))
# Remove the weird NA values from the notes column
#unique(pb_22_meta$notes)
pb_22_meta$notes <- str_replace(pb_22_meta$notes,"NANANANANANANANANANANANANANANANANANANANANANANANANANANANANA","")

# Select the columns we want
metad_22_wcoords <- pb_22_meta %>% select_final_metad_cols()
# Write this to csv
#write.csv(metad_22_wcoords, "./Data/Playback_Results/2022/Outputs/2022_PlaybackSurveyMetadataWDetectionsCoords_3-25-24.csv", row.names = FALSE)



# Create a datasheet for ArcGIS that has average playback site locations and cuckoo detections at each site
# split cuckoo_summed by site
site_detections_22 <- metad_22_wcoords %>% separate(survey_id, into = c("site","survey_num"), sep = "#")
site_detections_22 <- site_detections_22 %>% group_by(site,lat_avg,long_avg) %>% summarize(bbcu_detected = max(detection_hist_bbcu),ybcu_detected = max(detection_hist_ybcu)) %>% select(site,bbcu_detected,ybcu_detected,lat_avg,long_avg)
# Write this to a .csv file
#write.csv(site_detections_22, "./Data/Playback_Results/2022/Outputs/2022_PlaybackSiteLocationsAvgWithDetections_3-25-24.csv", row.names = FALSE)







# CODE GRAVEYARD #########
# r7PB_22 <- r7PB_22 %>% 
#   mutate(bbcu = ifelse(species == "BBCU",1,0)) %>% 
#   mutate(ybcu = ifelse(species == "YBCU",1,0)) 

# Original
# r7PB_22 <- r7PB_22 %>%
#   mutate(
#     bbcu = ifelse(species == "BBCU", 1,
#                   ifelse(species %in% c("NOBI", "YBCU"), 0, "no_data"))
#   ) %>%
#   mutate(
#     ybcu = ifelse(species == "YBCU", 1,
#                   ifelse(species %in% c("NOBI", "BBCU"), 0, "no_data"))
#   )

# # Read in playback and metadata, clean names, rename, and select the one that you want
# r5_22 <- read.csv("./Data/Metadata/Raw_Data/2022_ARUDeployment_Metadata_FWPR5.csv") %>% 
#   clean_names() %>% 
#   rename(lat = latitude, long = longitude, bbcu_detected = playback_cuckoo_detection)
# 
# r7PB_22 <- read.csv("./Data/Playback_Results/2022/Raw_Data/2022_R7_PlaybackSurveyData.csv") %>% clean_names() %>% rename(aru_id = point)
# 
# # Turn the cuckoo_detected column into YBCU detected and BBCU detected

# r7_metadat %>% rename(
#   lat = case_when(
#     latitude == "lat" ~ "latitude",
#     TRUE ~ "latitude"
#   ),
#   long = case_when(
#     longitude == "long" ~ "longitude",
#     TRUE ~ "longitude"
#   )
# ) %>%
#   select(point_id, aru_id, lat, long)
#metadata %>% rename(long = longitude, lat = latitude) %>% select(point_id, aru_id, lat, long)
# 
# remove_non_alphanumeric <- function(dataframe, column) {
#   dataframe[[column]] <- gsub("[^[:alnum:]]", "", dataframe[[column]])
#   return(dataframe)
# }
# # Apply remove_non_alphanumeric to all columns in the dataframe
# for (col_name in colnames(r7_metadat)) {
#   r7_metadat <- remove_non_alphanumeric(r7_metadat, col_name)
# }
# 
# remove_spaces_long_lat <- function(dataframe) {
#   dataframe$lat <- str_replace(dataframe$lat, "\\S" , "")
#   dataframe$long <- str_replace(dataframe$long, "\\S", "")
#   return(dataframe)
# }
# 
# test <- remove_spaces_long_lat(r7_metadat)
# 
# # Create a function to remove spaces from a column
# remove_spaces <- function(dataframe,column) {
#   dataframe$column <- str_replace(dataframe$column, " ", "")
# }
# 
# remove_spaces <- function(dataframe, column) {
#   dataframe[[column]] <- str_replace(dataframe[[column]], " ", "")
#   return(dataframe)
# }
# 
# for(col_name in colnames(r7_metadat)){
#   r7_metadat <- remove_spaces(r7_metadat, col_name)
# }
# 
# for(i in ncol(r7_metadat)){
#   # apply remove_spaces across every column
# }
# 
# apply(r7_metadat, 2, remove_spaces, )
# 
# # Create a function to apply remove_spaces to all columns in a dataframe
# remove_spaces_from_all_columns <- function(dataframe) {
#   result <- dataframe %>%
#     mutate_all(.funs = list(remove_spaces))
#   return(result)
# }
# 
# # Example usage:
# # Replace "your_data" with the name of your dataframe
# test <- remove_spaces_from_all_columns(r7_metadat)
# 
# 
# # create a function to read in the metadata and remove spaces from columns
# dataframe$col <- str_replace(dataframe$col," ", "")
# r7_metadat <- r7_metadat %>%
#   mutate_all(~trimws(.))

# test <- r7PB_22 %>% separate(point_id, into = c("site_id", "point"), sep = "-" , remove = FALSE) %>% mutate(survey_id = paste(site_id,"#1"))


# # Remove the spaces in the ARU ID column
# r7_22_sum$aru_id <- str_replace(r7_22_sum$aru_id,"U ", "")
# 
# 
# # need to link this with the metadata to link ARU ID to point ID
# 
# # %>%
# #   group_by(aru_id) %>% 
# #   summarize(bbcu_detected = sum(bbcu))
# 
# 
# # add on lat and long
# # Read in metadata
# r7MD_22 <- read_csv("./Data/Metadata/Raw_Data/2022_ARUDeployment_Metadata_FWPR7.csv") %>% clean_names() %>% rename(long = longitude, lat = latitude)

