#### Clean 2023 Playback Data ###################

## Purpose: to read in the raw data files from the playback data, clean them, and output the cleaned data into a new folder

# Created 8/23/2023

# Last modified: 3/25/2023

# Playback data:
# need to go through a change interval from 1-5 to M1-M5
# Metadata:
# rename num_surveyors to num_obs
# keep the coordinates
# remove colon from survey_site_start_time

# Later to do: add a column for year?


#### Setup #################################
packages <- c("data.table","tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)
# Load custom cleaning functions
source("./R_Scripts/6_Function_Scripts/Create_Functions_For_Cleaning_Playback_Data.R")



#### CLEAN METADATA #####
# Read in metadata files
fwp_ak <- read.csv("./Data/Playback_Results/2023/Raw_Data/2023_PlaybackSurveyMetadata_FWPR6.csv") %>% clean_names()
fwp_dj <- read.csv("./Data/Playback_Results/2023/Raw_Data/2023_PlaybackSurveyMetadata_FWP_DANIEL.csv") %>% clean_names()
fwp_mo <- read.csv("./Data/Playback_Results/2023/Raw_Data/2023_PlaybackSurveyMetadata_FWPR5.csv") %>% clean_names()
fwp_bs <- read.csv("./Data/Playback_Results/2023/Raw_Data/2023_PlaybackSurveyMetadata_BRS.csv") %>% clean_names()
umbel <- read.csv("./Data/Playback_Results/2023/Raw_Data/2023_PlaybackSurveyMetadata_UMBEL.csv") %>% clean_names()

# Bind them all together
pb23_meta <- rbind(fwp_ak, fwp_dj, fwp_mo, fwp_bs, umbel)
# Remove mosquitoes columns
pb23_meta <- pb23_meta %>% select(-c(mosquitoes_start, mosquitoes_end, mosquitoes_scale))
# Remove the rows of problematic surveys from Region 5
pb23_meta <- pb23_meta %>% filter(!survey_id %in% c("YWM#1.5", "GMW#1"))
# Remove other surveyors 
pb23_meta <- pb23_meta %>% filter(!observer %in% c("MIV","AES"))
#Check if there are any that are missing
#test1 <- pb23_meta %>% separate(survey_id, into = c("site","survey_num"))# %>% arrange(site) 
#test2 <- test1 %>% group_by(site) %>% summarize(n=n())
# Check which row is duplicated
#pb23_meta[duplicated(pb23_meta$survey_id) == TRUE,]
# remove row 44 from the dataframe that has the duplicate
# Filter out this row
pb23_meta <- unique(pb23_meta)
# This looks good
# Write this to a csv
#write.csv(pb23_meta, "./Data/Playback_Results/2023/Outputs/2023_PlaybackMetadata_NoCoords_3-22-24.csv", row.names = FALSE)




#### CLEAN PLAYBACK DATA ##############################################
# Manually cleaned up csv's to remove spaces 11/21/23
# Function: Change 1-5 in interval column to M1 etc
# Logic behind this is to make it the same data type as PB1 etc

# Other data from UMBEL sites 
an_umbel <- read.csv("./Data/Playback_Results/2023/Raw_Data/MMR2023_CuckooPlaybackData_v4clean_akmanuallyedited.csv") %>% clean_names()
# 3-8 manually added passive listening intervals - raw data version v3 archive, v4 is current
# remove some unnecessary columns
an_umbel <- an_umbel %>% select(-c("entry_order","entry_person","how2")) 
# rename columns to match other datasheets
an_umbel <- an_umbel %>% rename(obs=observer, 
                                site_id = site, 
                                start_time = time_format,
                                cluster = cluster_sz, 
                                notes = bird_notes)
# Combine columns into point id colum
an_umbel <- an_umbel %>% unite(point_id, c("site_id","point"),sep = "-", remove = FALSE)
# Need to work out the last couple columns, change to visual and call from how1
# V is visual C is calling (S is singing)
an_umbel <- an_umbel %>% mutate(visual = case_when(how1 == "V" ~ "Y",
                                                   how1 == "C" ~ "N",
                                                   how1 == "S"~ "N",
                                                   how1 == NA ~ NA),
                                call = case_when(how1 == "V" ~ "N",
                                                 how1 == "C" ~ "Y",
                                                 how1 == "S" ~ "Y",
                                                 how1 == NA ~ NA),
                                bearing = NA)
# Select only the columns we want to match the other datasets
an_umbel <- an_umbel <- an_umbel %>% select(obs,date,
                                            survey_id,
                                            site_id,
                                            point_id,
                                            start_time,
                                            interval,
                                            species,
                                            distance,
                                            bearing,
                                            visual,
                                            call,
                                            sex,
                                            cluster,
                                            notes)
# My data from UMBEL sites
ak_umbel <- read.csv("./Data/Playback_Results/2023/Raw_Data/2023_PlaybackPtCtSurveyData_UMBEL.csv") %>% clean_names() %>% filter(species %in% c("BBCU","YBCU","NOBI"))
# My data from FWP sites
ak_r6 <- read.csv("./Data/Playback_Results/2023/Raw_Data/2023_PlaybackPtCtSurveyData_FWPR6.csv") %>% clean_names() %>% filter(species %in% c("BBCU","YBCU","NOBI"))
# Daniel's data from FWP sites
dj_r567 <- read.csv("./Data/Playback_Results/2023/Raw_Data/2023_PlaybackSurveyData_FWP_DANIEL.csv")
# Brandi's Data
brs_r7 <- read.csv("./Data/Playback_Results/2023/Raw_Data/2023_PlaybackSurveydata_BRS.csv")
# Region 5 data
bu_r5 <- read.csv("./Data/Playback_Results/2023/Raw_Data/2023_PlaybackSurveydata_FWPR5.csv")
# Clean up one point
bu_r5$survey_id <- str_replace(bu_r5$survey_id,"GMW-1","GMW#2")
# Now remove the GMW#1 points, since the survey started outside of our survey period
bu_r5 <- bu_r5 %>% filter(!(survey_id %in% "GMW#1"))

# Combine all the data into one dataframe
pb23_all <- rbind(an_umbel,ak_umbel,ak_r6,dj_r567,brs_r7,bu_r5)
# Create a new column for cuckoo detections
pb23_all <- pb23_all %>% mutate(bbcu_detection = ifelse(species == "BBCU", 1, 0), ybcu_detection = ifelse(species == "YBCU", 1, 0)) %>% rename(observer = obs)
# Remove the colon from the time
pb23_all$start_time <- str_replace(pb23_all$start_time,":", "")
pb23_all <- pb23_all %>% mutate(across(c(distance, bearing, visual, call, cluster), ~ifelse(. == "", NA, .)))
pb_23_dat <- pb23_all %>% clean_intervals %>% select_final_pbdata_cols

# Write this output
#write.csv(pb_23_dat, "./Data/Playback_Results/2023/Outputs/2023_PlaybackData_3-25-24.csv", row.names = FALSE)

  
##### DONE Get the number of playback surveys conducted by UMBEL ####
pb_nwenergy <- umbel %>% filter(!observer %in% c("MIV","AES"))
nwenergy_surveys <- unique(pb_nwenergy$survey_id)
length(nwenergy_surveys)


#### DONE Get the coordinates that are associated with each point ####
# read in the metadata
pb_all <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackMetadata_NoCoords_3-22-24.csv")

# Assign coordinates
# separate by site
temp <- pb_all %>% separate(col = survey_id, into = c("site","ID"), sep = "#")
all_sites <- unique(temp$site) # total of 33 sites, 95 surveys

# pull in 2023_playbackpoints-FWP and UMBEL and filter out only the sites with their site in the list of points used in 2023
points_fwp <- read.csv("./Data/Monitoring_Points/2023_PlaybackPoints_FWP.csv") %>% rename(long = x, lat = y) %>% select(point_id, lat,long)
points_umbel <- read.csv("./Data/Monitoring_Points/2023_Playback_Survey_Points_UMBEL.csv") %>% rename(lat = latitude,long = longitude) %>% select(point_id,lat,long) 
# remove the sites that aren't formatted correctly and weren't used
#points_umbel <- points_umbel[-c(60:65),]
points_umbel <- points_umbel %>% filter(!point_id %in% c("SIP-1_old",
                                         "SIP-2_old",
                                         "Playback Site 2",
                                         "Playback Site 3",
                                         "Playback Site 5",
                                         "Playback Site 6",
                                         "Playback Site 7",
                                         "Playback Site 8") )
points_umbel$point_id <- str_replace(points_umbel$point_id,"_new", "")
# Bind fwp and umbel points together
points_all <- rbind(points_fwp,points_umbel)


# Separate points all into site and point
points_all <- points_all %>% separate(point_id, into = c("site","id"), sep = "-", remove = FALSE)
# Filter out the point that aren't in the temp ID
pb23_wcoord <- points_all %>% filter(site %in% temp$site)
# Mutate a new column that assigns the collaborator to each site

# write the playback points to a .csv
#write.csv(pb23_wcoord, "./Data/Playback_Results/2023/Outputs/2023_PlaybackSiteLocations_All_3-22-24.csv", row.names = FALSE)

# average the sites for use in visualization
points_avg <- pb23_wcoord %>% separate(col = point_id, into = c("site","id"), sep = "-") %>% group_by(site) %>% summarize(lat_avg = mean(lat), long_avg = mean(long))
# export this for use in ArcGIS
#write.csv(points_avg, "./Data/Playback_Results/2023/Outputs/2023_PlaybackSiteLocationsAvg_All_3-22-24.csv", row.names = FALSE)




#### Check Data and Create Deliverables  ######
## Include date, survey_id, start_time (take the minimum), max of bbcu_detection, max of ybcu_detection
# Add later: average of distance, visual Y if there was a Y in it, call Y if there was a Y in it (maybe transform these to 1's first?), max of cluster??

pb_data <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackData_3-25-24.csv")
# Read in metadata that has the coordinates
meta <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackMetadata_3-22-24.csv")
# Read in average site locations
avg_site_locs <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackSiteLocationsAvg_All_3-22-24.csv")

cuckoo_summed <- pb_data %>% group_by(survey_id) %>% summarize(detection_hist_bbcu = max(bbcu_detection),
                                                               detection_hist_ybcu = max(ybcu_detection),
                                                               num_points = length(unique(point_id))) %>% arrange()
# Can left join after this
cuckoo_summed <- as.data.frame(cuckoo_summed)

# Identify sites that appear in both data frames
common_site_ids <- cuckoo_summed %>%
  semi_join(meta, by = "survey_id")
# 94 that are in both
# Identify sites unique to playback data
cunique_to_pb_data <- cuckoo_summed %>%
  anti_join(meta, by = "survey_id")
# This is 0, looks good
# Identify sites unique to the metadata
cunique_to_meta <- meta %>%
  anti_join(cuckoo_summed, by = "survey_id")
# This is 0

# Combine the metadata with the playback data 
combined_dat <- left_join(meta,cuckoo_summed, by = "survey_id") %>% arrange(survey_id)
# Combine with long and lat points
combined_dat <- combined_dat %>% separate(survey_id, into = c("site","survey_num"), remove = FALSE) %>% left_join(avg_site_locs, by = "site") %>% rename(num_obs = num_surveyors) %>% clean_time_wcorrectcolname()
metad_23_wcoords <- combined_dat %>% select_final_metad_cols()
# Write this to csv
write.csv(metad_23_wcoords, "./Data/Playback_Results/2023/Outputs/2023_PlaybackSurveyMetadataWDetectionsCoords_3-22-24.csv", row.names = FALSE)


# Create a datasheet for ArcGIS that has average playback site locations and cuckoo detections at each site
# split cuckoo_summed by site
site_detections <- cuckoo_summed %>% separate(survey_id, into = c("site","survey_num"), sep = "#")
site_detections <- site_detections %>% group_by(site) %>% summarize(bbcu_detected = max(detection_hist_bbcu),ybcu_detected = max(detection_hist_ybcu)) 
site_detections_wcoords <- left_join(site_detections,avg_site_locs, by = "site")
# Write this to a .csv file
#write.csv(site_detections_wcoords, "./Data/Playback_Results/2023/Outputs/2023_PlaybackSiteLocationsAvgWithDetections_3-25-24.csv", row.names = FALSE)





### CODE GRAVEYARD ####
# What the function is based off of
# metad_wcoords <- combined_dat %>% select(survey_id, 
#                                          observer, num_obs, 
#                                          date, survey_site_start_time,
#                                          lat_avg,long_avg,
#                                          sky_start,sky_end,
#                                          wind_start,wind_end,
#                                          temp_start, temp_end,
#                                          detection_hist_bbcu, detection_hist_ybcu,
#                                          notes)
# Checking how many points there are
#pb_locs <- points_avg %>% filter(site %in% all_sites) # none missing - this looks good
# Total surveys
# 95 surveys for all the sites
# 3 points at each site except WBB that had 2
94 * 3 # 282
1 * 2 # 2
# Total number of surveys 
282 + 2 # 284

# Remove duplicates
#pb23_meta <- pb23_meta %>% distinct()

# remove certain lines - already manually cleaned
#pb_all <- pb_all %>% filter(!survey_id %in% c("84#2b","84#3b","203-1_2","^change this to 203_2"))

# filter out the points that are in the 2023 playback data
## OLD
#points_all <- points_all %>% separate(point_id, into = c("site","id"), sep = "-")
#pb23_wcoordd <- left_join(all_sites,points_all, by = "point_id")

#pb23_meta <- unique(pb23_meta[, "survey_id", drop = FALSE])
# # Pull out the number of surveys from Region 6
# fwp <- rbind(fwp_ak,fwp_dj)
# # separate the first column
# temp <- fwp %>% separate(col = survey_id, into = c("site","ID"), sep = "#")
# r6_surveys <- temp %>% filter(site %in% c("ROB","SNO","CUL","CLA"))
# r7_surveys <- temp %>% filter(!(site %in% c("ROB","SNO","CUL","CLA")))
# Old: 29 sites, 3 surveys at each site = 87 playback surveys

#condense1_23 <- rbind(ak_umbel,ak_r6,daniel)
# Pull out only NOBI, BBCU or YBCU from my data (UMBEL and FWPR6)
#condense1_23 <- condense1_23 %>% filter(species %in% c("BBCU","YBCU","NOBI")) # use this in conjunction with the other data

#an_umbel <- an_umbel %>% mutate(visual = ifelse(how1 == "V", "Y", "N"),
#                                call = ifelse(how1 %in% c("C","S"),"Y","N"))



