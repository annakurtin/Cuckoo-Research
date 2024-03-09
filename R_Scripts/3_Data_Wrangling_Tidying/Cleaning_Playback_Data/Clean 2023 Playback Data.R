#### Clean 2023 Playback Data ###################

## Purpose: to read in the raw data files from the playback data, clean them, and output the cleaned data into a new folder

# Created 8/23/2023

# Last modified: 3/8/2023


# What I need to go through and look at
## Did each of the playback surveys have 3 surveys at the site?
## Were each surveys within the 3 week period?


#### Setup #################################
packages <- c("data.table","tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)




#### METADATA #####
# Read in metadata files
fwp_ak <- read.csv("./Data/Playback_Results/2023/Raw_Data/2023_PlaybackSurveyMetadata_FWPR6.csv") %>% clean_names()
fwp_dj <- read.csv("./Data/Playback_Results/2023/Raw_Data/2023_PlaybackSurveyMetadata_FWP_DANIEL.csv") %>% clean_names()
fwp_mo <- read.csv("./Data/Playback_Results/2023/Raw_Data/2023_PlaybackSurveyMetadata_FWPR5.csv") %>% clean_names()
# Waiting on response from Megan about cleaning this
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
test1 <- pb23_meta %>% separate(survey_id, into = c("site","survey_num"))# %>% arrange(site) 
test2 <- test1 %>% group_by(site) %>% summarize(n=n())
# This looks good
# Write this to a csv
write.csv(pb23_meta, "./Data/Playback_Results/2023/Outputs/2023_PlaybackMetadata_NoCoords_3-8-24.csv", row.names = FALSE)


  
##### DONE Get the number of playback surveys conducted by UMBEL ####
pb_nwenergy <- umbel %>% filter(!observer %in% c("MIV","AES"))
nwenergy_surveys <- unique(pb_nwenergy$survey_id)
length(nwenergy_surveys)


#### DONE Get the coordinates that are associated with each point ####
# get all total surveys
pb_all <- rbind(fwp_ak,fwp_dj,fwp_mo,fwp_bs,umbel)
# Remove observer AES and MIV (didn't transcribe this data)
pb_all <- pb_all %>% filter(!observer %in% c("MIV","AES"))


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
#write.csv(pb23_wcoord, "./Data/Playback_Results/2023/Outputs/2023_PlaybackSiteLocations_All_11-28.csv", row.names = FALSE)

# average the sites for use in visualization
points_avg <- pb23_wcoord %>% separate(col = point_id, into = c("site","id"), sep = "-") %>% group_by(site) %>% summarize(lat_avg = mean(lat), long_avg = mean(long))
# export this for use in ArcGIS
#write.csv(points_avg, "./Data/Playback_Results/2023/Outputs/2023_PlaybackSiteLocationsAvg_All_11-28.csv", row.names = FALSE)

# Checking how many points there are
#pb_locs <- points_avg %>% filter(site %in% all_sites) # none missing - this looks good
# Total surveys
# 95 surveys for all the sites
# 3 points at each site except WBB that had 2
94 * 3 # 282
1 * 2 # 2
# Total number of surveys 
282 + 2 # 284




#### CLEAN PLAYBACK DATA ##############################################
# Manually cleaned up csv's to remove spaces 11/21/23

# Other data from UMBEL sites 
an_umbel <- read.csv("./Data/Playback_Results/2023/Raw_Data/MMR2023_CuckooPlaybackData_v4clean_akmanuallyedited.csv") %>% clean_names()
# remove some unnecessary columns
an_umbel <- an_umbel %>% select(-c("entry_order","entry_person","how2")) 
an_umbel <- an_umbel %>% rename(obs=observer, 
                                point_id = point, 
                                site_id = site, 
                                start_time = time_format)
# Need to work out the last couple columns, change to visual and call from how1________________________________
# V is visual C is calling (S is singing)
an_umbel <- an_umbel %>% mutate(visual = case_when(how1 == "V" ~ "Y",
                                                   how1 == "C" ~ "N",
                                                   how1 == "S"~ "N",
                                                   how1 == NA ~ NA),
                                call = case_when(how1 == "V" ~ "N",
                                                 how1 == "C" ~ "Y",
                                                 how1 == "S" ~ "Y",
                                                 how1 == NA ~ NA))


# 3-8 manually added passive listening intervals - raw data version v3 archive, v4 is current

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

# Combine all the data into one dataframe
pb23_all <- rbind(ak_umbel,ak_r6,dj_r567,brs_r7,bu_r5)
# Create a new column for cuckoo detections
pb23_all <- pb23_all %>% mutate(bbcu_detection = ifelse(species == "BBCU", 1, 0), ybcu_detection = ifelse(species == "YBCU", 1, 0))
# Remove the colon from the time
pb23_all$start_time <- str_replace(pb23_all$start_time,":", "")

# Write this output
#write.csv(pb23_all, "./Data/Playback_Results/2023/Outputs/2023_PlaybackData_3-8-24.csv", row.names = FALSE)








### CODE GRAVEYARD ####


# Remove duplicates
#pb23_meta <- pb23_meta %>% distinct()

# remove certain lines - already manually cleaned
#pb_all <- pb_all %>% filter(!survey_id %in% c("84#2b","84#3b","203-1_2","^change this to 203_2"))

# filter out the points that are in the 2023 playback data
## OLD
#points_all <- points_all %>% separate(point_id, into = c("site","id"), sep = "-")
#pb23_wcoordd <- left_join(all_sites,points_all, by = "point_id")


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



#### Condensing this down  ######

pb_data <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackData_3-8-24.csv")
# Read in metadata that has the coordinates

# Include detection data for each survey ID __________________________________________________________
# We want to condense across each survey ID 
## Include date, survey_id, start_time (take the minimum), max of bbcu_detection, max of ybcu_detection, average of distance, visual Y if there was a Y in it, call Y if there was a Y in it (maybe transform these to 1's first?), max of cluster
## Does it matter if some playbacks were stopped/started by a minute or so? I don't think so
# Pick up here again __________________________________________
cuckoo_summed <- all_cuckoo %>% group_by(survey_id) %>% summarize(detection_hist_bbcu = max(bbcu_detection),detection_hist_ybcu = max(ybcu_detection)) %>% arrange()