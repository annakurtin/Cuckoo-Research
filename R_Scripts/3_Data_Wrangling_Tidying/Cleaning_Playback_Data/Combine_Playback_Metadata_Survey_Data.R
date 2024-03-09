#### Combine Playback Metadata and Survey Data ###################

## Purpose: to read in the cleaned playback files and combine them for use in modeling 

# Created 3/8/2023

# Last modified: 3/8/2023



#### Setup #################################
packages <- c("data.table","tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)

#### Create output: combine average coordinates for site with detection/nondetection at the site ####
pb23_pointcoord <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackSiteLocations_All_11-28.csv")
pb23_avgsitecoord <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackSiteLocationsAvg_All_11-28.csv")
pb23_meta <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackMetadata_NoCoords_3-8-24.csv")
pb23_data <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackData_3-8-24.csv")

# Convert visual and call into a 1/0 for summarizing
pb23_data <- pb23_data %>% mutate(visual = case_when(visual == "Y" ~ 1,
                                                     visual == "N" ~ 0,
                                                     visual == NA ~ NA),
                                  call = case_when(call == "Y" ~ 1,
                                                   call == "N" ~ 0,
                                                   call == NA ~ NA))


## Removed the cluster and sex columns bc they don't have any useful information 
pb23_condense <- pb23_data %>% group_by(survey_id) %>% summarize(date = min(date), 
                                                                 earlist_starttime = min(start_time),
                                                                 bbcu = max(bbcu_detection),
                                                                 ybcu = max(ybcu_detection),
                                                                 avg_dist = mean(distance),
                                                                 cluster = max(cluster),
                                                                 max_dist = max(distance, na.rm = TRUE),
                                                                 avg_dist = mean(distance, na.rm = TRUE),
                                                                 visual_detection = max(visual),
                                                                 call_detection = max(call))


#test1 <- pb23_data %>% filter(species %in% c("YBCU","BBCU"))

# Separate the metadata out 
pb23_meta <- pb23_meta %>% separate(survey_id, into = c("site","survey_num"),sep = "#", remove = FALSE)
# Use the site column to combine with averaged coordinates
pb23_metawcoords <- left_join(pb23_meta,pb23_avgsitecoord, by = "site") %>% select(-"cuckoo.")
# Combine this with the condensed survey data 
test2 <- left_join(pb23_metawcoords,pb23_condense,by = "survey_id")

# NEED TO DO A TEST TO SEE WHY SOME DATA IS IN METADATA AND NOT SURVEY DATA
# Identify sites that appear in both data frames
common_site_ids <- pb23_condense %>%
  semi_join(pb23_metawcoords, by = "survey_id")

# Identify sites unique to playback
unique_to_pb <- pb23_condense %>%
  anti_join(pb23_metawcoords, by = "survey_id")
# NEED TO GO CLEAN ONE OF THE GMW SITES THAT HAS GMW-1 FOR SURVEY ID ??????????????????

# Identify sites unique to aru
unique_to_metad <- pb23_metawcoords %>%
  anti_join(pb23_condense, by = "survey_id")
# FIGURE OUT WHY THESE 13 SURVEYS DONT HAVE A MATCH IN THE SURVEY ID ???????????????????

# Look at the output?
#plot(test, x="long_avg", y = "lat_avg", col = "cuckoo.")

