#### Clean CNN2.0 Clips Files Post Vetting ###################

## Purpose: to read in the clips sheets and create an output for daily BBCU presence/absence 

# Created 3/21/2024

# Last modified: 3/26/2024

# # To do:
# Mask out all of the dates that align with the dates of the playback
# Look at 2023 ELI-3 and 2023 PRD-3 in the File_Comments google sheets to determine if this is YBCU or BBCU
# Filter out files in the file_comments google sheet that were playback audio

#### Setup #################################
packages <- c("data.table","tidyverse","janitor","chron")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)


#### Filter out playback audio####
# Read in the playback metadata
pbmeta_23 <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackSurveyMetadataWDetectionsCoords_3-22-24.csv")
pbmeta_22 <- read.csv("./Data/Playback_Results/2022/Outputs/2022_PlaybackSurveyMetadataWDetectionsCoords_3-25-24.csv")
pbmeta_21 <- read.csv("./Data/Playback_Results/2021/Outputs/2021_PlaybackSurveyMetadataWDetectionsCoords_3-25-24.csv")
pbmeta <- rbind(pbmeta_21,pbmeta_22,pbmeta_23)
# format date as date and time as time
pbmeta <- pbmeta %>% mutate(date_formatted=as.Date(date, format="%Y-%m-%d"))
# format the time as time

#test <- pbmeta %>% mutate(time_formatted = as.POSIXct(as.character(survey_site_start_time), format = "%H%M"))
pbmeta <- pbmeta %>%
  mutate(time_formatted = as.POSIXct(sprintf("%04d", survey_site_start_time), format = "%H%M"))
pbmeta <- pbmeta %>%
  mutate(time_only = format(time_formatted, format = "%H:%M:%S"))
pbmeta <- pbmeta %>%
  mutate(time = times(time_only, format = "h:m:s")) %>% select(-c(time_formatted,time_only))

# Should I format them as date and time columns or as integers????

pbmeta$date_int <- str_replace(pbmeta$date,"-","")
pbmeta$date_int <- as.numeric(str_replace(pbmeta$date_int,"-",""))

# Create a 2-hour time interval after playbacks to screen out
# Filter clip data through this

# Filter out YBCU detections
# method for doing this?
# Assign a new column for spaced_coo that is 1 if the file name is in the potential YBCU files from the google drive
# OR mutate call_type to spaced_coo if the file name is in the potential YBCU files from the google drive 
# Checking the files from the google drive: check that they meet the pattern SITE-#/YYYMMDD_HMMSS_SSS.0s-SSS.0s_call_type.wav
# visualize these for each site with date on the x axis, number (sum of annotation?) on the y axis and color corresponding to type of call 



