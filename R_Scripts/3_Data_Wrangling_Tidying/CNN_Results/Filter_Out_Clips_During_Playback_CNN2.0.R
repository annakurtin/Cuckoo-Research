#### Filter Out Playback Audio From CNN2.0 Clips ###################

## Purpose: to read in the clips sheets and filter out the clips that took place during playbacks

# Created 3/21/2024

# Last modified: 4/9/2024

# # To do:
# Mask out all of the dates that align with the dates of the playback
# Look at 2023 ELI-3 and 2023 PRD-3 in the File_Comments google sheets to determine if this is YBCU or BBCU
# Filter out files in the file_comments google sheet that were playback audio

#### Setup #################################
packages <- c("data.table","tidyverse","janitor","chron")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)

# Load in clip annotation data
#r5_22 <- read.csv("./Data/Classifier_Results/Model2.0/2022_FWPR5_topclips_perSiteperPeriod_annotations_v2_FINAL_AK3-7.csv")
#r5_23 <- read.csv("./Data/Classifier_Results/Model2.0/2023_FWPR5_topclips_perSiteperPeriod_annotations_v2_FINAL_AK_3-22.csv")
#r6_23 <- read.csv("./Data/Classifier_Results/Model2.0/2023_FWPR6_topclips_perSiteperPeriod_annotations_v2_FINAL_AK_3-22.csv")
#r7_23 <- read.csv("./Data/Classifier_Results/Model2.0/2023_FWPR7_topclips_perSiteperPeriod_annotations_v2_FINAL_AK_3-22.csv")
#umbel_23 <- read.csv("./Data/Classifier_results/Model2.0/2023_UMBEL_topclips_perSiteperPeriod_annotations_v2_AK_3-22.csv")
#clip_dat <- rbind(r5_22,r5_23,r6_23,r7_23)
# CHANGE LATER: reading in the ones annotated conservatively for Tessa
r6_22 <- read.csv("./Data/Classifier_Results/Model2.0/Conservative_ForTessa/2022_FWPR6_topclips_perSiteperPeriod_annotations_v2_3-30.csv")
r7_22 <- read.csv("./Data/Classifier_Results/Model2.0/Conservative_ForTessa/2022_FWPR7_topclips_perSiteperPeriod_annotations_v2_3-30.csv")
umbel_22 <- read.csv("./Data/Classifier_Results/Model2.0/Conservative_ForTessa/2022_UMBEL_topclips_perSiteperPeriod_annotations_v2_3-30.csv")
umbel_23 <- read.csv("./Data/Classifier_Results/Model2.0/Conservative_ForTessa/2023_UMBEL_topclips_perSiteperPeriod_annotations_v2_3-30.csv")
clip_dat <- rbind(r6_22,r7_22,umbel_22,umbel_23)





# Format date and time as a character
clip_dat$date <- as.character(clip_dat$date)
#clip_dat$time <- as.character(clip_dat$hour)
clip_dat$time <- sprintf("%06d", clip_dat$hour) 
# Crete a new column for date formatted as a date
clip_dat$date_formatted <- as.Date(clip_dat$date, format = "%Y%m%d")
# Create formatted time column (hh:mm:ss)
clip_dat$time_formatted <- format(strptime(clip_dat$time, format = "%H%M%S"), format = "%H:%M:%S")

# Create formatted datetime column (yyyy-mm-dd hh:mm:ss)
clip_dat$datetime <- as.POSIXct(paste(clip_dat$date, clip_dat$time), format = "%Y%m%d %H%M%S")
#test <- clip_dat %>% select(date,date_formatted,time,time_formatted,datetime) # looks good 
clip_dat$datetime <- as.POSIXct(clip_dat$datetime, tz = "UTC")  # Set initial timezone to "UTC"
clip_dat$datetime <- with_tz(clip_dat$datetime, "America/Denver")  # Convert to UTC-6 (MDT)
clip_dat$start_clip <- clip_dat$datetime + clip_dat$start_time
clip_dat <- clip_dat %>% separate(point_id, into= c("site_id","point_num"),sep = "-", remove = FALSE)



#### Filter out playback audio####
# Read in the playback metadata
pbmeta_23 <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackSurveyMetadataWDetectionsCoords_3-22-24.csv")
# Convert to date format
pbmeta_23$date <- as.Date(pbmeta_23$date, format = "%m/%d/%Y")
pbmeta_22 <- read.csv("./Data/Playback_Results/2022/Outputs/2022_PlaybackSurveyMetadataWDetectionsCoords_3-28-24.csv")
# Convert to date format
pbmeta_22$date <- as.Date(pbmeta_22$date, format = "%m/%d/%Y")
pbmeta_21 <- read.csv("./Data/Playback_Results/2021/Outputs/2021_PlaybackSurveyMetadataWDetectionsCoords_3-25-24.csv")
# Convert to date format
pbmeta_21$date <- as.Date(pbmeta_21$date, format = "%Y-%m-%d")
# Combine dataframes
pbmeta <- rbind(pbmeta_21,pbmeta_22,pbmeta_23)

# format the time as time
pbmeta <- pbmeta %>%
  mutate(time_formatted = as.POSIXct(sprintf("%04d", survey_site_start_time), format = "%H%M"))
pbmeta <- pbmeta %>%
  mutate(time_only = format(time_formatted, format = "%H:%M:%S"))
pbmeta <- pbmeta %>%
  mutate(time = times(time_only, format = "h:m:s")) %>% select(-c(time_formatted,time_only))
# Format as datetime 
pbmeta$datetime <- as.POSIXct(paste(pbmeta$date, pbmeta$time), format = "%Y-%m-%d %H:%M:%S")
# Add a timezone
pbmeta$datetime <- as.POSIXct(pbmeta$datetime, tz = "UTC")  # Set initial timezone to "UTC"
pbmeta$datetime <- with_tz(pbmeta$datetime, "America/Denver")  # Convert to UTC-6 (MDT)
pbmeta$interval_start <- pbmeta$datetime - (30*60)
pbmeta$interval_end <- pbmeta$datetime + (2*60*60)
# Can you do logical comparisons?
#pbmeta[1,20] >pbmeta[1,19] # yes
# make a column for site id
pbmeta <- pbmeta %>% separate(survey_id, into= c("site_id","survey_num"),sep = "#", remove = FALSE)


# make a column for site within the pbmeta 
# left join playback metadata by the site 

#### Filter out acoustic files that were recorded during playbacks ######
# filtered_clip_dat <- clip_dat %>% filter(!(start_clip %within% interval(pbmeta$interval_start,pbmeta$interval_end)))

# made small data to test it on
#small_clipdat <- clip_dat %>% filter(site_id %in% c("82","PRD"))
sites_list <- unique(clip_dat$site)

# initialize an empty dataframe
final_df <- data.frame()
# for each site id
for (site in sites_list){
  temp_clip <- clip_dat %>% filter(site_id == site)
  temp_pb <- pbmeta %>% filter(site_id == site)
  # initialize a logical vector to track if start_clip is within any interval
  during_playbacks <- logical(nrow(temp_clip))
  # filter the rows that aren't in the playback interval
  # for each row in temp_clip, check if start_clip is within any interval
  for (i in 1:nrow(temp_clip)) {
    during_playbacks[i] <- any(temp_clip$start_clip[i] %within% interval(temp_pb$interval_start, temp_pb$interval_end))
  }
  
  # add during_playbacks as a new column in temp_clip
  temp_clip$during_playbacks <- during_playbacks
  
  # filter temp_clip based on the logical vector
  filtered_site_clips <- temp_clip %>% filter(during_playbacks == FALSE)
  # not properly registering when the start_clip is within the interval
  # bind it to dataframe 
  final_df <- rbind(final_df,filtered_site_clips)
}

#test <- final_df %>% filter(point_id=="82-3")

# Identify which clips are only in the original clips data
removed_from_final <- clip_dat %>%
  anti_join(final_df, by = "clip")
# Looks good

# Write this to a .csv to send to tessa
#write.csv(final_df, "./Data/Classifier_Results/Model2.0/Conservative_ForTessa/22R67umbel_23umbel_Cleaned_Clip_Annotations_3-30.csv", row.names = FALSE)


#### CODE GRAVEYARD ######

#test <- pbmeta %>% mutate(time_formatted = as.POSIXct(as.character(survey_site_start_time), format = "%H%M"))
# format date as date and time as time
#pbmeta <- pbmeta %>% mutate(date_formatted=as.Date(date, format="%Y-%m-%d"))

# # Should I format them as date and time columns or as integers????
# test <- pbmeta %>% mutate(time_test = time+2)
# 
# pbmeta$date_int <- str_replace(pbmeta$date,"-","")
# pbmeta$date_int <- as.numeric(str_replace(pbmeta$date_int,"-",""))
# Create formatted date column (yyyy-mm-dd)
#clip_dat$formatted_date <- as.Date(clip_dat$date, format = "%Y%m%d")
# # Define a function to check if a datetime falls within 2 hours of any datetime in pbmeta
# check_within_two_hours <- function(x, pbmeta_datet) {
#   any(pbmeta_datet < x & x <= pbmeta_datet + hours(2))
# }
# check_within_two_hours <- function(x, pbmeta_dates) {
#   any(interval(pbmeta_dates - hours(2), pbmeta_dates + hours(2)) %--% x)
# }

# filtered_clip_dat <- clip_dat %>%
#   filter(!sapply(datetime, check_within_two_hours, pbmeta_datet = pbmeta$datetime))
# for i in nrow(clip_df){
#   # check if it falls within two hours
# }

# filtered_clip_dat <- clip_dat %>% filter(!(site== site_id & (start_clip %within% interval(pbmeta$interval_start,pbmeta$interval_end))))

# # STALE LOOPS
# for (site in sites_list){
#   temp_clip <- small_clipdat %>% filter(site_id == site)
#   temp_pb <- pbmeta %>% filter(site_id == site)
#   # filter the rows that aren't in the playback interval
#   filtered_site_clips <- temp_clip %>% filter(!((start_clip %within% interval(temp_pb$interval_start,temp_pb$interval_end)))) %>% mutate(during_playbacks = ifelse(start_clip %within% interval(temp_pb$interval_start,temp_pb$interval_end),TRUE,FALSE))
#   # not properly registering when the start_clip is within the interval
#   # bind it to dataframe 
#   final_df <- rbind(final_df,filtered_site_clips)
# }
# for (site in sites_list){
#   temp_clip <- small_clipdat %>% filter(site_id == site)
#   temp_pb <- pbmeta %>% filter(site_id == site)
#   # initialize a logical vector to track if start_clip is within any interval
#   is_within <- logical(nrow(temp_clip))
#   # filter the rows that aren't in the playback interval
#   # for each row in temp_clip, check if start_clip is within any interval
#   for (i in 1:nrow(temp_clip)) {
#     is_within[i] <- any(temp_clip$start_clip[i] %within% interval(temp_pb$interval_start, temp_pb$interval_end))
#   }
#   
#   # filter temp_clip based on the logical vector
#   filtered_site_clips <- temp_clip[!is_within, ]
#   # not properly registering when the start_clip is within the interval
#   # bind it to dataframe 
#   final_df <- rbind(final_df,filtered_site_clips)
# }