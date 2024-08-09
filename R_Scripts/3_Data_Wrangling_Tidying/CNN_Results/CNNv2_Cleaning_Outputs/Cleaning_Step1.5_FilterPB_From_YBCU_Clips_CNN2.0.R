#### Filter Out Playbacks from YBCU Audio ###################

## Purpose: to read in the YBCU clips from the google sheets and filter out the ones that occur during playbacks

# Created 4/9/2024

# Last modified: 4/12/2024

#TODO
# Look at 2023 ELI-3 and 2023 PRD-3 in the File_Comments google sheets to determine if this is YBCU or BBCU

#### Setup #################################
packages <- c("data.table","tidyverse","janitor","chron")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)


#### Filter out YBCU detections ####
# Read in data comments
com <- read.csv("./Data/Classifier_Results/Model2.0/Raw_Data/Clip_Annotation_Major_Comments_4-12.csv")  %>% clean_names()
com <- com %>% filter(tag == "YBCU")
# Separate out the clip_id column
ybcu_clips <- com %>%
  separate(clip_id, into = c("point_id", "date", "hour","time_chunk"), sep = "[/_]", remove = FALSE)
ybcu_clips <- ybcu_clips %>%
  separate(time_chunk, into = c("start_time","other"), sep = "[.s-]", remove = FALSE) %>% select(-other)

# need these columns: date, hour, site
create_formatted_datetime <- function(clip_dat){
  # Format
  clip_dat$date <- as.character(clip_dat$date)
  clip_dat$hour <- as.numeric(clip_dat$hour)
  clip_dat$time <- sprintf("%06d", clip_dat$hour) 
  # Create a new column for date formatted as a date
  clip_dat$date_formatted <- as.Date(clip_dat$date, format = "%Y%m%d")
  # Create formatted time column (hh:mm:ss)
  clip_dat$time_formatted <- format(strptime(clip_dat$time, format = "%H%M%S"), format = "%H:%M:%S")
  
  # Create formatted datetime column (yyyy-mm-dd hh:mm:ss)
  clip_dat$datetime <- as.POSIXct(paste(clip_dat$date, clip_dat$time), format = "%Y%m%d %H%M%S")
  clip_dat$datetime <- as.POSIXct(clip_dat$datetime, tz = "UTC")  # Set initial timezone to "UTC"
  clip_dat$datetime <- with_tz(clip_dat$datetime, "America/Denver")  # Convert to UTC-6 (MDT)
  # Convert start_time to numeric 
  clip_dat$start_time <- as.numeric(clip_dat$start_time)
  clip_dat$start_clip <- clip_dat$datetime + clip_dat$start_time
  new_clip_dat <- clip_dat %>% separate(point_id, into= c("site_id","point_num"),sep = "-", remove = FALSE)
  return(new_clip_dat)
}

# create a formatted datetime for the ybcu clips
ybcu_clips_form <- create_formatted_datetime(ybcu_clips)

# read in playback metadata
pbmeta <- read.csv("./Data/Playback_Results/AllYears_PlaybackSurveyMetadataWIntervals4Cleaning_4-12.csv" ) %>% clean_names()
pbmeta$interval_start <- as_datetime(pbmeta$interval_start, tz = "America/Denver")
pbmeta$interval_end <- as_datetime(pbmeta$interval_end, tz = "America/Denver")

# Filter out the YBCU clips that are within the playback periods
filter_out_pb <- function(clip_dat,pbmeta){
  
  sites_list <- unique(clip_dat$site)
  # initialize an empty dataframe
  final_df <- data.frame()
  # for each site id
  for (site in sites_list){
    temp_clip <- clip_dat %>% filter(site_id == site)
    temp_pb <- pbmeta %>% filter(site_id == site)
    # filter the rows that aren't in the playback interval
    # for each row in temp_clip, check if start_clip is within any interval
    for (i in 1:nrow(temp_clip)) {
      temp_clip$during_playbacks[i] <- any(temp_clip$start_clip[i] %within% interval(temp_pb$interval_start, temp_pb$interval_end))
    }
    # filter temp_clip based on the logical vector
    filtered_site_clips <- temp_clip %>% filter(during_playbacks == FALSE)
    # not properly registering when the start_clip is within the interval
    # bind it to dataframe 
    final_df <- rbind(final_df,filtered_site_clips)
  }
  return(final_df)
  
}

ybcu_nopb <- filter_out_pb(ybcu_clips_form,pbmeta)
#test <- ybcu_nopb %>% select(point_id,site_id,start_clip)

# Identify which clips are only in the original clips data
removed_from_final <- ybcu_clips_form %>%
  anti_join(ybcu_nopb, by = "clip_id")
# Looks good

# write this to a .csv for use
#write.csv(ybcu_nopb, "./Data/Classifier_Results/Model2.0/Outputs/Clip_Annotation_YBCU_Files_4-12.csv", row.names = FALSE)

