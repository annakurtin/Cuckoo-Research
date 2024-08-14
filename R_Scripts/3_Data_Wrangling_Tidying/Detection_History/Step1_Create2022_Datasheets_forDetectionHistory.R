# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create Detection Histories for Data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Purpose: to create datasheets to read into camtrapR

# Copied from 2023 script 8/14/2024

# Last modified: 8/14/2024

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Setup #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(janitor)
library(camtrapR)
source("./R_Scripts/6_Function_Scripts/Combine_CSV_Files_In_Directory.R")
source("./R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create Start and End Columns #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# In the camtrapR package, these are the deployment and retrieval dates. Since we deployed them before we started to analyze the audio, we'll be masking out the periods where the ARU was deployed and recording but not a part of the study period. I do this by reading in the recording data and taking the first and last date that appear in the clips audio


# looking for problematic files in the 2022 data:
# 101-2 has a blip in it - files after this look fine though, kept in 
# JUD-2 drops off after 7/22 and has one file of correct size - there is some weird noise in this file but looks fine, can either leave it in or discount this day from it (MAKE PROBLEM PERIOD)
# SIP-1 has a blip in it - after 8/15, fine to leave in bc will be masked out by clips
# ELI-3 looks like it had issues and just one day of recordings - first two are fine and third file is corrupted (could probably just leave this one out) (MAKE PROBLEM PERIOD)
# HAM-2 had issues after 8-13 (MAKE PROBLEM PERIOD from 8/14 to 8/15)
# SWE-3 looks super bad and corrupted for the first part - seems fine once the audio files get back to normal size 

# Make problem periods: 
## JUD-2 from 7/22 to 7/23
## ELI-3 from 6/24/2023 - 6/25/2023
## HAM-2: from 8/14/2023-8/15/2023


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in datasheets and  combine ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read in the problematic periods file
prob_period <- read.csv("./Data/Detection_History/2022_All_ARUs/Raw_Data/List_Problematic_ARU_Files_2022_Start_Stop_ofIssues.csv") 
prob_period <- prob_period %>% mutate(Problem1_from = as_datetime(Problem1_from,tz = "America/Denver"),
                                      Problem1_to = as_datetime(Problem1_to,tz = "America/Denver"))



# Combine the recording period datasheets 
# Combine csvs from the audio_quality files
fwpr5_recper <- combine_csv_files("./Data/ARU_Recording_Periods/2022_FWPR5_Acoustic_Files")
fwpr6_recper <- combine_csv_files("./Data/ARU_Recording_Periods/2022_FWPR6_Acoustic_Files")
fwpr7_recper <- combine_csv_files("./Data/ARU_Recording_Periods/2022_FWPR7_Acoustic_Files")
umbel_recper <- combine_csv_files("./Data/ARU_Recording_Periods/2022_UMBEL_Acoustic_Files")
# combine them into one dataframe
recper_22 <- rbind(fwpr5_recper,fwpr6_recper,fwpr7_recper,umbel_recper) %>% clean_names()
recper_22_used <- recper_22 %>% filter(correct_size == "Y")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# check on correct formatting of datetimes ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unique(recper_22_used$time) # looks good
recper_22_used <- recper_22_used %>% separate(file_name, into = c("point_id","date2","time2"), sep = "_", remove = FALSE) %>% select(-c(date2,time2))
unique(recper_22_used$datetime) # looks good
unique(recper_22_used$point_id) # looks good 
recper_22_format <- recper_22_used 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# edit times for effort measurement ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Change the times in a new column so that the camera trap sampling will evenly account for effort
recper_22_format <- recper_22_format %>% mutate(time_effort = case_when(
  time == 10000 ~ "00:00:00",
  time == 70000 ~ "06:00:00",
  time == 90000 ~ "12:00:00",
  time == 230000 ~ "18:00:00"
))

recper_22_format <- recper_22_format %>% rename(date_orig = date)
recper_22_format$date_orig <- as.character(recper_22_format$date_orig)
recper_22_format$date_str <- paste0("0", recper_22_format$date_orig)
recper_22_format <- recper_22_format %>% mutate(date_f = as.Date(strptime(date_str, format = "%m%d%Y")))
# make a new datetime column for the effort time
recper_22_format$combo_effort <- paste0(recper_22_format$date_f," ", recper_22_format$time_effort)
recper_22_format$datetime_effort <- as.POSIXct(recper_22_format$combo_effort)
# Format date column for combining with point for combining with clips
recper_22_format <- recper_22_format %>%  mutate(
  date_tocomb = format(as.Date(date_str, format = "%m%d%Y"), "%Y%m%d")
)
recper_22_format <- recper_22_format %>% unite(col = point_date, c("point_id","date_tocomb"), sep = "_", remove = FALSE)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create sheet that doesn't have clips masked out ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a sheet that has the absolute first and last recording
#recsabsolute_first_last <- recper_22_format %>% group_by(point_id) %>% summarize(first_rec = min(datetime_effort, na.rm = TRUE),last_rec = max(datetime_effort, na.rm = TRUE))
# Group by point ID
#recs_firstlast_abs <- create_site_col(recsabsolute_first_last)
#recs_firstlast_abs <- recs_firstlast_abs %>% mutate(year = 2022)
#deploy_table_1 <- left_join(recs_firstlast_abs,prob_period, by = "point_id")
# write this for use
#write.csv(deploy_table_1,"./Data/Detection_History/2022_All_ARUs/2022_DeployPeriodTable_CorrectedEffort_NotClipMasked_8-14.csv",row.names = FALSE)
#saveRDS(deploy_table_1,file = "./Data/Detection_History/2023_All_ARUs/2023_DeployPeriodTable_CorrectedEffort_NotClipMasked_4-29.RData")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create sheet that does have clips masked out ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mask another datasheet to clips data
# Read in the clips data - this represents all the days that were run through the classifier and are part of our study period
# The monitors were deployed before June 1st, this data we aren't counting
clips_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPBYBCU_5-27.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
clips_22 <- clips_22 %>% unite(col= point_date, c("point_id","date"), sep = "_", remove = FALSE)
# Filter out only the files that were in the classifier runs
recs_withclips <- recper_22_format %>% filter(point_date %in% clips_22$point_date)
#unique(recs_withclips$date_tocomb)
# Pull out the first and the last recording periods
#recs_first_last <- recs_withclips %>% group_by(point_id) %>% summarize(first_rec = min(datetime_effort, na.rm = TRUE),last_rec = max(datetime_effort, na.rm = TRUE))
# Pull out just the first day in the survey period
recs_first <- recs_withclips %>% group_by(point_id) %>% summarize(first_rec = min(datetime_effort, na.rm = TRUE))
# Pull out just the last day in the survey period 
recs_last <- recs_withclips %>% group_by(point_id) %>% summarize(last_rec = max(datetime_effort, na.rm = TRUE))
# Change the datetime to reflect the fact that we have a full day of data here
recs_last <- recs_last %>% 
  mutate(last_rec = case_when(
    hour(last_rec) == 18 & month(last_rec) == 8 & mday(last_rec) == 15 ~ {
      new_date <- as.Date("2023-08-16")
      as.POSIXct(paste(new_date, format(new_date, "%H:%M:%S"), " 00:00:00"))
    },
    TRUE ~ last_rec 
    ))
# need to separate out the date and time columns
#recs_first_last %>% mutate(date = as.Date(datetime), time = format(datetime, format = "%H:%M:%S"))
#deployhist_wsite <- create_site_col(recs_first_last)
# Create a column for year
#deployhist_wsite <- deployhist_wsite %>% mutate(year = 2022)
#deploy_table_2 <- left_join(deployhist_wsite,prob_period, by = "point_id")

# Write this data for use in camtrapR #
#write.csv(deployhist_wsite,"./Data/Detection_History/2022_All_ARUs/2022_DeployPeriodTable_CorrectedEffort_8-14.csv",row.names = FALSE)


# Join together the data you've pulled out
deploy_table3 <- left_join(recs_last,recs_first, by = "point_id")
#deploy_table3_test <- left_join(recs_first,recs_last, by = "point_id")
# add in the problem periods
deploy_table3_fin <- left_join(deploy_table3,prob_period, by = "point_id")
# Create a column for site ID and year
deploy_table3_fin <- create_site_col(deploy_table3_fin)
deploy_table3_fin <- deploy_table3_fin %>% mutate(year = 2022)
# save this
saveRDS(deploy_table3_fin,file = "./Data/Detection_History/2022_All_ARUs/Outputs/2022_DeployPeriodTable_CorrectedEffort_ClipMaskedFinalRec_8-14.RData")




###### Read in clips data from before and create a site ID column #######################
clips_wsite <- clips_22 %>%
  mutate(site_id = case_when(
    # If point_id has four letters then a dash then three numbers
    grepl("^[[:alpha:]]{4}-[[:digit:]]{3}$", point_id) ~ point_id,
    # If point_id has two numbers, three numbers, or three letters then a dash then one number
    grepl("^[[:digit:]]{2,3}-[[:digit:]]{1}$", point_id) |
      grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$", point_id) ~ gsub("-.*", "", point_id)
  ))
# Filter out just the detections and create a species column
clips_wsite <- clips_wsite %>% filter(annotation == 1) %>% mutate(species = case_when(annotation == 1 ~ 'bbcu'))
#unique(clips_wsite$species) # looks good

# write this to a csv for use
write.csv(clips_wsite,"./Data/Detection_History/2022_All_ARUs/Outputs/2022_BBCUDetections.csv", row.names = FALSE)

# Write this to an .rdata file for easier use
saveRDS(clips_wsite,file = "./Data/Detection_History/2022_All_ARUs/Outputs/2022_BBCUDetections.RData")


  



  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Code Graveyard ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # some of the datetimes aren't formatted right?
# # 102-5 and MISO-032, MISO-204 formatted 5/26/2023 1:00
# # split apart these, format them separately, then recombine
# format1 <- recper_23_used %>% filter(point_id %in% c("102-5","MISO-032","MISO-204"))
# #format1 <- format1 %>% mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M")) %>%  mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) 
# format1$date <- str_extract(format1$file_name, "([[:digit:]]{8})")
# format2 <- recper_23_used %>% filter(!(point_id %in% c("102-5","MISO-032","MISO-204")))
# #format2 <- format2 %>% mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")) %>%  mutate(datetime = as_datetime(datetime,  tz = "America/Denver"))
# format2$date <- str_extract(format2$file_name, "([[:digit:]]{8})")
# recper_23_format <- rbind(format1,format2)

# Pull out just the first date from the recording periods
#recs_first <- recper_22_format %>% group_by(point_id) %>% summarize(first_rec = min(datetime_effort, na.rm = TRUE))