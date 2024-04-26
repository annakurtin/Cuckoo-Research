#### Create Detection Histories for Data ###################

## Purpose: to create datasheets to read into camtrapR

# Created 4/26/2023

# Last modified: 4/26/2024

#### Setup #################################
packages <- c("tidyverse","janitor","camtrapR")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Combine_CSV_Files_In_Directory.R")
load_packages(packages)


##### Create start and end columns ####
# In the camtrapR package, these are the deployment and retrieval dates. Since we deployed them before we started to analyze the audio, we'll be masking out the periods where the ARU was deployed and recording but not a part of the study period. I do this by reading in the recording data and taking the first and last date that appear in the clips audio

# Combine the recording period datasheets 
# Combine csvs from the audio_quality files
fwpr5_recper <- combine_csv_files("./Data/ARU_Recording_Periods/2023_FWPR5_Acoustic_Files")
fwpr6_recper <- combine_csv_files("./Data/ARU_Recording_Periods/2023_FWPR6_Acoustic_Files")
fwpr7_recper <- combine_csv_files("./Data/ARU_Recording_Periods/2023_FWPR7_Acoustic_Files")
umbel_recper <- combine_csv_files("./Data/ARU_Recording_Periods/2023_UMBEL_Acoustic_Files")
# combine them into one dataframe
recper_23 <- rbind(fwpr5_recper,fwpr6_recper,fwpr7_recper,umbel_recper) %>% clean_names()
recper_23_used <- recper_23 %>% filter(correct_size == "Y")
unique(recper_23_used$time) # This still has the MISO-032 points in it
# Clean this up a bit
recper_23_used <- recper_23_used %>% separate(file_name, into = c("point_id","date2","time2"), sep = "_", remove = FALSE) %>% select(-c(date2,time2))
# some of the datetimes aren't formatted right?
# 102-5 and MISO-032, MISO-204 formatted 5/26/2023 1:00
# split apart these, format them separately, then recombine
format1 <- recper_23_used %>% filter(point_id %in% c("102-5","MISO-032","MISO-204"))
#format1 <- format1 %>% mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M")) %>%  mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) 
format1$date <- str_extract(format1$file_name, "([[:digit:]]{8})")
format2 <- recper_23_used %>% filter(!(point_id %in% c("102-5","MISO-032","MISO-204")))
#format2 <- format2 %>% mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")) %>%  mutate(datetime = as_datetime(datetime,  tz = "America/Denver"))
format2$date <- str_extract(format2$file_name, "([[:digit:]]{8})")
recper_23_format <- rbind(format1,format2)
#unique(recper_23_format$point_id) looks good 
recper_23_format <- recper_23_format %>% unite(col= point_date, c("point_id","date"), sep = "_", remove = FALSE)
# Change the times in a new column so that the camera trap sampling will evenly account for effort
recper_23_format <- recper_23_format %>% mutate(time_effort = case_when(
  time == 10000 ~ "00:00:00",
  time == 70000 ~ "06:00:00",
  time == 90000 ~ "12:00:00",
  time == 230000 ~ "18:00:00"
))
# make a new datetime column for the effort time
#recper_23_format$time_colon <- sub("(\\d{1,2})(\\d{2})(\\d{2})", "\\1:\\2:\\3", recper_23_format$time_effort)
recper_23_format$combo_effort <- paste0(recper_23_format$date,"-", recper_23_format$time_effort)
recper_23_format$datetime_effort <- as.POSIXct(recper_23_format$combo_effort, format = "%Y%m%d-%H:%M:%S")

# Read in the clips data - this represents all the days that were run through the classifier and are part of our study period
# The monitors were deployed before June 1st, this data we aren't counting
clips_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_4-24.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
clips_23 <- clips_23 %>% unite(col= point_date, c("point_id","date"), sep = "_", remove = FALSE)
# Filter out only the files that were in the classifier runs
recs_withclips <- recper_23_format %>% filter(point_date %in% clips_23$point_date)
# Pull out the first and the last recording periods
recs_first_last <- recs_withclips %>% group_by(point_id) %>% summarize(first_rec = min(datetime_effort, na.rm = TRUE),last_rec = max(datetime_effort, na.rm = TRUE))
# need to separate out the date and time columns
#recs_first_last %>% mutate(date = as.Date(datetime), time = format(datetime, format = "%H:%M:%S"))



##### Create a column for site #### 
## The site is either the part on the left hand side of the point_id or the point_id if it is RIVE-###
deployhist_wsite <- recs_first_last %>%
  mutate(site_id = case_when(
    # If point_id has four letters then a dash then three numbers
    grepl("^[[:alpha:]]{4}-[[:digit:]]{3}$", point_id) ~ point_id,
    # If point_id has two numbers, three numbers, or three letters then a dash then one number
    grepl("^[[:digit:]]{2,3}-[[:digit:]]{1}$", point_id) |
      grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$", point_id) ~ gsub("-.*", "", point_id)
  ))

##### Create a column for year ######
deployhist_wsite <- deployhist_wsite %>% mutate(year = 2023)


#### Write this data for use in camtrapR #####
#write.csv(deployhist_wsite,"./Data/Detection_History/2023_All_ARUs/2023_DeployPeriodTable_CorrectedEffort_4-26.csv",row.names = FALSE)




# Read in clips data from before and create a site ID column
clips_wsite <- clips_23 %>%
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
#write.csv(clips_wsite,"./Data/Detection_History/2023_All_ARUs/2023_BBCUDetections.csv", row.names = FALSE)

saveRDS(clips_wsite,
        file = "./Data/Detection_History/2023_All_ARUs/2023_BBCUDetections.RData")


#for reading them back in:
  
  

##### Code Graveyard ####
#mutate(datetime = format(datetime, tz = "America/Denver"))
#%>% mutate(datetime=as.POSIXct(datetime,tx="UTC"))%>% mutate(datetime=format(datetime,tz="America/Guayaquil")) #"%Y-%m-%d %H:%M:%S"
#recs_first_last <- recper_23_format %>% group_by(point_id) %>% summarize(first_rec = min(datetime),last_rec = max(datetime))
# 
# # Step 1: read in deployment data
# deploy <- read.csv("./Data/Metadata/Outputs/2023_ARUDeployment_MetadataFull_Cleaned10-24.csv")
# # remove the sites that were re deployed
# deploy <- deploy %>% slice(1:155)
# # Checking all the dates deployed: unique(deploy$date_deployed) #deploy %>% filter(date_deployed == "6/2/2023")
# 
# # Combine the start of the recording with the deployment data
# deploy_wstart <- left_join(deploy,recs_first_last)
