#### Clean CNN2.0 Clips ###################

## Purpose: to read in the clips sheets and create an output for daily BBCU presence/absence 

# Created 4/9/2024

# Last modified: 5/27/2024

#### Setup #################################
packages <- c("data.table","tidyverse","janitor","chron")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)


##### Read in the clips data for 2023 and 2022 #####
clips_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_4-24.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
# split out the MISO-032 clips
miso_032 <- clips_23 %>% filter(point_id == "MISO-032")
miso_032_red <- miso_032 %>% filter(hour %in% c(10000,70000,90000,230000))
# clip extraction already only looked at the hours within 1000,7000,9000,and 230000
unique(clips_23$hour) # looks good 
# Remove the hours that aren't in the specified values
# test <- clips_23 %>% filter(hour %in% c(10000,70000,90000,230000))
# test2 <- test %>% filter(point_id == "MISO-032")
# test3 <- clips_23 %>% filter(point_id == "MISO-032")
# unique(test3$hour)

# Create site-level presence and absence
clips_new <- clips_23 %>%
  mutate(site_id = case_when(
    # If point_id has four letters then a dash then three numbers
    grepl("^[[:alpha:]]{4}-[[:digit:]]{3}$", point_id) ~ point_id,
    # If point_id has two numbers, three numbers, or three letters then a dash then one number
    grepl("^[[:digit:]]{2,3}-[[:digit:]]{1}$", point_id) |
      grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$", point_id) ~ gsub("-.*", "", point_id)
  ))
# Summarize this
site_det_23 <- clips_new %>% group_by(site_id) %>% summarize(bbcu = max(annotation))
# Number of sites with detections
sum(site_det_23$bbcu)
nrow(site_det_23)
sum(site_det_23$bbcu)
# Write it
#write.csv(site_det_23, "./Data/Classifier_Results/Model2.0/Outputs/SiteLevelBBCU_2023.csv", row.names = FALSE)


clips_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPBYBCU_4-24.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
# Summarize this
site_det_22 <- clips_22 %>% group_by(point_id) %>% summarize(bbcu = max(annotation))
nrow(site_det_22)
sum(site_det_22$bbcu)
# Write it
#write.csv(site_det_22, "./Data/Classifier_Results/Model2.0/Outputs/SiteLevelBBCU_2022.csv", row.names = FALSE)
# read in the locations data
locs <- read.csv("./Data/Monitoring_Points/2023_AllARUPoints_FromDeploymentData.csv")
# filter out by geographic location
# only values that are less than -108.0743650
# only values that are greater than 47.4279918
# xmin_upperMISO <- -108.0743650
# ymin_upperMISO <- 47.4279918
