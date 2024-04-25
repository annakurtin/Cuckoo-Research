#### Clean CNN2.0 Clips ###################

## Purpose: to read in the clips sheets and create an output for daily BBCU presence/absence 

# Created 4/9/2024

# Last modified: 4/9/2024

#### Setup #################################
packages <- c("data.table","tidyverse","janitor","chron")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)


##### Read in the clips data for 2023 and 2022 #####
clips_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_4-24.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
# Summarize this
site_det_23 <- clips_23 %>% group_by(point_id) %>% summarize(bbcu = max(annotation))
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
