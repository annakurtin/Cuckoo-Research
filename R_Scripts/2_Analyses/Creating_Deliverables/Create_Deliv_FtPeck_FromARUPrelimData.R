#### Create Deliverables from ARU Detections for Ft Peck ###################

## Purpose: to read in thepreliminary ARU vetting files for region 6 and create a csv that can be pulled into am ap  

# Created 3/14/2023

# Last modified: 3/14/2023



#### Setup #################################
packages <- c("data.table","tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)

#### Read in Data ####

# read in aru prelim vetting data
r6_prelim <- read.csv("./Data/Classifier_Results/2023_FWPR6_topclips_perSiteperPeriod_annotations2-16FINAL.csv") %>% clean_names()

# Read in monitoring point location data
aru_points <- read.csv("./Data/Monitoring_Points/2023_AllARUPoints_FromDeploymentData.csv")

# we're ignoring U for now, remove these and just focus on the 1 and 0
# remove u from the data with filter
r6_prelim <- r6_prelim %>% filter(!annotation == "u")
# test
unique(r6_prelim$annotation)
#looks good

# convert annotation to numeric
r6_prelim$annotation <- as.numeric(r6_prelim$annotation)

# summarize by point_id, taking the max of annotation
r6_sum <- r6_prelim %>% group_by(point_id) %>% summarize(bbcu_present = max(annotation))
# Looks good

# create a new column for whether or not the point is on the ft peck reservation 
r6_sum <- r6_sum %>% mutate(ft_peck = ifelse(point_id %in% c("MISO-069","MISO-086","MISO-150","MISO-202","MISO-009"),"Y","N"))

# bind with long and lat for arcgis 
r6_prelim_det <- left_join(r6_sum,aru_points, by = "point_id")

# Export this for use in ArcGIS
write.csv(r6_prelim_det, "./Data/Cuckoo_Presence_Absence_ARU/Model_2.0/2023_FWPR6_PrelimARUDetections.csv",row.names = FALSE)

#### Code graveyard #####
# result <- r6_prelim %>%
#   group_by(point_id) %>%
#   summarize(unique_annotations = toString(unique(annotation)))
# 
# result2 <- result %>%
#   # Split the unique_annotations into separate columns
#   separate(unique_annotations, into = c("annotation_0", "annotation_1", "annotation_u"), sep = ", ") %>%
#   # Fill missing values with NA
#   mutate(across(starts_with("annotation"), ~na_if(., "")))
# 
# mutate(MonthNum = case_when(
#   Month == "January" ~ 1,

# survey location data 