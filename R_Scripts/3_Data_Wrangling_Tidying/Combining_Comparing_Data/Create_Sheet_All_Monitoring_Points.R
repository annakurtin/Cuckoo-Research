#### Create a Sheet for ArcGIS with all Monitoring Points #####

# this is a script to read in datasheets for the monitoring points across different years and output one master datasheet that has the locations of all the points 
# Created 6/17/2023

# Last updated 6/17/2023

#### Install and load packages #####
packages <- c("tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)

#### 2023 Data #######
# Read in the actual locations
aru_23 <- read.csv("./Data/Monitoring_Points/2023_AllARUPoints_FromDeploymentData.csv")
# 
pbu_23 <- read.csv("./Data/Monitoring_Points/2023_Playback_Survey_Points_UMBEL.csv")
pbu_23 <- pbu_23 %>% select(point_id, longitude, latitude) %>% rename(long = longitude, lat = latitude)
# Filter out only the ones that are in the veg points 
pbf_23 <- read.csv("./Data/Monitoring_Points/2023_PlaybackPoints_FWP.csv")
pbf_23 <- pbf_23 %>% select(point_id,x,y) %>% rename(long = x, lat = y)

# Read in the veg locations
veg_23 <- read.csv("./Data/Vegetation_Data./Outputs/2023_VegSurvey_MainData_Cleaned5-24.csv")
test <- veg_23 %>% slice(-15) %>% select(point_id,long, lat) %>% mutate(point_id = paste0(point_id,"_veg"))


#### 2022 Data ####

#### 2021 Data ####
