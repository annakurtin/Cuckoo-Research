##### Look at Average Start Date #####
# The purpose of this script is to look at the start date on the clip extraction files

# Created 2/3/2024
# Last updated 2/3/2024

#### Setup #################################
packages <- c("data.table","tidyverse","janitor")
# Read in the packages function
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
# Load packages
load_packages(packages)


#### Code ########################
r5_23 <- read.csv("F:/Cuckoo_Acoustic_Data/2023/2023_FWPR5_Data/2023_FWPR5_Clips/2023_FWPR5_topclip_perperiod/2023_FWPR5_topclips_perSiteperPeriod.csv")
r5_23 %>% group_by(point_id) %>% summarize(min(date))

r5_22 <- read.csv("F:/Cuckoo_Acoustic_Data/2022/2022_FWPR5_Data/2022_FWPR5_Clips/2022_FWPR5_topclip_perperiod/2022_FWPR5_topclips_perSiteperPeriod.csv")
r5_22 %>% group_by(point_id) %>% summarize(min(date))
# Average is mid july 
r6_22 <- read.csv("F:/Cuckoo_Acoustic_Data/2022/2022_FWPR6_Data/2022_FWPR6_Clips/2022_FWPR6_topclip_perperiod/2022_FWPR6_topclips_perSiteperPeriod.csv")
r6_22 %>% group_by(point_id) %>% summarize(min(date))
# Average is late june (6-27)

r7_22 <- read.csv("F:/Cuckoo_Acoustic_Data/2022/2022_FWPR7_Data/2022_FWPR7_Clips/2022_FWPR7_topclip_perperiod/2022_FWPR7_topclips_perSiteperPeriod.csv")
r7_22 %>% group_by(point_id) %>% summarize(min(date))
# Late june

