#### Clean CNN Clips Files Post Vetting ###################

## Purpose: to read in the clips sheets and create an output for daily BBCU presence/absence 

# Created 3/21/2023

# Last modified: 3/21/2023



#### Setup #################################
packages <- c("data.table","tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)

#### Read in Data ####
# Get a naive estimate of occupancy at all the sites

# read in aru prelim vetting data
r5_prelim <- read.csv("./Data/Classifier_Results/Model2.0_Prelim_Results_FromDrive/2023_FWPR5_topclips_perSiteperPeriod_annotations_v2_AK_3-20.csv") %>% clean_names()
r6_prelim <- read.csv("./Data/Classifier_Results/Model2.0_Prelim_Results_FromDrive/2023_FWPR6_topclips_perSiteperPeriod_annotations_v2_AK_3-20.csv") %>% clean_names()
r7_prelim <- read.csv("./Data/Classifier_Results/Model2.0_Prelim_Results_FromDrive/2023_FWPR7_topclips_perSiteperPeriod_annotations_v2_AK_3-21.csv") %>% clean_names()
umbel_prelim <- read.csv("./Data/Classifier_Results/Model2.0_Prelim_Results_FromDrive/2023_UMBEL_topclips_perSiteperPeriod_annotations_FINAL_AK3-6.csv") %>% clean_names()
# Remove u annotations (won't be necessary once all are properly vetted)
r5_prelim <- r5_prelim %>% filter(!annotation == "u")
r6_prelim <- r6_prelim %>% filter(!annotation == "u")
r7_prelim <- r7_prelim %>% filter(!annotation == "u")
umbel_prelim <- umbel_prelim %>% filter(!annotation == "u")
# Going to estimate naive occupancy accross all sites not just habitat?
prelim_habitat <- rbind(r5_prelim,r6_prelim,r7_prelim,umbel_prelim)
# Filter out jsut the habitat points (those that match MMMM-###)
# Regular expression pattern to match "MISO-###" format
pattern <- "^[A-Z]{4}-\\d{3}$"

# Filter using dplyr and stringr
prelim_habitat <- prelim_habitat %>%
  filter(str_detect(point_id, pattern))

unique(filtered_df$point_id) 
# looks good


# convert annotation to numeric
prelim_habitat$annotation <- as.numeric(prelim_habitat$annotation)

# summarize by point_id, taking the max of annotation
prelim_sum <- prelim_habitat %>% group_by(point_id) %>% summarize(bbcu_present = max(annotation))

present <- prelim_sum %>% filter(bbcu_present == 1)
18/85
# 21% naive occupancy 


# How I actually want to clean the data
r6_diel_per <- r6_prelim %>% group_by(point_id,date, time_period, call_type) %>% summarize(bbcu_present = max(annotation))
