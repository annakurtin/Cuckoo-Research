#### Filter Out YBCU Audio From CNN2.0 Clips ###################

## Purpose: to read in the clips sheets and filter out calls that are YBCU

# Created 4/9/2024

# Last modified: 4/9/2024

#### Setup #################################
packages <- c("data.table","tidyverse","janitor","chron")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)


#### Filter out YBCU detections ####
# Read in data comments
com <- read.csv("./Data/Classifier_Results/Model2.0/Clip_Annotation_Major_Comments_3-28.csv")  %>% clean_names()
com <- com %>% filter(tag == "YBCU")
# write this to a .csv for pittsburgh
#write.csv(com, "./Data/Classifier_Results/Model2.0/Clip_Annotation_YBCU_Files_3-28.csv", row.names = FALSE)
# method for doing this?
# Assign a new column for spaced_coo that is 1 if the file name is in the potential YBCU files from the google drive
# OR mutate call_type to spaced_coo if the file name is in the potential YBCU files from the google drive 
# Checking the files from the google drive: check that they meet the pattern SITE-#/YYYMMDD_HMMSS_SSS.0s-SSS.0s_call_type.wav

# visualize these for each site with date on the x axis, number (sum of annotation?) on the y axis and color corresponding to type of call 

