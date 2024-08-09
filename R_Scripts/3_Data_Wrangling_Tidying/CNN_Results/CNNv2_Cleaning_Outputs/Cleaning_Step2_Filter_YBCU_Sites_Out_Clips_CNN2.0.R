#### Filter Out YBCU Audio From CNN2.0 Clips ###################

## Purpose: to read in the clips sheets and filter out calls that are YBCU

# Created 4/24/2023
# Last modified 5/27/2023

#### Setup #################################
packages <- c("data.table","tidyverse","janitor","chron")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_SiteColumn_fromPointID.R")
load_packages(packages)

### Read in data #####
clips_21 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2021_AllCollab_topclips_filteredPB_4-12.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver"))
clips_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/ClipData_NotScreenedYBCU/2022_AllCollab_topclips_filteredPB_4-12.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver"))
clips_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/ClipData_NotScreenedYBCU/2023_AllCollab_topclips_filteredPB_4-12.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver"))

# Read in which sites were confirmed as YBCU from google drive
ybcu_sites <- read.csv("./Data/Classifier_Results/Model2.0/Raw_Data/Screen_Out_YBCU.csv") %>% clean_names()
ybcu_sites <- ybcu_sites %>% filter(decision == "YBCU") %>% separate(point_year, into = c("point_id","year"), sep = "_", remove = FALSE)


# Clean the 2023 data
ybcu_23 <- ybcu_sites %>% filter(year == 2023)
# Pull out the ybcu site clips
subset_wybcu_23 <- clips_23 %>% filter(point_id %in% ybcu_23$point_id)
# Change the 1's in the annotation to YBCU
ybcu_clips_23  <- subset_wybcu_23  %>% rename(ybcu = annotation)
# Write this to csv for future use
#write.csv(ybcu_clips_23,"./Data/Classifier_Results/Model2.0/Outputs/2023_YBCU_Clips_4-24.csv", row.names = FALSE)
# Change all the annotations that are 1 to zero 
cleaned_clips_23 <- subset_wybcu_23 %>% mutate(annotation = case_when(
  annotation == 0 ~0, 
  annotation == 1 ~0
))
#unique(cleaned_clips_23$annotation)
# Looks good
nonybcu_23 <- clips_23 %>% filter(!(point_id %in% ybcu_23$point_id))
unique(other1$point_id)
# Combine these together again
screened_23 <- rbind(nonybcu_23,cleaned_clips_23)
# Add on a site_id column
screened_23_fin <- screened_23 %>% create_site_col()
# Write this data with the correct site ID to the outputs folder
#write.csv(screened_23_fin, "./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_5-27.csv", row.names = FALSE)
# Double check that this is the same as the one you cleaned from the create detection history script
#test <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_5-27_redone.csv")
# Looks good


# Clean the 2022 data
ybcu_22 <- ybcu_sites %>% filter(year == 2022)
# Pull out the ybcu site clips
subset_wybcu_22 <- clips_22 %>% filter(point_id %in% ybcu_22$point_id)
# Change the 1's in the annotation to YBCU
ybcu_clips_22  <- subset_wybcu_22  %>% rename(ybcu = annotation)
# Write this to csv for future use
#write.csv(ybcu_clips_22,"./Data/Classifier_Results/Model2.0/Outputs/2022_YBCU_Clips_4-24.csv", row.names = FALSE)
# Change all the annotations that are 1 to zero 
cleaned_clips_22 <- subset_wybcu_22 %>% mutate(annotation = case_when(
  annotation == 0 ~0, 
  annotation == 1 ~0
))
#unique(cleaned_clips_22$annotation)
# Looks good
nonybcu_22 <- clips_22 %>% filter(!(point_id %in% ybcu_22$point_id))
# Combine these together again
screened_22 <- rbind(nonybcu_22,cleaned_clips_22)
screened_22_fin <- screened_22  %>% create_site_col()
#write.csv(screened_22_fin,"./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPBYBCU_5-27.csv", row.names = FALSE)


# 2021 Data
## Don't need to do anything for this dataset other than to create a site ID
clips_21_fin <- clips_21  %>% create_site_col()
#write.csv(clips_21_fin,"./Data/Classifier_Results/Model2.0/Outputs/2021_AllCollab_topclips_filteredPBYBCU_5-27.csv", row.names = FALSE)
