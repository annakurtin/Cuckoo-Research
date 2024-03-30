#### Look at CNN2.0 Clips Files Post Vetting ###################

## Purpose: to read in the clips sheets and create an output for daily BBCU presence/absence 

# Created 3/27/2024

# Last modified: 3/27/2024

# # To do:
# Mask out all of the dates that align with the dates of the playback
# Look at 2023 ELI-3 and 2023 PRD-3 in the File_Comments google sheets to determine if this is YBCU or BBCU
# Filter out files in the file_comments google sheet that were playback audio

#### Setup #################################
packages <- c("data.table","tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)


#### Look at Preliminary Results ####
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

#### Read in Final Data #######
r6_23 <- read.csv("./Data/Classifier_Results/Model2.0/2023_FWPR6_topclips_perSiteperPeriod_annotations_v2_FINAL_AK_3-22.csv")
r6_23$date <- as.Date(as.character(r6_23$date),format="%Y%m%d")
unique(r6_23$point_id)
# How I actually want to clean the data
example_date <- 20230703 # 2023, July 3rd
r6_diel1 <- r6_23 %>% group_by(point_id,date, time_period, call_type) %>% summarize(bbcu_detection = max(annotation))
#r6_diel1$date <- as.Date(as.character(r6_diel1$date),format="%Y%m%d")
# look at different combinations of this - group across call types, or paste the call types into a new column? one column for cadence_coo and one column for rattle


# Visualizing results
r6_diel2 <- r6_23 %>% group_by(point_id,date) %>% summarize(bbcu_detection = max(annotation))
# visualize this
r6_diel2 %>% ggplot(aes(x = date,y = bbcu_detection)) + geom_bar(stat= "identity") + labs(title = "2023 BBCU Detections Grouped")

# Look at it across sites
r6_diel2 %>% ggplot(aes(x = date,y = bbcu_detection, color = point_id)) + 
  geom_bar(stat= "identity") + 
  facet_wrap(~ point_id) +
  labs(title = "2023 BBCU Detections Accross Sites")

# Look at individual sites
site <- "CUL-1"
r6_diel2 %>% filter(point_id == site) %>% 
  ggplot(aes(x = date,y = bbcu_detection, fill = point_id)) + 
  geom_bar(stat= "identity") +
  labs(title = paste("Detection History for", site)) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1))#+
scale_x_discrete(breaks=c(20230601,20230615,20230701, 20230715,20230801,20230815))
#r6_diel2 %>% filter(point_id == site, bbcu_detection == 1) %>% select(date, bbcu_detection)

# Summarize the number of dates that have a presence for each point
r6_diel3 <- r6_diel2 %>% group_by(point_id) %>% summarize(dates_bbcu_detected = sum(bbcu_detection))
r6_diel3 %>% ggplot(x = dates_bbcu_detected) +geom_histogram()
hist(r6_diel3$dates_bbcu_detected,xlab = "Number of Days with BBCU Detection", ylab = "Number of Points", main = "2023 Region 6")







##### Code Graveyard #####
# This combines all the MISO habitat points
# test <- r6_diel2 %>% separate(point_id, into = c("site","point"), sep = "-")
# unique(test$site)
# r6_diel3 <- r6_diel2 %>% separate(point_id, into = c("site","point"), sep = "-") %>% group_by(site) %>% summarize(bbcu_present = max(bbcu_present))
# unique(r6_diel3$site)
# Look at it summarized accross sites
# r6_diel3 <- r6_diel2 %>% group_by(point_id) %>% summarize(bbcu_present = max(bbcu_present))
# unique(r6_diel3$site)
# # visualize this
# r6_diel3 %>% ggplot(aes(x = date,y = bbcu_present, color = point_id)) + geom_bar(stat= "identity")