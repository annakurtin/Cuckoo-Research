####### Multiple Observer Method for Estimating Detection Probability  ########


# A script to read in estimated numbers of sites with cuckoo detections for playbacks and acoustic surveys and create naive estimates of detection probability based on each metho

# TODO
## This 

# Created 4/9/2024

# Last updated 4/9/2024


#### Setup ###############
packages <- c("stringr","tidyverse","janitor","lubridate")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)

#### CODE ##################
# Read in ARU detection data
r5_23 <- read.csv("./Data/Classifier_Results/Model2.0/2023_FWPR5_topclips_perSiteperPeriod_annotations_v2_FINAL_AK_3-22.csv")
r6_23 <- read.csv("./Data/Classifier_Results/Model2.0/2023_FWPR6_topclips_perSiteperPeriod_annotations_v2_FINAL_AK_3-22.csv")
r7_23 <- read.csv("./Data/Classifier_Results/Model2.0/2023_FWPR7_topclips_perSiteperPeriod_annotations_v2_FINAL_AK_3-22.csv")
umbel_23 <- read.csv("./Data/Classifier_Results/Model2.0/2023_UMBEL_topclips_perSiteperPeriod_annotations_v2_AK_3-22.csv")
aru_23 <- rbind(r5_23,r6_23,r7_23,umbel_23)
# Will change this once you've gone through and combined detection for 2023 ************************
aru_points_pos23 <- aru_23%>% filter(!annotation=="u")%>% group_by(point_id) %>% summarize(detection = max(annotation)) %>% filter(detection == 1)
# Removing unknown values for now 
aru_23 <- aru_23 %>% separate(point_id, into = c("site","point_num"),sep = "-", remove = FALSE) %>% filter(!annotation=="u")

aru_23_sum <- aru_23 %>% group_by(site) %>% summarize(bbcu_detected_aru = max(annotation))
aru_23_possite <- aru_23_sum %>% filter(bbcu_detected_aru == 1)

# 2022 data
r5_22 <- read.csv("./Data/Classifier_Results/Model2.0/2022_FWPR5_topclips_perSiteperPeriod_annotations_v2_FINAL_AK3-7.csv")
r6_22 <- read.csv("./Data/Classifier_Results/Model2.0/2022_FWPR6_topclips_perSiteperPeriod_annotations_v2_AK_3-22.csv")
r7_22 <- read.csv("./Data/Classifier_Results/Model2.0/2022_FWPR7_topclips_perSiteperPeriod_annotations_v2_AK_3-28.csv")
umbel_22 <- read.csv("./Data/Classifier_Results/Model2.0/2022_UMBEL_topclips_perSiteperPeriod_annotations_v2_FINAL_AK_4-3.csv")
aru_22 <- rbind(r5_22,r6_22,r7_22,umbel_22)
aru_points_pos22 <- aru_22 %>% filter(!annotation=="u") %>% group_by(point_id) %>% summarize(detection = max(annotation)) %>% filter(detection == 1)
all_aru_points22 <- aru_22 %>% filter(!annotation=="u") %>% group_by(point_id) %>% summarize(detection = max(annotation))

# 2021 data
aru_21 <- read.csv("./Data/Classifier_Results/Model2.0/2021_UMBEL_topclips_perSiteperPeriod_annotations_v2_FINAL_AK_4-3.csv")
aru_points_pos21 <- aru_21 %>% filter(!annotation=="u")%>% group_by(point_id) %>% summarize(detection = max(annotation)) %>% filter(detection == 1)
all_aru_points21 <- aru_21 %>% filter(!annotation=="u") %>% group_by(point_id) %>% summarize(detection = max(annotation))
# Will change this once you've gone through and combined detection for 2023 ************************



# Read in playback data 
pb_23 <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackSurveyMetadataWDetectionsCoords_3-22-24.csv")
pb_22 <- read.csv("./Data/Playback_Results/2022/Outputs/2022_PlaybackSurveyMetadataWDetectionsCoords_3-28-24.csv")
pb_21 <- read.csv("./Data/Playback_Results/2021/Outputs/2021_PlaybackSurveyMetadataWDetectionsCoords_3-25-24.csv")
# separate the playback data into site 
pb_23 <- pb_23 %>% separate(survey_id, into = c("site","survey_num"), sep = "#", remove = FALSE)
pb_23_sum <- pb_23 %>% group_by(site) %>% summarize(bbcu_detected_pb = max(detection_hist_bbcu, na.rm = TRUE))
pb_23_possite <- pb_23_sum %>% filter(bbcu_detected_pb == 1)

pb_22 <- pb_22 %>% separate(survey_id, into = c("site","survey_num"), sep = "#", remove = FALSE)
pb_22_sum <- pb_22 %>% group_by(site) %>% summarize(bbcu_detected_pb = max(detection_hist_bbcu, na.rm = TRUE))
pb_22_possite <- pb_22_sum %>% filter(bbcu_detected_pb == 1)

pb_21 <- pb_21 %>% separate(survey_id, into = c("site","survey_num"), sep = "#", remove = FALSE)
pb_21_sum <- pb_21 %>% group_by(site) %>% summarize(bbcu_detected_pb = max(detection_hist_bbcu, na.rm = TRUE))
pb_21_possite <- pb_21_sum %>% filter(bbcu_detected_pb == 1)


# Get a rough estimate of double observer method - look 1 ######
# There were no playback sites that detected cuckoos where arus didn't- would be good to look at this for the 2022 data as well, since our initial classifier missed sites where the playbacks found cuckoos 
aru_23_possite <- aru_23_possite %>% filter(!site %in% c("MISO","MUSH","YELL"))

# Total sites with cuckoo detections: 10
nrow(aru_23_possite)
# plus WBB which wasn't counted on the ARU 

# Sites detected with playback: 3
# Only WBB unique - had no acoustic monitor 

# Sites detected with aru: 9


## Can we only use sites that had ARU and playbacks for the double observer method??
# Look 2 #######
# 2023 #
# filter out the playback sites that didn't have ARUs at them 
sites_doublemet <- left_join(aru_23_sum,pb_23_sum, by =  "site")
# Filter out the NAs for sites with ARUs and no playbacks
sites_doublemet <- sites_doublemet %>% filter(is.na(bbcu_detected_pb)==FALSE)
# Mutate a new column for present at the site to whether it was detected with either method
# this doesn't take into account within season movement of cuckoos and potential double counting? 
## Could this be one where we just look at the most active period and compare it to playbacks?

# Total sites: 21
nrow(sites_doublemet)

# Sites with presence: 9 
sites_doublemet %>% filter(bbcu_detected_aru == 1)
# Detection probability of ARU is 9/9, 100%

# Sites detected with playback: 2
sites_doublemet %>% filter(bbcu_detected_pb== 1)
# Detection probability of playback is 2/9, 22%

# 2022 #
# filter out the playback sites that didn't have ARUs at them 
sites_doublemet <- left_join(aru_23_sum,pb_23_sum, by =  "site")
# Filter out the NAs for sites with ARUs and no playbacks
sites_doublemet <- sites_doublemet %>% filter(is.na(bbcu_detected_pb)==FALSE)
