##### Look at Playback Data #######

# This is a script to look at descriptive statistics for playback surveys

# visualize how many and where the bbcu and ybcu detections are
# See how many surveys conducted at each site

# Created 3/25/2024

# Last edited 1/16/2025

##### Setup #######
packages <- c("stringr","tidyverse","janitor","lubridate","ggplot2")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)

#### Code ######
# read in the data
pb_21 <- read.csv("./Data/Playback_Results/2021/Outputs/2021_PlaybackSiteLocationsAvgWithDetections_3-25-24.csv")
pb_21$bbcu_detected <- as.factor(pb_21$bbcu_detected)
pb_21$ybcu_detected <- as.factor(pb_21$ybcu_detected)
pb_22 <- read.csv("./Data/Playback_Results/2022/Outputs/2022_PlaybackSiteLocationsAvgWithDetections_3-25-24.csv")
# Convert detections to factor
pb_22$bbcu_detected <- as.factor(pb_22$bbcu_detected)
pb_22$ybcu_detected <- as.factor(pb_22$ybcu_detected)
pb_23 <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackSiteLocationsAvgWithDetections_3-25-24.csv")
# Convert detections to factor
pb_23$bbcu_detected <- as.factor(pb_23$bbcu_detected)
pb_23$ybcu_detected <- as.factor(pb_23$ybcu_detected)

# Look at number of sites
nrow(pb_21) #35 sites
sum(pb_21$bbcu_detected) # 4 sites with bbcu
sum(pb_21$ybcu_detected) # 0 sites with ybcu

nrow(pb_22) # 40 sites
sum(pb_22$bbcu_detected) # 4 sites with bbcu - 10% naive occ
sum(pb_22$ybcu_detected) #1 site with ybcu - 2.5% naive occ

nrow(pb_23) #33 sites
sum(pb_23$bbcu_detected) #3 sites with bbcu - 9% naive occ
sum(pb_23$ybcu_detected) #3 sites with ybcu - 9% naive occ


# Look at how many surveys conducted each
# read in raw data
surveys_23 <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PBData_FormatforOccMod_8-5-24.csv")
surveys_23 <- surveys_23 %>% rowwise() %>% mutate(nsurvey = sum(!is.na(c_across(starts_with("X")))))
table(surveys_23$nsurvey) # 2 with 1 survey, 2 with 2 surveys, other 29 have all three surveys

# Plot BBCU Detections
pb_21 %>% ggplot(aes(x = long_avg, y = lat_avg, color = bbcu_detected)) +
  geom_point() +
  labs(title = "2021 Playback Detections of BBCU")
pb_22 %>% ggplot(aes(x = long_avg, y = lat_avg, color = bbcu_detected)) +
  geom_point() +
  labs(title = "2022 Playback Detections of BBCU")
pb_23 %>% ggplot(aes(x = long_avg, y = lat_avg, color = bbcu_detected)) +
  geom_point() +
  labs(title = "2023 Playback Detections of BBCU")

# Plot YBCU Detections
pb_22 %>% ggplot(aes(x = long_avg, y = lat_avg, color = ybcu_detected)) +
  geom_point() +
  labs(title = "2022 Playback Detections of YBCU")
pb_23 %>% ggplot(aes(x = long_avg, y = lat_avg, color = ybcu_detected)) +
  geom_point() +
  labs(title = "2023 Playback Detections of YBCU")

