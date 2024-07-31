#### Planning Modeling for Methods Chapter


### How many sites would we have if we used ARU data from 2021-2023?

source("./R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")


lidar_dat <- read.csv("./Data/Vegetation_Data/Outputs/AllPoints_AllScales_LiDARMetrics_7-23-24.csv")

# split alt_point_id into point_id and tag
dat <- lidar_dat %>% separate(alt_point_id, into = c("point_id","tag"), sep = "_")
# filter sites that are in the ARU data
aru_21 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2021_AllCollab_topclips_filteredPBYBCU_5-27.csv")
sites_21 <- unique(aru_21$site_id)
aru_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPBYBCU_5-27.csv")
sites_22 <- unique(aru_22$site_id)
aru_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_5-27.csv")
sites_23 <- unique(aru_23$site_id)
# Remove sites from 2023 that I'm not looking at for this chapter
pattern <- "^[A-Za-z]{4}-\\d{3}$"
sites_23_new <- sites_23[!grepl(pattern, sites_23)]

# How many total sites?
all_aru <- c(sites_21, sites_22, sites_23_new) # 81 sites



### How many sites would we have if we did playback surveys?
# 33

pb <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackData_3-25-24.csv")
pb <- pb %>% group_by(survey_id) %>% summarize(bbcu = max(bbcu_detection, na.rm = TRUE))
pb <- pb %>% separate(survey_id, into = c("site_id","survey_num"), sep = "#")
pb <- pb %>% pivot_wider(names_from = survey_num, values_from = bbcu)
# Why so many NAs?

meta_pb <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackMetadata_3-22-24.csv")
meta_pb <- meta_pb %>% separate(survey_id, into = c("site_id","survey_num"), sep = "#")
test <- meta_pb %>% pivot_wider(names_from = survey_num, values_from = c(observer, 
                                                                         num_surveyors, 
                                                                         date, 
                                                                         survey_site_start_time, 
                                                                         sky_start, 
                                                                         wind_start, 
                                                                         temp_start))
# again, this seems like a lot of NAs in this data??? What's going on with pivor longer


# First fit just detection model, then try to fit full occupancy model using covariates from your habitat chapter
# Will we even have enough sites for something like this???
