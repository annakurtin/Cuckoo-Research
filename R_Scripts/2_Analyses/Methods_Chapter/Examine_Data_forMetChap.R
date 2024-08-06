#### Planning Modeling for Methods Chapter

# I am formatting the data for the playback survey detection submodel. As of 8/5/2024, this is a more rough and ready version. I haven't gone through with a fine tooth comb to decide which data I'm including and why (start time outside of survey period, didn't record start temp but recorded end temp, etc). If the playback model does converge, I'll go back and look at this in finer detail. 

### How many sites would we have if we used ARU data from 2021-2023?
library(tidyverse)
library(hms)
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
# Remove the survey 1.5 - I'm not sure how to deal with this yet
pb <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackData_3-25-24.csv")
pb <- pb %>% group_by(survey_id) %>% summarize(bbcu = max(bbcu_detection, na.rm = TRUE))
pb <- pb %>% separate(survey_id, into = c("site_id","survey_num"), sep = "#")
pb_filtered <- pb %>% filter(!survey_num == "1.5")
pb_f <- pb_filtered %>% pivot_wider(names_from = survey_num, values_from = bbcu)



meta_pb <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackMetadata_3-22-24.csv")
meta_pb <- meta_pb %>% separate(survey_id, into = c("site_id","survey_num"), sep = "#", remove = FALSE)
unique(meta_pb$survey_num)
meta_pb_filtered <- meta_pb %>% filter(!survey_num =="1.5")
# Detection covariates to look at: number of surveyors, date, start time, sky, wind, temp
meta_toformat <- meta_pb_filtered %>% select(site_id,
                                    survey_num,
                                    num_surveyors,
                                    date,
                                    survey_site_start_time,
                                    sky_start,
                                    wind_start,
                                    temp_start)
meta_toformat <- meta_toformat %>% rename(num_obs = num_surveyors,
                                          starttime = survey_site_start_time,
                                          sky = sky_start,
                                          wind = wind_start,
                                          temp = temp_start)
meta_f <- meta_toformat %>% pivot_wider(names_from = survey_num, 
                                values_from = c(num_obs,
                                                date,
                                                starttime,
                                                sky,
                                                wind,
                                                temp), 
                                names_sep = "_s")

# Combine detection and detection covariates data
pb_dat_comb <- left_join(pb_f, meta_f, by = "site_id")
# reformat
pb_dat_comb <- pb_dat_comb %>% mutate(date_s1 = as.Date(date_s1, format="%m/%d/%Y"),
                       date_s2 = as.Date(date_s2, format="%m/%d/%Y"),
                       date_s3 = as.Date(date_s3, format="%m/%d/%Y"))
# convert to Julian date
pb_dat_comb$date_s1 <- yday(pb_dat_comb$date_s1)
pb_dat_comb$date_s2 <- yday(pb_dat_comb$date_s2)
pb_dat_comb$date_s3 <- yday(pb_dat_comb$date_s3)

# Definitely a better way to do this but it'll work

pb_dat_comb <- pb_dat_comb %>% separate(starttime_s1, into = c("hr1","min1"), sep = ":") 
pb_dat_comb$hr1 <- as.numeric(pb_dat_comb$hr1)
pb_dat_comb$min1 <- as.numeric(pb_dat_comb$min1)
pb_dat_comb$hr_tomin1 <- pb_dat_comb$hr1 * 60
pb_dat_comb$time_s1 <- pb_dat_comb$hr_tomin1 + pb_dat_comb$min1
pb_dat_comb <- pb_dat_comb %>% select(-c(hr1,min1,hr_tomin1))

pb_dat_comb <- pb_dat_comb %>% separate(starttime_s2, into = c("hr2","min2"), sep = ":") 
pb_dat_comb$hr2 <- as.numeric(pb_dat_comb$hr2)
pb_dat_comb$min2 <- as.numeric(pb_dat_comb$min2)
pb_dat_comb$hr_tomin2 <- pb_dat_comb$hr2 * 60
pb_dat_comb$time_s2 <- pb_dat_comb$hr_tomin2 + pb_dat_comb$min2
pb_dat_comb <- pb_dat_comb %>% select(-c(hr2,min2,hr_tomin2))

pb_dat_comb <- pb_dat_comb %>% separate(starttime_s3, into = c("hr3","min3"), sep = ":") 
pb_dat_comb$hr3 <- as.numeric(pb_dat_comb$hr3)
pb_dat_comb$min3 <- as.numeric(pb_dat_comb$min3)
pb_dat_comb$hr_tomin3 <- pb_dat_comb$hr3 * 60
pb_dat_comb$time_s3 <- pb_dat_comb$hr_tomin3 + pb_dat_comb$min3
pb_dat_comb <- pb_dat_comb %>% select(-c(hr3,min3,hr_tomin3))

# make final datasheet of scaled covariates
pb_dat_f <- pb_dat_comb
# scale the covariates - observers, date, wind, temp
pb_dat_f[5:10] <- round(scale(pb_dat_comb[5:10]), 2)
pb_dat_f[14:22] <- round(scale(pb_dat_comb[14:22]), 2)


# Select the order of the final columns
pb_dat_f <- pb_dat_f %>% rename(s1 = "1", s2 = "2", s3 = "3")
pb_dat_fin <- pb_dat_f %>% select(site_id,
                                   s1, s2, s3,
                                   num_obs_s1, num_obs_s2, num_obs_s3,
                                   date_s1, date_s2, date_s3, 
                                   time_s1, time_s2, time_s3,
                                   temp_s1, temp_s2, temp_s3,
                                   wind_s1, wind_s2, wind_s3,
                                   sky_s1, sky_s2, sky_s3)
# Write this 
#write.csv(pb_dat_fin, "./Data/Playback_Results/2023/Outputs/2023_PBData_FormatforOccModSCALED_8-6-24.csv", row.names = FALSE)

# Look at distributions
hist(pb_dat_comb$num_obs_s1)
hist(pb_dat_fin$num_obs_s1)

hist(pb_dat_comb$date_s2)
hist(pb_dat_fin$date_s2)

hist(pb_dat_comb$date_s3, breaks = 7)
hist(pb_dat_fin$date_s3, breaks = 7)
# I think these look ok, I think scaling them is slightly changing the distribution

hist(pb_dat_comb$wind_s1)
hist(pb_dat_fin$wind_s1)

hist(pb_dat_comb$temp_s1)
hist(pb_dat_fin$temp_s1)

hist(pb_dat_comb$temp_s3)
hist(pb_dat_fin$temp_s3)


# First fit just detection model, then try to fit full occupancy model using covariates from your habitat chapter
# Will we even have enough sites for something like this???





#### Graveyard/Figuring things out
#pb_dat_comb$midnight <- "00:00"
#pb_dat_comb <- pb_dat_comb %>% mutate(midnight = hms::as_hms(paste0(midnight, ":00")))
#pb_dat_comb <- pb_dat_comb %>% mutate(start_test1 = is.na(starttime_s1), NA, hms::as_hms(paste0(starttime_s1,":00")))
# trying something else:

# make_mins <- function(time_col,num){
#   pb_dat_comb <- pb_dat_comb %>% separate(time_col, into = c("hr","min"), sep = ":") 
#   pb_dat_comb$hr <- as.numeric(pb_dat_comb$hr)
#   pb_dat_comb$min <- as.numeric(pb_dat_comb$min)
#   pb_dat_comb$hr_tomin <- pb_dat_comb$hr * 60
#   pb_dat_comb$starttime_min <- pb_dat_comb$hr_tomin + pb_dat_comb$min
#   pb_dat_comb
# }

# pb_dat_comb$midnight <- as.POSIXct(pb_dat_comb$midnight,format = "%h:%m")
# test <- pb_dat_comb %>% mutate(midnight = as.POSIXct(midnight, format = "%h:%m"))


# Why so many NAs? because there were columns that didn't pivot 
# sites <- c("CUL","CUL","AME","AME","ROB","ROB")
# survey_num <- c(1,2,1,2,1,2)
# sky_start <- c(2,3,2,2,1,4)
# temp_start <- c(55,65,54,57,48,62)
# r_dat <- data.frame("site" = sites,"survey" = survey_num, "sky_start" = sky_start, "temp_start" = temp_start)
# 
# # Test
# r_dat_test <- r_dat %>%
#   pivot_wider(names_from = survey, values_from = c(sky_start, temp_start), names_sep = "_survey")
# 
# 
# site_new <- c("CUL","AME","ROB")
# sky_survey1 <- c(2,2,1)
# sky_survey2 <- c(3,2,4)
# temp_survey1 <- c(55,54,48)
# temp_survey2 <- c(65,57,62)
# r_dat_new <- data.frame("site" = site_new, "sky_survey1" = sky_survey1, "sky_survey2" = sky_survey2, "temp_survey1" = temp_survey1, "temp_survey2" = temp_survey2)
# 
# 
# test <- meta_pb %>% group_by(site_id) %>% pivot_wider(names_from = survey_num, values_from = c(observer, 
#                                                                                                num_surveyors, 
#                                                                                                date, 
#                                                                                                survey_site_start_time, 
#                                                                                                sky_start, 
#                                                                                                wind_start, 
#                                                                                                temp_start))
# # again, this seems like a lot of NAs in this data??? What's going on with pivor longer
# test <- meta_pb %>% pivot_wider(names_from = survey_num, values_from = c(observer))