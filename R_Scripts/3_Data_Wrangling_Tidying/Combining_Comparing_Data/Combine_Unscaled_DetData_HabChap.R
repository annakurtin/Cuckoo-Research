# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Combine Detection Covariates ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This is a script made from the code from Detection Model 1 to combine together the detection covariates
# only combining detection data from 2023

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Setup ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Combine and Clean Data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Site-level covariates
# Read in vegetation density for each site
vegdense <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Detection_Covariates/ShrubTreeDensityComposite_2023_ARUPoints.csv")
vegdense <- vegdense %>%
  mutate(site_id = case_when(
    # If point_id has four letters then a dash then three numbers
    grepl("^[[:alpha:]]{4}-[[:digit:]]{3}$", point_id) ~ point_id,
    # If point_id has two numbers, three numbers, or three letters then a dash then one number
    grepl("^[[:digit:]]{2,3}-[[:digit:]]{1}$", point_id) |
      grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$", point_id) ~ gsub("-.*", "", point_id)
  ))
# take the average of this across the site
vegdense_avg <- vegdense %>% group_by(site_id,sampling_design) %>% summarize(veg_density_avg = round(mean(composite_dense,na.rm = TRUE),2))


# background noise
background_db <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Detection_Covariates/BackgroundNoiseBandpass_2023_ARUPoints.csv")
colnames(background_db) <- c("site_id","backdb_survey1","backdb_survey2","backdb_survey3","backdb_survey4","backdb_survey5","backdb_survey6")


# Read in detection and effort data 
effort <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Detection_History/2023_All_ARUs/Outputs/DetectionHist_CamTrapR/bbcu__effort__not_scaled_14_days_per_occasion__2024-04-29.csv") %>% clean_names
survey_periods <- colnames(effort)
colnames(effort) <- c("site_session","effort_survey1","effort_survey2","effort_survey3","effort_survey4","effort_survey5","effort_survey6")
effort <- effort %>% separate(site_session, into = c("site_id","session_name"), sep = "__") %>% select(-session_name)


arutype <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/ARUtype_22-23.csv")
# separate out 2023 data
arutype <- arutype %>% filter(year == "2023")


detections <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Detection_History/2023_All_ARUs/Outputs/DetectionHist_CamTrapR/bbcu__detection_history__with_effort__14_days_per_occasion__2024-04-29.csv") %>% clean_names
colnames(detections) <- c("site_session","det_survey1","det_survey2","det_survey3","det_survey4","det_survey5","det_survey6")





# Split apart the detections into site and point
detections <- detections %>% separate(site_session, into = c("site_id","session_name"), sep = "__")

# # Combine them
full_dat <- left_join(detections,background_db, by = "site_id")
full_dat <- left_join(full_dat,vegdense_avg, by = "site_id")
full_dat <- left_join(full_dat, effort, by = "site_id")
full_dat <- full_dat %>% mutate(start_date_s1 = 152,
                                start_date_s2 = 166,
                                start_date_s3 = 180,
                                start_date_s4 = 194,
                                start_date_s5 = 208,
                                start_date_s6 = 222)
full_dat <- full_dat %>% select(-c(session_name, sampling_design))

# center and scale variables
test <- scale(full_dat$veg_density_avg)

#saveRDS(full_dat, file ="./Data/Habitat_Model_Covariates/Full_DetectionData_JAGSModels_HabChap.Rdata")