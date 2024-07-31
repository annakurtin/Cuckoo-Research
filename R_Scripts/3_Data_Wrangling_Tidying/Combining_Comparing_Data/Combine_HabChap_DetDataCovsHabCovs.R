### Combine all habitat chapter data ####
packages <- c("tidyverse","janitor","ggplot2","corrgram")
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")
load_packages(packages)

# created 7/29/2024 to combine all of the data to run the occupancy submodels

# last modified 7/30/2024 to keep veg sd residuals 

#### read in data ####

hab_covs_det <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Occupancy_Covariates/DetHist_VegCovs_Scaled_7-24.csv")
det_covs <- readRDS(file ="C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Full_DetectionDataSCALED_JAGSModels_HabChap.Rdata")
# round this to make it more readable
det_covs[,8:26] <- round(det_covs[,8:26],2)

# join by site ID
all_dat <- left_join(hab_covs_det, det_covs, by = "site_id")
 
# remove the dupicate detection column
all_dat <- all_dat %>% select(-c(22:27))
# rename
all_dat <- all_dat %>% rename(det_s1 = det_survey1.x,
                   det_s2 = det_survey2.x,
                   det_s3 = det_survey3.x,
                   det_s4 = det_survey4.x,
                   det_s5 = det_survey5.x,
                   det_s6 = det_survey6.x)

# write this to .csv
write.csv(all_dat,"C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_SCALED_7-30.csv", row.names = FALSE)
