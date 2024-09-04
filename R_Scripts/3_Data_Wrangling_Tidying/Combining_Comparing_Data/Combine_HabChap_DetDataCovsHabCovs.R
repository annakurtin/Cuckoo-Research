### Combine all habitat chapter data ####
packages <- c("tidyverse","janitor","ggplot2","corrgram")
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")
load_packages(packages)

# created 7/29/2024 to combine all of the data to run the occupancy submodels

# last modified 7/30/2024 to keep veg sd residuals 

#### read in data ####

hab_covs_det <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Occupancy_Covariates/DetHist_2023VegCovs_Scaled_9-4.csv")
det_covs <- readRDS(file ="C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Full_DetectionDataSCALED_JAGSModels_HabChap.Rdata")
# round this to make it more readable
det_covs[,8:26] <- round(det_covs[,8:26],2)

# join by site ID
all_dat <- left_join(hab_covs_det, det_covs, by = "site_id")
 
# remove the duplicate detection column
all_dat <- all_dat %>% select(-c(26:31))
# rename
all_dat <- all_dat %>% rename(det_s1 = det_survey1.x,
                   det_s2 = det_survey2.x,
                   det_s3 = det_survey3.x,
                   det_s4 = det_survey4.x,
                   det_s5 = det_survey5.x,
                   det_s6 = det_survey6.x)

# write this to .csv
#write.csv(all_dat,"C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_SCALED_9-4.csv", row.names = FALSE)



### Create combined data unscaled

# State covariates
hab_covs_det_us <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Occupancy_Covariates/All_2023Veg_Covariates_9-4.csv")
# Detection covariates
det_covs_us <- readRDS(file ="C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Full_DetectionData_JAGSModels_HabChap.Rdata")

# join by site ID
all_dat_us <- left_join(hab_covs_det_us, det_covs_us, by = "site_id")

# rename columns 
all_dat_us <- all_dat_us %>% rename(det_s1 = det_survey1,
                              det_s2 = det_survey2,
                              det_s3 = det_survey3,
                              det_s4 = det_survey4,
                              det_s5 = det_survey5,
                              det_s6 = det_survey6,
                              backdb_s1 = backdb_survey1,
                              backdb_s2 = backdb_survey2,
                              backdb_s3 = backdb_survey3,
                              backdb_s4 = backdb_survey4,
                              backdb_s5 = backdb_survey5,
                              backdb_s6 = backdb_survey6,
                              effort_s1 = effort_survey1,
                              effort_s2 = effort_survey2,
                              effort_s3 = effort_survey3,
                              effort_s4 = effort_survey4,
                              effort_s5 = effort_survey5,
                              effort_s6 = effort_survey6,
                              )

# Select relevant columns
all_dat_us_fin <- all_dat_us %>% select(site_id,
                                        x, y,
                                        23:47,
                                        5:22) 

# write this to .csv
#write.csv(all_dat_us_fin,"C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_UNSCALED_9-4.csv", row.names = FALSE)


# Make this into a version for Thomas' class
# remove the x and y coordinates, remove ctree and veg sd resid
# change the site IDs into 1-107
class_dat <- all_dat_us_fin %>% select(-c(x, y, veg_sd_resid,ctree))
class_dat$site_id <- c(1:110)
# check csv for this
#write.csv(class_dat,"C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_ClassData.csv", row.names = FALSE)
