#### Create Confusion Matrix for ARU Type and BBCU ####
library(tidyverse)

# Read in annotation
clips_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_5-27.csv")
clips_23 <- clips_23 %>% group_by(point_id, date) %>% summarize(bbcu = max(annotation))
clips_23 <- clips_23 %>% group_by(point_id) %>% summarize(days_wbbcu = sum(bbcu), bbcu_present = max(bbcu))
clips_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPBYBCU_5-27.csv")
clips_22 <- clips_22 %>% group_by(point_id, date) %>% summarize(bbcu = max(annotation))
clips_22 <- clips_22 %>% group_by(point_id) %>% summarize(days_wbbcu = sum(bbcu), bbcu_present = max(bbcu))

# Read in Metadata
#aru_type <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/ARUtype_22-23.csv")
arus_22 <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/ARUModelByPoint_22_11-4.csv")
arus_23 <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/ARUModelByPoint_23_11-4.csv")

# Join the datasets
dat_23 <- left_join(clips_23, arus_23, by = "point_id") %>% select(-c(site_id,aru_mbinary))
coords_23 <- read.csv("./Data/Metadata/Outputs/2023_ARUDeployment_MetadataFull_Cleaned10-24.csv") %>% select(point_id,x,y)
dat_wcoords23 <- 
dat_22 <- left_join(clips_22, arus_22, by = "point_id") %>% select(-c(site_id,aru_mbinary))
coords_22_1 <- read.csv("./Data/Metadata/Outputs/2022_ARUDeployment_Retrieval_Metadata_FWPALL_Cleaned1-22.csv") %>% select(point_id,lat,long)
coords_22_2 <- read.csv("./Data/Metadata/Outputs/2022_ARUDeployment_Retrieval_Metadata_UMBEL_Cleaned1-22.csv") %>% select(point_id,lat,long)
coords_22 <- rbind(coords_22_1,coords_22_2) %>% rename(x = long, y = lat)

# Create data for arcgis
coords_dat_23 <- left_join(dat_23, coords_23, by = "point_id")
# write
#write.csv(coords_dat_23, "./Data/Habitat_Model_Covariates/Detection_Covariates/ARUModelByPoint_wCoords_23_11-4.csv", row.names = FALSE)
coords_dat_22 <- left_join(dat_22, coords_22, by = "point_id")
#write.csv(coords_dat_22, "./Data/Habitat_Model_Covariates/Detection_Covariates/ARUModelByPoint_wCoords_22_11-4.csv", row.names = FALSE)
allcoords_dat <- rbind(coords_dat_23, coords_dat_22)
# Write this
#write.csv(allcoords_dat, "./Data/Habitat_Model_Covariates/Detection_Covariates/ARUModelByPoint_wCoords_22-23_11-4.csv", row.names = FALSE)

# Create a confusion matrix for ARU type
matrix_23 <- dat_23 %>% group_by(aru_model) %>% summarize(total_days = sum(days_wbbcu), bbcu = sum(bbcu_present), total = n())
matrix_22 <- dat_22 %>% group_by(aru_model) %>% summarize(total_days = sum(days_wbbcu), bbcu = sum(bbcu_present), total = n())

# Look at both of them combined
alldat <- rbind(dat_23, dat_22)
matrix_all <- alldat %>% group_by(aru_model) %>% summarize(total_days = sum(days_wbbcu), bbcu = sum(bbcu_present), total = n())
