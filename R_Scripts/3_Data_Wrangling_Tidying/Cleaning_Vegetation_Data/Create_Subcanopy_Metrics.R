#### Create Community Subcanopy Metrics from Veg Survey Data ###################

## Purpose: to read in the cleaned data and create datasheets for subcanopy vegetation metrics at sites with ARUs for my habitat chapter

# Created 6/11/2024

# Last modified: 6/11/2024

#### Setup #################################
packages <- c("tidyverse","janitor","forcats")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_SiteColumn_fromPointID.R")
source("./R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")
load_packages(packages)


#### Read in Data ###################################
shrub_orig <- read.csv("./Data/Vegetation_Data/Outputs/2023_VegSurvey_ShrubData_Cleaned5-24.csv")
#shrub_points <- unique(shrub$point_id)

# Remove the points that don't have an ARU at it
veg <- read.csv("./Data/Vegetation_Data/Outputs/2023_VegSurvey_MainData_Cleaned5-24.csv")
veg_waru <- veg %>% filter(aru_present == "yes")
points_waru <- veg_waru$point_id

shrub <- shrub_orig %>% filter(point_id %in% points_waru) 

##### Metric 1: Subcanopy Veg Height #######
# Average height of all of the vegetation layers below the tree canopy 
shrub_height <- shrub %>% group_by(site_id) %>% summarize(avg_shrub_height = round(mean(shrub_height_m),2))


#### Metric 2: Subcanopy Veg Density ########
# Total percent area covered by subcanopy veg
# total_percent_shrub_cover from the veg datasheet
shrub_dense <- veg_waru %>% group_by(site_id) %>% summarize(shrub_pcover = sum(total_percent_shrub_cover))

#### Metric 3: Subcanopy Veg Complexity #####
# Variance in vegetation height below the canopy
shrub_cplx <- shrub %>% group_by(site_id) %>% summarize(shrub_heigh_sd = round(sd(shrub_height_m, na.rm = TRUE),2))

#### Combine them ####
shrub_metrics1 <- left_join(shrub_height,shrub_dense, by = "site_id") 
shrub_metrics <- left_join(shrub_metrics1,shrub_cplx, by = "site_id")

# Write this to csv
write.csv(shrub_metrics,"./Data/Habitat_Model_Covariates/Occupancy_Covariates/2023_ARUSites_SubCanopyMetrics_5-24.csv",row.names = FALSE)


#test <- cbind(shrub_height,shrub_dense,shrub_cplx, by = "site_id")
# Not reliable enough
