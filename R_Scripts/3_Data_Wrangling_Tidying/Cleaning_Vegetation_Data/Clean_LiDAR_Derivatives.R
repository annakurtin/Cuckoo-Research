###### Clean LiDAR Derivatives #####

# This is a script to read in the outputs from the arcgis processing steps for the MT state lidar products (see OneNote Data Processing/Proj 9.6) and process the output tables to create habitat covariates for use in my analysis

# date create 7/15/20234
# last modified 7/15/2024

##### Packages and Functions #####
packages <- c("tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_SiteColumn_fromPointID.R")
source("./R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")
load_packages(packages)


##### Data ######
# using landsc to refer to landscape scape (1000m)
# using core to refer to core area (130m)
# total pixels
tp_core <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_130rad_totalpixels.csv") %>% clean_names() 
tp_core <- tp_core %>% select(alt_point_id,sum) %>% rename(total_pixels = sum)
tp_landsc <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_1000rad_totalpixels.csv")  %>% clean_names()
tp_landsc <- tp_landsc %>% select(alt_point_id,sum) %>% rename(total_pixels = sum)
# percent cover
canp_core <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_130rad_canopypixels.csv")  %>% clean_names()
canp_core <- canp_core %>% select(alt_point_id,sum) %>% rename(canopy_pixels = sum)
subcanp_core <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_130rad_subcanpixels.csv")  %>% clean_names()
subcanp_core <- subcanp_core %>% select(alt_point_id,sum) %>% rename(subcan_pixels = sum)
canp_landsc <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_1000rad_canopypixels.csv") %>% clean_names()
canp_landsc <- canp_landsc %>% select(alt_point_id,sum) %>% rename(canopy_pixels = sum)
subcanp_landsc <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_1000rad_subcanpixels.csv")  %>% clean_names()
subcanp_landsc <- subcanp_landsc %>% select(alt_point_id,sum) %>% rename(subcan_pixels = sum)
# height
canh_core <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_130rad_canopyavgheight.csv")  %>% clean_names()
canh_core <- canh_core %>% select(alt_point_id, mean) %>% rename(canopy_avgheight = mean)
canh_core$canopy_avgheight <- round((canh_core$canopy_avgheight / 1000000) , 2) # Convert from integer
subcanh_core <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_130rad_subcanavgheight.csv")  %>% clean_names()
subcanh_core <- subcanh_core %>% select(alt_point_id, mean) %>% rename(subcan_avgheight = mean)
subcanh_core$subcan_avgheight <- round((subcanh_core$subcan_avgheight / 1000000) , 2) # Convert from integer
subcansd_core <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_130rad_subcanstdev.csv")  %>% clean_names()
subcansd_core <- subcansd_core %>% select(alt_point_id, std) %>% rename(subcan_stdev = std)
subcansd_core$subcan_stdev <- round((subcansd_core$subcan_stdev / 1000000) , 2)  # Convert from integer


#### Join Data ####
# Combine core 
core <- left_join(tp_core, canp_core, by = "alt_point_id")
core <- left_join(core, subcanp_core, by = "alt_point_id")
core <- left_join(core, canh_core, by = "alt_point_id")
core <- left_join(core, subcanh_core, by = "alt_point_id")
core <- left_join(core, subcansd_core, by = "alt_point_id")
# summary(core)
# Combine landscape 
landsc <- left_join(tp_landsc, canp_landsc, by = "alt_point_id")
landsc <- left_join(landsc, subcanp_landsc, by = "alt_point_id")
# summary(landsc)


#### Create Metrics #####
# percent canopy cover
landsc_metric <- landsc %>% mutate(percent_canopy = round((canopy_pixels/total_pixels)*100, digits= 2))
# percent subcanopy cover (removed canopy from total pixels)
landsc_metric <- landsc_metric %>% mutate(percent_subcan = round((subcan_pixels/(total_pixels-canopy_pixels))*100, digits= 2))
# TODO CHECK THESE AGAINST ACTUAL POINTS AND SEE IF THEY MAKE SENSE
landsc_metric <- landsc_metric %>% select(alt_point_id,
                                          percent_canopy,
                                          percent_subcan)


# percent canopy cover
core_metric <- core %>% mutate(percent_canopy = round((canopy_pixels/total_pixels)*100, digits= 2))
# percent subcanopy cover (removed canopy from total pixels)
core_metric <- core_metric %>% mutate(percent_subcan = round((subcan_pixels/(total_pixels-canopy_pixels))*100, digits= 2))
core_metric <- core_metric %>% select(alt_point_id,
                                      canopy_avgheight,
                                      subcan_avgheight, 
                                      subcan_stdev, 
                                      percent_canopy, 
                                      percent_subcan)


##### Evaluate this for reasonableness ####
hist(landsc_metric$percent_canopy, main = "Landscape % Canopy")
hist(landsc_metric$percent_subcan, main = "Landscape % Subcan")
hist(core_metric$percent_canopy, main = "Core % Canopy")
hist(core_metric$percent_subcan, main = "Core % Subcan")
hist(core_metric$canopy_avgheight, main = "Core Avg Canopy Height")
hist(core_metric$subcan_avgheight, main = "Core Avg Subcan Height")
hist(core_metric$subcan_stdev, main = "Core Subcan St Dev")

#### Export Data #####
write.csv(landsc_metric,"./Data/Vegetation_Data/Outputs/AllPoints_LandscapeScale_LiDARMetrics_7-15-24.csv", row.names = FALSE)
write.csv(core_metric,"./Data/Vegetation_Data/Outputs/AllPoints_CoreAreaScale_LiDARMetrics_7-15-24.csv", row.names = FALSE)
