###### Clean LiDAR Derivatives #####

# This is a script to read in the outputs from the arcgis processing steps for the MT state lidar products (see OneNote Data Processing/Proj 9.6) and process the output tables to create habitat covariates for use in my analysis

# date create 7/15/20234
# last modified 9/4/2024

##### Packages and Functions #####
packages <- c("tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")
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
subcanp_core <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_130rad_subcanpixels_nocrop.csv")  %>% clean_names()
subcanp_core <- subcanp_core %>% select(alt_point_id,sum) %>% rename(subcan_pixels = sum)
canp_landsc <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_1000rad_canopypixels.csv") %>% clean_names()
canp_landsc <- canp_landsc %>% select(alt_point_id,sum) %>% rename(canopy_pixels = sum)
subcanp_landsc <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_1000mrad_subcanpixels_nocrop.csv")  %>% clean_names()
subcanp_landsc <- subcanp_landsc %>% select(alt_point_id,sum) %>% rename(subcan_pixels = sum)
# height
canh_core <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_130rad_canopyavgheight.csv")  %>% clean_names()
canh_core <- canh_core %>% select(alt_point_id, mean)
canh_core$canopy_avgheight_core <- round((canh_core$mean / 1000000) , 2) # Convert from integer
subcanh_core <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_130rad_subcanavgheight.csv")  %>% clean_names()
subcanh_core <- subcanh_core %>% select(alt_point_id, mean)
subcanh_core$subcan_avgheight_core <- round((subcanh_core$mean / 1000000) , 2) # Convert from integer
subcansd_core <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_130rad_subcanstdev.csv")  %>% clean_names()
subcansd_core <- subcansd_core %>% select(alt_point_id, std) 
subcansd_core$subcan_stdev_core <- round((subcansd_core$std / 1000000) , 2)  # Convert from integer
allvegsd_core <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_130rad_allvegstdev.csv") %>% clean_names()
allvegsd_core$all_stdev_core <- round((allvegsd_core$std / 1000000) , 2)
# crop pixels
#crop_core <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_130rad_croppixels.csv") %>% clean_names()
#crop_core <- crop_core %>% select(alt_point_id,sum) %>% rename(crop_pixels_core = sum)
#crop_landsc <- read.csv("./Data/Vegetation_Data/Raw_Data/lidar_1000mrad_croppixels.csv") %>% clean_names()
#crop_landsc <- crop_landsc %>% select(alt_point_id,sum) %>% rename(crop_pixels_landsc = sum)


#### Join Data ####
# Combine core 
core <- left_join(tp_core, canp_core, by = "alt_point_id")
core <- left_join(core, subcanp_core, by = "alt_point_id")
core <- left_join(core, canh_core, by = "alt_point_id")
core <- left_join(core, subcanh_core, by = "alt_point_id")
core <- left_join(core, subcansd_core, by = "alt_point_id")
core <- left_join(core, allvegsd_core, by = "alt_point_id")
# summary(core)
# Combine landscape 
landsc <- left_join(tp_landsc, canp_landsc, by = "alt_point_id")
landsc <- left_join(landsc, subcanp_landsc, by = "alt_point_id")
# summary(landsc)


#### Create Metrics #####
# landscape
landsc_metric <- landsc %>% mutate(percent_canopy_landsc = round((canopy_pixels/total_pixels)*100, digits= 2))
# percent subcanopy cover (removed canopy from total pixels)
landsc_metric <- landsc_metric %>% mutate(percent_subcan_landsc = round((subcan_pixels/(total_pixels-canopy_pixels))*100, digits= 2))
landsc_metric <- landsc_metric %>% mutate(percent_openshrub_landsc = round((subcan_pixels/(total_pixels))*100, digits= 2))
landsc_metric <- landsc_metric %>% select(alt_point_id,
                                          percent_canopy_landsc,
                                          percent_subcan_landsc,
                                          percent_openshrub_landsc)


#### core ####
core_metric <- core %>% mutate(percent_canopy_core = round((canopy_pixels/total_pixels)*100, digits= 2))
# percent subcanopy cover (removed canopy from total pixels)
core_metric <- core_metric %>% mutate(percent_subcan_core = round((subcan_pixels/(total_pixels-canopy_pixels))*100, digits= 2))
core_metric <- core_metric %>% mutate(percent_openshrub_core = round((subcan_pixels/(total_pixels))*100, digits= 2))
core_metric <- core_metric %>% select(alt_point_id,
                                      canopy_avgheight_core,
                                      subcan_avgheight_core, 
                                      subcan_stdev_core, 
                                      all_stdev_core,
                                      percent_canopy_core, 
                                      percent_subcan_core,
                                      percent_openshrub_core)

lidar_metrics <- left_join(landsc_metric, core_metric, by = "alt_point_id")
# Which are not in core metric?
setdiff(landsc_metric$alt_point_id, core_metric$alt_point_id) # the ones missing are the culbertson points, this is fine

#### Look at difference between two derivatives for subcanopy ####
plot(lidar_metrics$percent_subcan_landsc ~ lidar_metrics$percent_openshrub_landsc, xlab = "% Open Shrub Landscape", ylab = "% Subcanopy Landscape")
plot(lidar_metrics$percent_subcan_core ~ lidar_metrics$percent_openshrub_core, xlab = "% Open Shrub Core", ylab = "% Subcanopy Core")

##### Evaluate this for reasonableness ####
hist(landsc_metric$percent_canopy_landsc, main = "Landscape % Canopy")
hist(landsc_metric$percent_subcan_landsc, main = "Landscape % Subcan")
hist(core_metric$percent_canopy_core, main = "Core % Canopy")
hist(core_metric$percent_subcan_core, main = "Core % Subcan")
hist(core_metric$canopy_avgheight_core, main = "Core Avg Canopy Height")
hist(core_metric$subcan_avgheight_core, main = "Core Avg Subcan Height")
hist(core_metric$subcan_stdev_core, main = "Core Subcan St Dev")
hist(core_metric$all_stdev_core, main = "All Veg St Dev")

#### Export Data #####
write.csv(landsc_metric,"./Data/Vegetation_Data/Outputs/AllPoints_LandscapeScale_LiDARMetrics_9-4-24.csv", row.names = FALSE)
write.csv(core_metric,"./Data/Vegetation_Data/Outputs/AllPoints_CoreAreaScale_LiDARMetrics_9-4-24.csv", row.names = FALSE)
write.csv(lidar_metrics,"./Data/Vegetation_Data/Outputs/AllPoints_AllScales_LiDARMetrics_9-4-24.csv", row.names = FALSE)

#### Double Check Against Map ####
all <- read.csv("./Data/Vegetation_Data/Outputs/AllPoints_AllScales_LiDARMetrics_9-4-24.csv")

# Lower MISO
all %>% filter(alt_point_id == "ROB_23avg") # looks good
#all[all$alt_point_id == "ROB_23avg",]
# MISO-069
all %>% filter(alt_point_id == "MISO-069_23") # Percent subcanopy core seems a little high?

# Upper MISO
all %>% filter(alt_point_id == "MISO-185_23") # looks goood
all %>% filter(alt_point_id == "MISO-016_23") # looks good
all %>% filter(alt_point_id == "JUD_23avg") # looks good

# MUSH
all %>% filter(alt_point_id == "MUSH-060_23") # looks good
all %>% filter(alt_point_id == "MUSH-169_23") # looks good

# YELL
all %>% filter(alt_point_id == "HOL_22avg") # looks good
all %>% filter(alt_point_id == "YELL-050_23") # looks good
all %>% filter(alt_point_id == "SID_22avg") # looks good
