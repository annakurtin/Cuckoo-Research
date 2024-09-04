#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create all habitat covariates data for modeling####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
packages <- c("tidyverse","janitor","ggplot2","corrgram")
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")
load_packages(packages)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data cleaning
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in the monitoring locations
points_forhabchap <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Monitoring_Points/21-23_AllMonitoringPoints_forLiDARExtractions.csv")
# pull out just the 2022 points
points_forhabchap <- points_forhabchap %>% separate(alt_point_id, into = c("point_id2","tag"), sep = "_")
points_22 <- points_forhabchap %>% filter(tag == "22")
sites_22 <- points_22 %>% create_site_col()
sites_22 <- sites_22 %>% group_by(site_id) %>% summarize(x = mean(long), y = mean(lat))
# formerly points for hab chap

# Read in the covariates derived from lidar data
lidar <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Vegetation_Data/Outputs/AllPoints_AllScales_LiDARMetrics_9-4-24.csv")
lidar <- lidar %>% separate(alt_point_id, into = c('point_id','year'), sep = "_", remove = FALSE)
lidar <- lidar %>% create_site_col()

# Filter out only the points used in 2022
lidar_22 <- lidar %>% filter(year == "22")
# some sites had three points, so let's take the average of the lidar metrics across those three to have one measurement per site
lidar_22 <- lidar_22 %>% group_by(site_id) %>% summarize(pct_can_landsc = round(mean(percent_canopy_landsc),2),
                                                         pct_subcan_landsc = round(mean(percent_subcan_landsc),2),
                                                         pct_openshrub_landsc = round(mean(percent_openshrub_landsc),2),
                                                         pct_can_core = round(mean(percent_canopy_core),2),
                                                         pct_subcan_core = round(mean(percent_subcan_core),2),
                                                         pct_openshrub_core = round(mean(percent_openshrub_core),2),
                                                         ht_can_core = round(mean(canopy_avgheight_core),2),
                                                         ht_subcan_core = round(mean(subcan_avgheight_core),2),
                                                         sd_subcan_core = round(mean(subcan_stdev_core),2),
                                                         sd_allveg_core = round(mean(all_stdev_core),2))

# Read in data on dominant shrub community - copy this over from the same points in 2023
shrubs_aru <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Occupancy_Covariates/2023_ARUSites_ShrubDominantCommunity_6-19.csv")# %>% mutate(year = "23")
shrubs_aru <- shrubs_aru %>% select(site_id, dominant_community)
# Change sites with no shrub cover to a different category instead of NA
shrubs_aru <- shrubs_aru %>% mutate(dominant_community = ifelse(is.na(dominant_community),"no_shrub", dominant_community))
shrubs_pb <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Occupancy_Covariates/2023_PBSites_ShrubDominantCommunity_8-15.csv")
shrubs <- rbind(shrubs_aru,shrubs_pb)

# read in data on tree species richness - both deciduous and conifer
tree_rich_aru <- read.csv("./Data/Habitat_Model_Covariates/Occupancy_Covariates/2023_ARUSites_TreeSppRich_8-12.csv")
# create a dummy variable for conifer presence at site
tree_rich_aru <- tree_rich_aru %>% mutate(ctree = ifelse(ctree_spp_rich > 0, 1, 0))
tree_rich_pb <- read.csv("./Data/Habitat_Model_Covariates/Occupancy_Covariates/2023_PBSites_TreeSppRich_8-15.csv")
# create a dummy variable for conifer presence at site
tree_rich_pb <- tree_rich_pb %>% mutate(ctree = ifelse(ctree_spp_rich > 0, 1, 0))
tree_rich <- rbind(tree_rich_aru, tree_rich_pb)
# Combine these into one dataframe and select columns of interest
all_dat <- left_join(sites_22,lidar_22, by = "site_id")
all_dat <- left_join(all_dat, shrubs, by ="site_id")
all_dat <- left_join(all_dat, tree_rich, by = "site_id")

# Create covariate of residuals
# create the regression model
reg <- glm(formula = sd_allveg_core ~ ht_can_core, data = all_dat)
# predict new values of y based on regression model
sd_predict <- predict(reg, all_dat)
# take differene between actual and predicted values
sd_resid <- all_dat$sd_allveg_core - sd_predict
# add it to the data
all_dat$veg_sd_resid <- round(sd_resid,2)

#Create dummy variables for shrub community
all_dat <- all_dat %>% mutate(broadleaf_shrub = case_when(dominant_community == "misc_broadleaf" ~ 1,
                                                          dominant_community == "invasive" ~ 0,
                                                          dominant_community == "upland" ~ 0,
                                                          dominant_community == "floodplain" ~ 0,
                                                          dominant_community == "no_shrub" ~ 0),
                              invasive_shrub = case_when(dominant_community == "misc_broadleaf" ~ 0,
                                                         dominant_community == "invasive" ~ 1,
                                                         dominant_community == "upland" ~ 0,
                                                         dominant_community == "floodplain" ~ 0,
                                                         dominant_community == "no_shrub" ~ 0),
                              upland_shrub = case_when(dominant_community == "misc_broadleaf" ~ 0,
                                                       dominant_community == "invasive" ~ 0,
                                                       dominant_community == "upland" ~ 1,
                                                       dominant_community == "floodplain" ~ 0,
                                                       dominant_community == "no_shrub" ~ 0),
                              floodplain_shrub = case_when(dominant_community == "misc_broadleaf" ~ 0,
                                                           dominant_community == "invasive" ~ 0,
                                                           dominant_community == "upland" ~ 0,
                                                           dominant_community == "floodplain" ~ 1,
                                                           dominant_community == "no_shrub" ~ 0))

# Select columns of interest
all_dat_fin <- all_dat %>% select(site_id, 
                              x,
                              y,
                              broadleaf_shrub,
                              invasive_shrub,
                              upland_shrub,
                              floodplain_shrub,
                              ctree,
                              dtree_spp_rich,
                              ctree_spp_rich,
                              pct_can_landsc,
                              pct_subcan_landsc,
                              pct_openshrub_landsc,
                              pct_can_core,
                              pct_subcan_core,
                              pct_openshrub_core,
                              ht_can_core,
                              ht_subcan_core,
                              sd_subcan_core,
                              veg_sd_resid,
                              sd_allveg_core)

write.csv(all_dat_fin, "C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Occupancy_Covariates/All_2022Veg_Covariates_9-4.csv", row.names = FALSE)


# Create another version of the data that has all of the covariates centered and scaled
#all_scaled <- all_dat_fin
dat1 <- all_dat_fin[,1:8]
#dat1 <- all_dat_fin[,5:11]
dat2 <- round(scale(all_dat_fin[,9:21]),2)
all_scaled <- cbind(dat1,dat2)

write.csv(all_scaled, "C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Occupancy_Covariates/All_2022VegCovs_Scaled_9-4.csv", row.names = FALSE)




# Gravy yard #####
# # group the data into grts points (either the points I selected for habitat use or the points selected via GRTS for the UMBEL monitoring) or nonrandomly selected points - not using this for linear model stuff
# lidar <- lidar %>% mutate(grts_grouped = case_when(sampling_design == "nonrand" ~ "nonrand",
#                                                    sampling_design == "mmr_grts"~ "grts",
#                                                    sampling_design == "habitat_grts" ~ "grts"))