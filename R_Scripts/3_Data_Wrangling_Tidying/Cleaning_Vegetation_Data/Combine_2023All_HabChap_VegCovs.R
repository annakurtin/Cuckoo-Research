# Create all habitat covariates data for modeling####

packages <- c("tidyverse","janitor","ggplot2","corrgram")
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")
load_packages(packages)

# Read in the monitoring locations
points_forhabchap <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Monitoring_Points/2023_AllARUPoints_FromDeploymentData.csv")
points_forhabchap <- points_forhabchap %>% create_site_col()
points_forhabchap <- points_forhabchap %>% group_by(site_id) %>% summarize(x = mean(x), y = mean(y))

# Read in the covariates derived from lidar data
lidar <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Vegetation_Data/Outputs/AllPoints_AllScales_LiDARMetrics_7-23-24.csv")
lidar <- lidar %>% separate(alt_point_id, into = c('point_id','year'), sep = "_", remove = FALSE)
lidar <- lidar %>% create_site_col()
lidar <- lidar %>% create_samp_col()
# group the data into grts points (either the points I selected for habitat use or the points selected via GRTS for the UMBEL monitoring) or nonrandomly selected points
lidar <- lidar %>% mutate(grts_grouped = case_when(sampling_design == "nonrand" ~ "nonrand",
                                                   sampling_design == "mmr_grts"~ "grts",
                                                   sampling_design == "habitat_grts" ~ "grts"))
# Filter out only the points usedin 2023, which are the only points I'm looking at for this chapter
lidar_23 <- lidar %>% filter(year == "23")
# some sites had three points, so let's take the average of the lidar metrics across those three to have one measurement per site
lidar_23 <- lidar_23 %>% group_by(site_id) %>% summarize(pct_can_landsc = round(mean(percent_canopy_landsc),2),
                                                         pct_subcan_landsc = round(mean(percent_subcan_landsc),2),
                                                         pct_can_core = round(mean(percent_canopy_core),2),
                                                         pct_subcan_core = round(mean(percent_subcan_core),2),
                                                         ht_can_core = round(mean(canopy_avgheight_core),2),
                                                         ht_subcan_core = round(mean(subcan_avgheight_core),2),
                                                         sd_subcan_core = round(mean(subcan_stdev_core),2),
                                                         sd_allveg_core = round(mean(all_stdev_core),2),
                                                         grts_grouped = first(grts_grouped))

# Read in data on dominant shrub community
shrubs <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Occupancy_Covariates/2023_ARUSites_ShrubDominantCommunity_6-19.csv")# %>% mutate(year = "23")
shrubs <- shrubs %>% select(site_id, dominant_community)
# Change sites with no shrub cover to a different category instead of NA
shrubs <- shrubs %>% mutate(dominant_community = ifelse(is.na(dominant_community),"no_shrub", dominant_community))
# read in data on tree species richness - both deciduous and conifer
#tree_rich <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Occupancy_Covariates/2023_ARUSites_TreeSppRich_7-24.csv")# %>% mutate(year = "23")
tree_rich <- read.csv("./Data/Habitat_Model_Covariates/Occupancy_Covariates/2023_ARUSites_TreeSppRich_8-12.csv")
# create a dummary variable for conifer presence at site
tree_rich <- tree_rich %>% mutate(ctree = ifelse(ctree_spp_rich > 0, 1, 0))
# Combine these into one dataframe and select columns of interest
all_dat <- left_join(points_forhabchap,lidar_23, by = "site_id")
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
                              grts_grouped,
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
                              pct_can_core,
                              pct_subcan_core,
                              ht_can_core,
                              ht_subcan_core,
                              sd_subcan_core,
                              veg_sd_resid,
                              sd_allveg_core)

#write.csv(all_dat_fin, "C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Occupancy_Covariates/All_Veg_Covariates_8-12.csv", row.names = FALSE)


# Create another version of the data that has all of the covariates centered and scaled
#all_scaled <- all_dat_fin
dat1 <- all_dat_fin[,1:9]
#dat1 <- all_dat_fin[,5:11]
dat2 <- round(scale(all_dat_fin[,10:20]),2)
all_scaled <- cbind(dat1,dat2)

#write.csv(all_scaled, "C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Occupancy_Covariates/All_VegCovs_Scaled_8-12.csv", row.names = FALSE)

## Combine this with detection history data
det <- read.csv("./Data/Detection_History/2023_All_ARUs/Outputs/SiteDetHist_TwoWeekSurvey_Cleaned_7-24.csv")
det_veg_comb <- left_join(det, all_scaled) %>% select(-c(grts_grouped,x,y))
#write.csv(det_veg_comb, "C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Occupancy_Covariates/DetHist_VegCovs_Scaled_8-12.csv", row.names = FALSE)
