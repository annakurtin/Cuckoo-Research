### Create 2022 2023 Site Avg BBCU Presence for ArcGIS

library(tidyverse)

# Read in site level bbcu
raw_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/SiteLevelBBCU_2022.csv")
raw_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/SiteLevelBBCU_2023.csv")

# Read in monitoring point locations
locs <- read.csv("./Data/Monitoring_Points/21-23_AllMonitoringPoints_forLiDARExtractions.csv")



# Write two sheets: by site and by point