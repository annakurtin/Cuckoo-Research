#### What's going on with subcanopy and upland?? ####

library(tidyverse)
library(ggplot2)
library(beepr)
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")

# Data
full_dat <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Occupancy_Covariates/DetHist_VegCovs_Scaled_7-24.csv")

s_dat <- full_dat %>% mutate(upland_only = ifelse(upland_shrub == 1, "Y","N")) %>% select(site_id,upland_only, pct_subcan_landsc, pct_subcan_core)


# What do the relationships look like?
ggplot(data = s_dat) +
  geom_bar(aes(fill = upland_only, y = pct_subcan_landsc, x = upland_only), stat = "identity")

ggplot(data = s_dat) +
  geom_point(aes(col = upland_only, y = pct_subcan_landsc, x = pct_subcan_core), stat = "identity", size = 3)

# make a new variable that has only percent subcanopy landscape without upland sites?