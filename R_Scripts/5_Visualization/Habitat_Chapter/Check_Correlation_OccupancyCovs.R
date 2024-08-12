##### Create Correlation Plot for Habitat Chapter Data #######

library(tidyverse)
library(ggplot2)
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")

all_dat <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_SCALED_8-12.csv")


# Detection covariates
# how to plot the covariance???


# Habitat covariates
#corrdat <- full_dat %>% select(-veg_sd_resid, -pct_can_core)
corrdat <- all_dat[,8:23]
corrgram(corrdat, order=TRUE, lower.panel=panel.cor,
         upper.panel=panel.pts, text.panel=panel.txt,
         main="Correlations in Occupancy Metrics")


# make corrgram of jsut the data in the current global model
corrdat2 <- all_dat %>% select(dtree_spp_rich, ctree_spp_rich, pct_can_landsc, pct_subcan_landsc, pct_subcan_core, veg_sd_resid, floodplain_shrub)
corrgram(corrdat2, order=TRUE, lower.panel=panel.cor,
         upper.panel=panel.pts, text.panel=panel.txt,
         main="Correlations in Occupancy Metrics In Global Model 3") 
