##### Comparing Detection Models ######

# Created 6/20/2024

# Last Modified 6/20/2024

#### Setup ####
library(jagsUI)
library(rjags)
library(tidyverse)
library(jagshelper)
library(ggplot2)
library(coda)
library(janitor)
library(loo)
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")


#### Code #####
habdet_m2 <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Detection_Submodel/Models_Ran_Outputs/Actual_Data/JAGS_Global_Detection_Model.Rdata")

habdet_m2 <- jags.samples(habdet_m2$model,
                          c("WAIC","deviance"),
                          type = "mean",
                          n.iter = 5000,
                          n.burnin = 1000,
                          n.thin = 2)

habdet_m2$p_waic <- habdet_m2$WAIC
habdet_m2$waic <- habdet_m2$deviance + habdet_m2$p_waic
tmp <- sapply(habdet_m2, sum)
waic_m2 <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)
# WAIC is 5625.7



habdet_m3 <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Detection_Submodel/Models_Ran_Outputs/Actual_Data/JAGS_Global_Detection_Model_QuadDate.Rdata")

samples_m3 <- jags.samples(habdet_m3$model,
                           c("WAIC","deviance"),
                           type = "mean",
                           n.iter = 20000,
                           n.burnin = 4000,
                           n.thin = 2)
# Have to run this one models that aren't run in parallel
# can change this to 5000 with a burnin of 1000

samples_m3$p_waic <- samples_m3$WAIC
samples_m3$waic <- samples_m3$deviance + samples_m3$p_waic
tmp <- sapply(samples_m3, sum)
waic_m3 <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)
# WAIC is 5636.9
# Get standard error