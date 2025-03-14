#### Look at classifier outputs ####
packages <- c("data.table","tidyverse","janitor")
# Read in the packages function
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
# Load packages
load_packages(packages)



##### Input Which Site and Working Directory ####################
##### 2020 Model 1.0 #####
# load in the 2020 classifier scores
cnn_23 <- fread("./Data/Classifier_Results/Raw_Data/2023_UMBEL_topclips_petSiteperPeriod_annotations_v2_FINAL_AK_4-11.csv")
