#### Look at classifier outputs ####
packages <- c("data.table","tidyverse","janitor")
# Read in the packages function
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
# Load packages
load_packages(packages)



##### Input Which Site and Working Directory ####################
##### 2020 Model 1.0 #####
# load in the 2020 classifier scores
cnn_23 <- fread("F:/CNN_Classifier_Files/Model_2.0/Model_Scores/predictions_epoch-10_opso-0-10-1-2023_UMBEL_Audio.csv")
