### Combine Output from Sonic Masking Covariate Estimation in Python ####

# Script created 3/19/2023
# Script last edited 3/19/2023

#### Setup#####
packages <- c("data.table","tidyverse","janitor")
# Read in the packages function
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
# Load packages
load_packages(packages)


#### Code#####
test <- read.csv("./Data/Detection_Covariates/Test/2023_FWPR6_Background_Noise.csv") %>% clean_names()
test %>% filter(is.na(average_d_b)==TRUE)
