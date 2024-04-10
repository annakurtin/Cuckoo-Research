#### Clean CNN2.0 Clips ###################

## Purpose: to read in the clips sheets and create an output for daily BBCU presence/absence 

# Created 4/9/2024

# Last modified: 4/9/2024

#### Setup #################################
packages <- c("data.table","tidyverse","janitor","chron")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)