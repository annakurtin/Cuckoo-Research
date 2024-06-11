####### See How Many Sites Along Each River ########

# A script to determine from the metadata how many sites are at each study portion for LiDAR data

# Created 6/11/2023
# last modified 6/11/2023

#### Setup ####
packages <- c("tidyverse","janitor", "ggplot2")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)
# Read in cuckoo color palette
source("./R_Scripts/5_Visualization/Create_CuckooColor_HexCodes.R")

#### Read in Data ####
mdep <- read.csv("./Data/Metadata/Outputs/2023_ARUDeployment_MetadataFull_Cleaned10-24.csv")
mret <- read.csv("./Data/Metadata/Outputs/2023_ARURetrieval_MetadataFull_Cleaned10-24.csv")
mret %>% group_by(river) %>% summarize( n_points = n())
