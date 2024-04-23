#########Look at ARU Heights #########

# A script to look at the retrieval data and visualize aru heigh t

# Created 4/23/2023
# last modified 4/23/2023

#### Setup ####
packages <- c("tidyverse","janitor", "ggplot2")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)
# Read in cuckoo color palette
source("./R_Scripts/5_Visualization/Create_CuckooColor_HexCodes.R")

#### Code: Preliminary Data #####
# Read in data
aru_ret_23 <- read.csv("./Data/Metadata/Outputs/2023_ARURetrieval_MetadataFull_Cleaned10-24.csv")
# Fix data entry error
aru_ret_23[14,11] <- 2.00
hist(aru_ret_23$aru_height)
# most arus around 2m, some at 1.5 m 
