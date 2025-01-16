####### See How Many Sites Along Each River ########

# A script to determine from the metadata how many sites are at each study portion for LiDAR data

# Created 6/11/2023
# last modified 1/16/2025

#### Setup ####
packages <- c("tidyverse","janitor", "ggplot2")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)
source("./R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")
# Read in cuckoo color palette
#source("./R_Scripts/5_Visualization/Create_CuckooColor_HexCodes.R")

#### Read in Data ####
mdep <- read.csv("./Data/Metadata/Outputs/2023_ARUDeployment_MetadataFull_Cleaned10-24.csv")
mret <- read.csv("./Data/Metadata/Outputs/2023_ARURetrieval_MetadataFull_Cleaned10-24.csv")
mret <- mret %>% create_site_col()
# get number of points at each river
mret %>% group_by(river) %>% summarize(n_points = n())
# get number of sites at each river
mret %>% group_by(river) %>% summarize(n_sites = length(unique(site_id)))
# remove the two we didn't consider from MISO - the site on private and the site we couldn't retrieve

#### Look at playback data as well ####
#pb23 <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackMetadata_3-22-24.csv")
pb23 <- read.csv("./Data/Playback_Results/2023/Outputs/2023_PlaybackSiteLocationsAvg_All_3-22-24.csv") %>% rename(x = long_avg, y = lat_avg)


# Split out Upper Missouri River
# 108.0743650°W 47.6626152°N - easternmost extent of the UMBEL deployments
# 108.7668825°W 47.4279918°N - southernmost edge of the UMBEL deployments\
upper_miso <- pb23 %>% filter(x < -108.0743650 & y > 47.4279918)

# Split out Lower Missouri River 
# Coordinates for southern and westernmost boundary 47.963980, -106.414798
lower_miso <- pb23 %>% filter(x > -106.414798 & y > 47.963980)

# Split out Yellowstone
# Coordinates for Nothernmost and westernmost boundary 47.751310, -107.7825650
yell <- pb23 %>% filter(x > -107.7825650 & y < 47.751310)

# Split out Region 5/Musselshell
# 107.7825650°W 46.7159112°N 
mush <- pb23 %>% filter(x < -107.7825650 & y < 46.7159112)