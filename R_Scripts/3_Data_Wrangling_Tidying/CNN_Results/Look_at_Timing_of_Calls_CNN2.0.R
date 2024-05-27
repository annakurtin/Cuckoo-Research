####### Look at Distribution of Calls ########

# A script to read in the detection history and look at first/last calls, create histograms of date, etc

# Created 5/27/2024
# last modified 5/27/2024

#### Setup ########################
packages <- c("tidyverse","janitor", "ggplot2")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)
# Read in cuckoo color palette
source("./R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")

#### Code ##########################

clips23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_5-27.csv")
clips22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPBYBCU_5-27.csv")
clips21 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2021_AllCollab_topclips_filteredPBYBCU_5-27.csv")
#convert date to datetime
clips23$datetime <- as_datetime(clips23$datetime)
clips22$datetime <- as_datetime(clips22$datetime)
clips21$datetime <- as_datetime(clips21$datetime)

# Find the latest date of the calls
latest_calls_23 <- clips23 %>% filter(annotation == 1) %>% group_by(site_id) %>% summarize(latest_date = max(datetime))
latest_23 <- max(latest_calls_23$latest_date)
latest_calls_22 <- clips22 %>% filter(annotation == 1) %>% group_by(site_id) %>% summarize(latest_date = max(datetime))
latest_22 <- max(latest_calls_22$latest_date)
latest_calls_21 <- clips21 %>% filter(annotation == 1) %>% group_by(site_id) %>% summarize(latest_date = max(datetime))
latest_21 <- max(latest_calls_21$latest_date)

# How to decide where to cut this off?
## Visualize distributions