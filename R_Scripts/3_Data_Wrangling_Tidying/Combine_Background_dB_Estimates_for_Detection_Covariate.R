##### Summarize the Background dB in Files ######

# read in background dB, take mean per site, combine across collabs, and output for detection covariates

# Created 4/29/2023

# Last edited 4/29/2023

### Setup #####

packages <- c("tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)


##### Code #####

umbel_23 <- read.csv("./Data/Detection_Covariates/Avg_dB_FullFiles/2023_UMBEL_Background_Noise.csv") %>% clean_names()
fwpr7_23 <- read.csv("./Data/Detection_Covariates/Avg_dB_FullFiles/2023_FWPR7_Background_Noise.csv") %>% clean_names()
fwpr6_23 <- read.csv("./Data/Detection_Covariates/Avg_dB_FullFiles/2023_FWPR6_Background_Noise.csv") %>% clean_names()
fwpr5_23 <- read.csv("./Data/Detection_Covariates/Avg_dB_FullFiles/2023_FWPR5_Background_Noise.csv") %>% clean_names()

all_back <- rbind(umbel_23,fwpr7_23,fwpr6_23,fwpr5_23) %>% separate(file_name, into = c("point_id","date","hour_ext"), sep = "_")

all_back_fin <- all_back %>% group_by(point_id) %>% summarize(mean_db = mean(average_d_b, na.rm = TRUE))
