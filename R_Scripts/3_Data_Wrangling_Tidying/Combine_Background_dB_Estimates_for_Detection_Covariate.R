##### Summarize the Background dB in Files ######

# read in background dB, take mean per site, combine across collabs, and output for detection covariates

# Created 4/29/2023

# Last edited 4/29/2023

### Setup #####

packages <- c("tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)


##### Code #####

umbel_23 <- read.csv("./Data/Detection_Covariates/Avg_dB_FullFiles/Raw_Data/2023_UMBEL_Background_Noise.csv") %>% clean_names()
fwpr7_23 <- read.csv("./Data/Detection_Covariates/Avg_dB_FullFiles/Raw_Data/2023_FWPR7_Background_Noise.csv") %>% clean_names()
fwpr6_23 <- read.csv("./Data/Detection_Covariates/Avg_dB_FullFiles/Raw_Data/2023_FWPR6_Background_Noise.csv") %>% clean_names()
fwpr5_23 <- read.csv("./Data/Detection_Covariates/Avg_dB_FullFiles/Raw_Data/2023_FWPR5_Background_Noise.csv") %>% clean_names()

all_back <- rbind(umbel_23,fwpr7_23,fwpr6_23,fwpr5_23) %>% separate(file_name, into = c("point_id","date","hour_ext"), sep = "_")

all_back_fin <- all_back %>% group_by(point_id) %>% summarize(mean_db = mean(average_d_b, na.rm = TRUE))

all_back_fin <- all_back_fin %>%
  mutate(site_id = case_when(
    # If point_id has four letters then a dash then three numbers
    grepl("^[[:alpha:]]{4}-[[:digit:]]{3}$", point_id) ~ point_id,
    # If point_id has two numbers, three numbers, or three letters then a dash then one number
    grepl("^[[:digit:]]{2,3}-[[:digit:]]{1}$", point_id) |
      grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$", point_id) ~ gsub("-.*", "", point_id)
  ))
# Write the point-level averages
#write.csv(all_back_fin, "./Data/Detection_Covariates/Avg_dB_FullFiles/Outputs/2023_AllCollabs_AvgBackground_Noise.csv", row.names = FALSE)

all_back_site <- all_back_fin %>% group_by(site_id) %>% summarize(avg_db = mean(mean_db))
