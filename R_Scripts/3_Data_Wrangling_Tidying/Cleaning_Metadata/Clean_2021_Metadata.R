## Clean 2021 Metadata #####



#### Install and load pacakges #####
packages <- c("tidyverse","janitor", "lubridate", "chron")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)



# Read in data
metad_21 <- read.csv("./Data/Metadata/Raw_Data/2021_ARUDeployment_Metadata_UMBEL.csv") %>% clean_names
notdep <- metad_21 %>% filter(deployed == "n")
clips_21 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2021_AllCollab_topclips_filteredPB_4-12.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
points_audio <- unique(clips_21$point_id)

notdep %>% filter(point_id %in% points_audio)
# Which ones said they weren't deployed but we still have data on them?
aru_dat_nometa <- setdiff(points_audio, notdep$point_id)
