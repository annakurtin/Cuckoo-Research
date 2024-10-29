### Combine Background dB Estimates for Linear Model ####

packages <- c("tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")
load_packages(packages)


##### Code #####
## Read in data
# 23
umbel_23 <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/Background_Noise_Bandpass/2023_UMBEL_Background_Noise.csv") %>% clean_names()
fwpr7_23 <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/Background_Noise_Bandpass/2023_FWPR7_Background_Noise.csv") %>% clean_names()
fwpr6_23 <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/Background_Noise_Bandpass/2023_FWPR6_Background_Noise.csv") %>% clean_names()
fwpr5_23 <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/Background_Noise_Bandpass/2023_FWPR5_Background_Noise.csv") %>% clean_names()
# Combine them together
all_back23 <- rbind(umbel_23,fwpr7_23,fwpr6_23,fwpr5_23)
# remove the file headers
all_back23 <- all_back23 %>% filter(!file_name == "File_Name")%>% separate(file_name, into = c("point_id","date","hour_ext"), sep = "_") %>% mutate(year = "23") %>% create_site_col()
all_back23 <- all_back23 %>% mutate(date = as.Date(date, format = "%Y%m%d"))

# 22
umbel_22 <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/Background_Noise_Bandpass/2022_UMBEL_Background_Noise.csv") %>% clean_names()
fwpr7_22 <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/Background_Noise_Bandpass/2022_FWPR7_Background_Noise.csv") %>% clean_names()
fwpr6_22 <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/Background_Noise_Bandpass/2022_FWPR6_Background_Noise.csv") %>% clean_names()
fwpr5_22 <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/Background_Noise_Bandpass/2022_FWPR5_Background_Noise.csv") %>% clean_names()
# Combine them together
all_back22 <- rbind(umbel_22,fwpr7_22,fwpr6_22,fwpr5_22) 
all_back22 <- all_back22 %>% filter(!file_name == "File_Name") %>% separate(file_name, into = c("point_id","date","hour_ext"), sep = "_") %>% mutate(year = "22") %>% create_site_col()
all_back22 <- all_back22 %>% mutate(date = as.Date(date, format = "%Y%m%d"))

# Join these together 
all_back <- rbind(all_back22,all_back23)
# convert the average db column to numeric
all_back$db <- as.numeric(all_back$average_d_b)


## Trim these to just the dates between July 1st and 15th 
all_back <- all_back %>% filter((date > as.Date("2022-06-30") & date < as.Date("2022-07-15")) | (date > as.Date("2023-06-30") & date < as.Date("2023-07-15")))


## Average out the values for each site
backdb_avg <- all_back %>% group_by(year,site_id) %>% summarize(avg_db = mean(db, na.rm = TRUE))
backdb_avg$avg_db <- round(backdb_avg$avg_db,2)


## Write this data
# Write it as unscaled data 
write.csv(backdb_avg,"./Data/Habitat_Model_Covariates/Detection_Covariates/BackNoiseBandpass_22-23_14DayPer.csv", row.names = FALSE)
