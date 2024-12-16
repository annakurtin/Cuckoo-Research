### Descriptive Stats PAM/PB Detection Models

library(tidyverse)
library(patchwork)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in PB data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pb_dat <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Playback_Results/2023/Outputs/2023_PBData_FormatforOccMod_8-5-24.csv")

# Extract dates and format longer to look at colinearity 
dates <- pb_dat %>% select(site_id, date_s1, date_s2, date_s3)
dates_long <- dates %>% pivot_longer(cols = c(date_s1,date_s2,date_s3), names_to = "survey_period",values_to = "date")
dates_long <- dates_long %>% mutate(survey_period = case_when(survey_period == "date_s1" ~ "s1",
                                                              survey_period == "date_s2" ~ "s2",
                                                              survey_period == "date_s3" ~ "s3"))
dates_long <- unite(dates_long,site_survey,c(site_id, survey_period),sep="_",remove=TRUE)

# Extract wind and format longer to look at colinearity 
wind <- pb_dat %>% select(site_id, wind_s1, wind_s2, wind_s3)
wind_long <- wind %>% pivot_longer(cols = c(wind_s1, wind_s2, wind_s3), names_to = "survey_period", values_to = "wind")
wind_long <- wind_long %>% mutate(survey_period = case_when(survey_period == "wind_s1" ~ "s1",
                                                            survey_period == "wind_s2" ~ "s2",
                                                            survey_period == "wind_s3" ~ "s3"))
wind_long <- unite(wind_long, site_survey,c(site_id,survey_period), sep = "_", remove = TRUE)

# Extract temp and format longer to look at colinearity 
temp <- pb_dat %>% select(site_id, temp_s1, temp_s2, temp_s3)
temp_long <- temp %>% pivot_longer(cols = c(temp_s1, temp_s2, temp_s3), names_to = "survey_period", values_to = "temp")
temp_long <- temp_long %>% mutate(survey_period = case_when(survey_period == "temp_s1" ~ "s1",
                                                            survey_period == "temp_s2" ~ "s2",
                                                            survey_period == "temp_s3" ~ "s3"))
temp_long <- unite(temp_long, site_survey, c(site_id, survey_period), sep = "_", remove = TRUE)

covs_long_pb <- left_join(dates_long, wind_long, by = "site_survey") %>% left_join(., temp_long, by = "site_survey")

# Convert date to date
covs_long_pb$date <- as.Date(covs_long_pb$date, format = "%m/%d/%Y")
covs_long_pb$jdate <-  yday(covs_long_pb$date)

# Convert temp to C
covs_long_pb$temp_c <- round((covs_long_pb$temp - 32)*(5/9),0)

# Get summary statistics
pb1 <- summary(covs_long_pb)
pb2 <- covs_long_pb %>% summarize(sd_temp = sd(temp_c,na.rm = TRUE),
                                  sd_jday = sd(jdate, na.rm = TRUE),
                                 sd_wind = sd(wind, na.rm = TRUE))
pb3 <- covs_long_pb %>% summarize(min_date = min(jdate, na.rm = TRUE),
                                 max_date = max(jdate, na.rm = TRUE),
                                 med_date = median(jdate, na.rm = TRUE))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in PAM data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
full_dat <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Full_DetectionData_JAGSModels_HabChap.Rdata")
# Pivot this data long to be able to compare 
dates <- select(full_dat, site_id, 23:28)
dates_long <- dates %>% pivot_longer(cols = c(start_date_s1,
                                              start_date_s2,
                                              start_date_s3,
                                              start_date_s4,
                                              start_date_s5,
                                              start_date_s6), names_to = "survey_period",values_to = "date" )
# rename to s1, s2, etc
dates_long <- dates_long %>% mutate(survey_period = case_when(survey_period == "start_date_s1" ~ "s1",
                                                              survey_period == "start_date_s2" ~ "s2",
                                                              survey_period == "start_date_s3" ~ "s3",
                                                              survey_period == "start_date_s4" ~ "s4",
                                                              survey_period == "start_date_s5" ~ "s5",
                                                              survey_period == "start_date_s6" ~ "s6"))
# combine
dates_long <- unite(dates_long,site_survey,c(site_id, survey_period),sep="_",remove=TRUE)

db <- select(full_dat, site_id,8:13)
db_long <- db %>% pivot_longer(cols = c(backdb_survey1,
                                        backdb_survey2,
                                        backdb_survey3,
                                        backdb_survey4,
                                        backdb_survey5,
                                        backdb_survey6), names_to = "survey_period",values_to = "db" )
db_long <- db_long %>% mutate(survey_period = case_when(survey_period == "backdb_survey1" ~ "s1",
                                                        survey_period == "backdb_survey2" ~ "s2",
                                                        survey_period == "backdb_survey3" ~ "s3",
                                                        survey_period == "backdb_survey4" ~ "s4",
                                                        survey_period == "backdb_survey5" ~ "s5",
                                                        survey_period == "backdb_survey6" ~ "s6"))
db_long <- unite(db_long,site_survey,c(site_id, survey_period),sep="_",remove=TRUE)

effort <- select(full_dat, site_id, 15:20)
effort_long <- effort %>% pivot_longer(cols = c(effort_survey1,
                                                effort_survey2,
                                                effort_survey3,
                                                effort_survey4,
                                                effort_survey5,
                                                effort_survey6), names_to = "survey_period",values_to = "effort" )
effort_long <- effort_long %>% mutate(survey_period = case_when(survey_period == "effort_survey1" ~ "s1",
                                                                survey_period == "effort_survey2" ~ "s2",
                                                                survey_period == "effort_survey3" ~ "s3",
                                                                survey_period == "effort_survey4" ~ "s4",
                                                                survey_period == "effort_survey5" ~ "s5",
                                                                survey_period == "effort_survey6" ~ "s6"))
effort_long <- unite(effort_long,site_survey,c(site_id, survey_period),sep="_",remove=TRUE)

covs_long_pam <- left_join(dates_long, db_long, by = "site_survey") %>% left_join(., effort_long, by = "site_survey")

# Get summary statistics
pam1 <- summary(covs_long_pam)
pam2 <- covs_long_pam %>% summarize(sd_date = sd(date,na.rm = TRUE), 
                                   sd_db = sd(db,na.rm = TRUE), 
                                 sd_eff = sd(effort, na.rm = TRUE))
pam3 <- covs_long_pb %>% summarize(min_date = min(date, na.rm = TRUE),
                                 max_date = max(date, na.rm = TRUE),
                                 med_date = median(date, na.rm = TRUE))

veg <- full_dat$veg_density_avg

summary(veg)
sd(veg, na.rm = TRUE)
