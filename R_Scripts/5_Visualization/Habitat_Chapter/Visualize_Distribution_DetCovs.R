# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Visualize Distributions of Detection Covariates ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This is a script that takes the visualization parts of Fit_JAGS_DetModFinal2_HabChap and puts them into a separate script so that I can create and save plots of the covariates

# Created 8/27/2024 
# Last modified 8/27/2024

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Setup ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggdist)
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Histograms of Distributions ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
full_dat_us <- readRDS(file ="C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Full_DetectionData_JAGSModels_HabChap.Rdata")

# Veg density
hist(full_dat_us$veg_density_avg, xlab = "Average Composite Density at a Site", main = "Histogram of Vegetation Density", col = palette_5[1])
#hist(scale(full_dat_us$veg_density_avg), xlab = "Average Composite Density at a Site", main = "Scaled Histogram of Vegetation Density", col = palette_5[1])
mean(full_dat_us$veg_density_avg, na.rm = TRUE)
sd(full_dat_us$veg_density_avg, na.rm = TRUE)

# average dB
db <- full_dat_us[,8:13]
db_long <- db %>% pivot_longer(cols = c(backdb_survey1,backdb_survey2,backdb_survey3,backdb_survey4,backdb_survey5,backdb_survey6), names_to = "survey_period",values_to = "db" )
hist(db_long$db, xlab = "Background dB", main = "Histogram of Background Noise", col = palette_5[2])
mean(db_long$db, na.rm = TRUE)
sd(db_long$db, na.rm = TRUE)

# Date
dates_us <- full_dat_us[,21:26]
dates_us_long <- dates_us %>% pivot_longer(cols = c(start_date_s1,start_date_s2,start_date_s3,start_date_s4,start_date_s5,start_date_s6), names_to = "survey_period",values_to = "date" )
min(dates_us_long$date) # -1.46
max(dates_us_long$date) # 1.46
hist(dates_us_long$date, xlab = "Date", main = "Histogram of Survey Start Date", col = cuckoo_palette[1])

# Effort
efforts_us <- full_dat_us[,15:20]
efforts_us_long <- efforts_us %>% pivot_longer(cols = c(effort_survey1,effort_survey2,effort_survey3,effort_survey4,effort_survey5,effort_survey6), names_to = "survey_period",values_to = "effort" )
hist(efforts_us_long$effort, xlab = "Effort", main = "Histogram of Survey Effort", col = cuckoo_palette[1])
mean(efforts_us_long$effort, na.rm = TRUE)
sd(efforts_us_long$effort, na.rm = TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read data and format scaled data with detections ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in scaled data
full_dat <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_SCALED_8-12.csv")

survey_dat <- full_dat %>% select(2:7, 24:29,31:42)
# first select the data columns of interest
dates <- full_dat %>% select(site_id, 37:42)
effort <- full_dat %>% select(site_id, 31:36)
backdb <- full_dat %>% select(site_id, 24:29)

# Then for each sub dataset, pivot longer, rename to s1,s2, etc and unite with site_id for site_survey
dates_long <- dates %>% pivot_longer(cols = c(start_date_s1,
                                              start_date_s2,
                                              start_date_s3,
                                              start_date_s4,
                                              start_date_s5,
                                              start_date_s6), names_to = "survey_period",values_to = "date") 
dates_long <- dates_long %>% mutate(survey_period = case_when(survey_period == "start_date_s1" ~ "s1",
                                                              survey_period == "start_date_s2" ~ "s2",
                                                              survey_period == "start_date_s3" ~ "s3",
                                                              survey_period == "start_date_s4" ~ "s4",
                                                              survey_period == "start_date_s5" ~ "s5",
                                                              survey_period == "start_date_s6" ~ "s6")) 
dates_long <- dates_long %>% unite(site_survey, c("site_id", "survey_period"), sep = "_")

effort_long <- effort %>% pivot_longer(cols = c(effort_survey1,
                                                effort_survey2,
                                                effort_survey3,
                                                effort_survey4,
                                                effort_survey5,
                                                effort_survey6), names_to = "survey_period",values_to = "effort") 
effort_long <- effort_long %>% mutate(survey_period = case_when(survey_period == "effort_survey1" ~ "s1",
                                                                survey_period == "effort_survey2" ~ "s2",
                                                                survey_period == "effort_survey3" ~ "s3",
                                                                survey_period == "effort_survey4" ~ "s4",
                                                                survey_period == "effort_survey5" ~ "s5",
                                                                survey_period == "effort_survey6" ~ "s6")) 
effort_long <- effort_long %>% unite(site_survey, c("site_id", "survey_period"), sep = "_")


backdb_long <- backdb %>% pivot_longer(cols = c(backdb_survey1,
                                                backdb_survey2,
                                                backdb_survey3,
                                                backdb_survey4,
                                                backdb_survey5,
                                                backdb_survey6), names_to = "survey_period",values_to = "backdb") 
backdb_long <- backdb_long %>% mutate(survey_period = case_when(survey_period == "backdb_survey1" ~ "s1",
                                                                survey_period == "backdb_survey2" ~ "s2",
                                                                survey_period == "backdb_survey3" ~ "s3",
                                                                survey_period == "backdb_survey4" ~ "s4",
                                                                survey_period == "backdb_survey5" ~ "s5",
                                                                survey_period == "backdb_survey6" ~ "s6")) 
backdb_long <- backdb_long %>% unite(site_survey, c("site_id", "survey_period"), sep = "_")

# Format detection data long as well 
dets <- full_dat %>% select(1:7)
det_long <- dets %>% pivot_longer(cols = c(det_s1,
                                           det_s2,
                                           det_s3,
                                           det_s4,
                                           det_s5,
                                           det_s6), names_to = "survey_period", values_to = "det")
det_long <- det_long %>% mutate(survey_period = case_when(survey_period == "det_s1" ~ "s1",
                                                          survey_period == "det_s2" ~ "s2",
                                                          survey_period == "det_s3" ~ "s3",
                                                          survey_period == "det_s4" ~ "s4",
                                                          survey_period == "det_s5" ~ "s5",
                                                          survey_period == "det_s6" ~ "s6")) 
det_long <- det_long %>% unite(site_survey, c("site_id", "survey_period"), sep = "_")

# Unite all of them into one dataframe 
dat_long <- det_long %>% left_join(dates_long, by = "site_survey") %>% left_join(effort_long, by = "site_survey") %>% left_join(backdb_long, by = "site_survey")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create Plots - Scaled Data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
veg <- full_dat %>% select(2:7, veg_density_avg_scaled)
veg$det <- apply(veg[,1:6], 1, max, na.rm = TRUE)

vegplot <- ggplot(veg) +
  geom_jitter(aes(x = veg_density_avg_scaled, y = det), color = palette_5[1], width = 0, height = .05) +
  geom_smooth(aes(x = veg_density_avg_scaled, y = det), color = palette_5[1],se = FALSE, method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Vegetation Density", y = "Detection") + theme_minimal()

dbplot <- ggplot(dat_long) +
  geom_jitter(aes(x = backdb, y = det), color = palette_5[2], width = 0, height = .05) +
  geom_smooth(aes(x = backdb, y = det), color = palette_5[2], se = FALSE, method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Background Noise", y = "Detection") + theme_minimal()

effortplot <- ggplot(dat_long) +
  geom_jitter(aes(x = effort, y = det), color = palette_5[3], width = .05, height = .05) +
  geom_smooth(aes(x = effort, y = det), color = palette_5[3], se = FALSE, method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Survey Effort", y = "Detection") + theme_minimal()

dateplot <- ggplot(dat_long) +
  geom_jitter(aes(x = date, y = det), color = palette_5[4], width = .05, height = .05) +
  geom_smooth(aes(x = date, y = det), color = palette_5[5], se = FALSE, method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Date", y = "Detection") + theme_minimal()

vegplot + dbplot

effortplot + dateplot




