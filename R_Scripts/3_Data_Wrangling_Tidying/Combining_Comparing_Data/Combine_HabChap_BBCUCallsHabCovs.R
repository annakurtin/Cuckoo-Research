# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Join days with calls data and habitat covariates, and scale them for use in 
# the linear model 

# date created 8/15/2023
# last edited 9/26/2023
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)



# ORIGINAL: all days #####
# Read in percent days with calls
calls_22 <- read.csv("./Data/Detection_History/2022_All_ARUs/Outputs/2022_Sites_DayswCalling_8-15.csv")
calls_23 <- read.csv("./Data/Detection_History/2023_All_ARUs/Outputs/2023_Sites_DayswCalling_8-15.csv")
# Read in unscaled habitat covariates
habcovs_22 <- read.csv("./Data/Habitat_Model_Covariates/Occupancy_Covariates/All_2022Veg_Covariates_9-4.csv")
habcovs_23 <- read.csv("./Data/Habitat_Model_Covariates/Occupancy_Covariates/All_2023Veg_Covariates_9-4.csv") %>% select(-grts_grouped)

# Join each year
dat_22 <- left_join(calls_22, habcovs_22, by = "site_id")
dat_23 <- left_join(calls_23, habcovs_23, by = "site_id")

# Check that they are the same columns and uniquely identifyable
# looks good

# Rbind them into one dataframe
lm_dat <- rbind(dat_22,dat_23)

# pull out the mean and sd values for use in linear model (LMMod11_GamPois)
mean(lm_dat$veg_sd_resid, na.rm = TRUE)
sd(lm_dat$veg_sd_resid, na.rm = TRUE)
mean(lm_dat$pct_openshrub_landsc, na.rm = TRUE)
sd(lm_dat$pct_openshrub_landsc, na.rm = TRUE)
mean(lm_dat$pct_can_landsc, na.rm = TRUE)
sd(lm_dat$pct_can_landsc, na.rm = TRUE)
mean(lm_dat$pct_openshrub_core, na.rm = TRUE)
sd(lm_dat$pct_openshrub_core, na.rm = TRUE)

# Scale relevant covariates
dat1 <- lm_dat[,1:12]
dat2 <- round(scale(lm_dat[,13:25]),2)
all_scaled <- cbind(dat1,dat2)

# Export for use in linear model 
#write.csv(all_scaled, "./Data/Habitat_Model_Covariates/Linear_Model/HabChap_22-23_DaysCalls_HabCovs_9-12.csv", row.names = FALSE)



# VERSION 1: july 1-15 period ####
calls_23 <- read.csv("./Data/Detection_History/2023_All_ARUs/Outputs/2023_Sites_DayswCalling_July1-15_9-26.csv")
# make a year column and a site_year column
calls_23$year <- "23"
calls_23 <- calls_23 %>% unite(site_yr,c("site_id", "year"),sep = "_",remove = FALSE)
calls_22 <- read.csv("./Data/Detection_History/2022_All_ARUs/Outputs/2022_Sites_DayswCalling_July1-15_9-26.csv")
# make a year column and a site_year column
calls_22$year <- "22"
calls_22 <- calls_22 %>% unite(site_yr,c("site_id", "year"),sep = "_",remove = FALSE)

# Read in unscaled habitat covariates
habcovs_22 <- read.csv("./Data/Habitat_Model_Covariates/Occupancy_Covariates/All_2022Veg_Covariates_9-4.csv")
habcovs_23 <- read.csv("./Data/Habitat_Model_Covariates/Occupancy_Covariates/All_2023Veg_Covariates_9-4.csv") %>% select(-grts_grouped)

# Join each year
dat_22 <- left_join(calls_22, habcovs_22, by = "site_id")
dat_23 <- left_join(calls_23, habcovs_23, by = "site_id")

# Check that they are the same columns and uniquely identifyable
# looks good

# Rbind them into one dataframe
lm_dat <- rbind(dat_22,dat_23)

# pull out the mean and sd values for use in linear model (LMMod11_GamPois)
mean(lm_dat$veg_sd_resid, na.rm = TRUE)
sd(lm_dat$veg_sd_resid, na.rm = TRUE)
mean(lm_dat$pct_openshrub_landsc, na.rm = TRUE)
sd(lm_dat$pct_openshrub_landsc, na.rm = TRUE)
mean(lm_dat$pct_can_landsc, na.rm = TRUE)
sd(lm_dat$pct_can_landsc, na.rm = TRUE)
mean(lm_dat$pct_openshrub_core, na.rm = TRUE)
sd(lm_dat$pct_openshrub_core, na.rm = TRUE)
mean(lm_dat$combined_days_rec,na.rm = TRUE)
sd(lm_dat$combined_days_rec,na.rm = TRUE)

# Scale relevant covariates
dat1 <- lm_dat[,1:13]
dat2 <- round(scale(lm_dat[,14:26]),2)
# also scale combined recording days
all_scaled <- cbind(dat1,dat2)
scale_alldays <- scale(all_scaled$combined_days_rec)
all_scaled$combined_days_rec <- round(scale_alldays[,1],2)

# Export for use in linear model 
#write.csv(all_scaled, "./Data/Habitat_Model_Covariates/Linear_Model/DaysCalls_14DayPer_HabCovsSCALED_22-23_9-26.csv", row.names = FALSE)
# Export unscaled data for other uses
#write.csv(lm_dat, "./Data/Habitat_Model_Covariates/Linear_Model/DaysCalls_14DayPer_HabCovsUNSCALED_22-23_9-26.csv", row.names = FALSE)







# VERSION 2: 20 day period ####
# Also derive june 25- July 15th to see if it makes a difference
calls22_20 <- read.csv("./Data/Detection_History/2022_All_ARUs/Outputs/2022_Sites_DayswCalling_Jun25-Jul15_9-26.csv")
# make a year column and a site_year column
calls22_20$year <- "22"
calls22 <- calls22_20 %>% unite(site_yr,c("site_id", "year"),sep = "_",remove = FALSE)
calls23_20 <- read.csv("./Data/Detection_History/2023_All_ARUs/Outputs/2023_Sites_DayswCalling_Jun25-Jul15_9-26.csv")
# make a year column and a site_year column
calls23_20$year <- "23"
calls23 <- calls23_20 %>% unite(site_yr,c("site_id", "year"),sep = "_",remove = FALSE)

# Read in unscaled habitat covariates
habcovs_22 <- read.csv("./Data/Habitat_Model_Covariates/Occupancy_Covariates/All_2022Veg_Covariates_9-4.csv")
habcovs_23 <- read.csv("./Data/Habitat_Model_Covariates/Occupancy_Covariates/All_2023Veg_Covariates_9-4.csv") %>% select(-grts_grouped)

# Join each year
dat_22 <- left_join(calls22, habcovs_22, by = "site_id")
dat_23 <- left_join(calls23, habcovs_23, by = "site_id")

# Check that they are the same columns and uniquely identifyable
# looks good

# Rbind them into one dataframe
lm_dat <- rbind(dat_22,dat_23)

# pull out the mean and sd values for use in linear model (LMMod11_GamPois)
mean(lm_dat$veg_sd_resid, na.rm = TRUE)
sd(lm_dat$veg_sd_resid, na.rm = TRUE)
mean(lm_dat$pct_openshrub_landsc, na.rm = TRUE)
sd(lm_dat$pct_openshrub_landsc, na.rm = TRUE)
mean(lm_dat$pct_can_landsc, na.rm = TRUE)
sd(lm_dat$pct_can_landsc, na.rm = TRUE)
mean(lm_dat$pct_openshrub_core, na.rm = TRUE)
sd(lm_dat$pct_openshrub_core, na.rm = TRUE)
mean(lm_dat$combined_days_rec,na.rm = TRUE)
sd(lm_dat$combined_days_rec,na.rm = TRUE)

# Scale relevant covariates
dat1 <- lm_dat[,1:13]
dat2 <- round(scale(lm_dat[,14:26]),2)
all_scaled <- cbind(dat1,dat2)
# also scale combined recording days
scale_alldays <- scale(all_scaled$combined_days_rec)
all_scaled$combined_days_rec <- round(scale_alldays[,1],2)

# Export for use in linear model 
#write.csv(all_scaled, "./Data/Habitat_Model_Covariates/Linear_Model/DaysCalls_20DayPer_HabCovsSCALED_22-23_9-26.csv", row.names = FALSE)
#write.csv(lm_dat, "./Data/Habitat_Model_Covariates/Linear_Model/DaysCalls_20DayPer_HabCovsUNSCALED_22-23_9-26.csv", row.names = FALSE)
