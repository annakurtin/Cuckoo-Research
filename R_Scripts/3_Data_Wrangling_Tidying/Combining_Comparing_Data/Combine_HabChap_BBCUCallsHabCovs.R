# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Join days with calls data and habitat covariates, and scale them for use in 
# the linear model 

# date created 8/15/2023
# last edited 8/15/2023
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)


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

# Scale relevant covariates
dat1 <- lm_dat[,1:12]
dat2 <- round(scale(lm_dat[,13:25]),2)
all_scaled <- cbind(dat1,dat2)

# Export for use in linear model 
#write.csv(all_scaled, "./Data/Habitat_Model_Covariates/Linear_Model/HabChap_22-23_DaysCalls_HabCovs_9-12.csv", row.names = FALSE)
