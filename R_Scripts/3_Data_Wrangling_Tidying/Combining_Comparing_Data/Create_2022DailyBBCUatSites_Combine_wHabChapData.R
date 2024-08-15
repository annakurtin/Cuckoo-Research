# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Format data for us in linear/poisson models ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(ggplot2)
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Format days with calls 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Extract number of days with calls from cleaned annotation data 
cnn_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPBYBCU_5-27.csv")
cnn_22$date_formatted <- as.Date(cnn_22$date_formatted)

# Get the number of days per site
det1 <- cnn_22 %>% group_by(site_id,date) %>% summarize(bbcu = max(annotation))

det2 <- det1 %>% group_by(site_id) %>% summarize(days_wcall = sum(bbcu))
# Old - only looking at sites with positives
#pos_only <- det2 %>% filter(days_wcall > 0)
# this looks good

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Look at ARU table
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aru_table <- readRDS("./Data/Detection_History/2022_All_ARUs/Outputs/2022_DeployPeriodTable_CorrectedEffort_ClipMaskedFinalRec_8-15.RData")
# create site column
aru_table <- create_site_col(aru_table)
# filter only sites in pos_only
aru_table <- aru_table %>% filter(site_id %in% det2$site_id) 
# were there any problem periods within these? yes. I'll be removing these, since they don't count towards the total days the aru was on/surveying since even if a cuckoo called, it wouldn't have been picked up.
# change posix to dates
aru_table <- aru_table %>% mutate(first_date = as.Date(first_rec),
                                  last_date = as.Date(last_rec), 
                                  prob_start = as.Date(Problem1_from), 
                                  prob_end = as.Date(Problem1_to))
aru_table <- aru_table %>% mutate(days_dep = last_date - first_date,
                                  days_prob = prob_end - prob_start)
aru_table <- aru_table %>% mutate(days_dep = as.numeric(days_dep),
                             days_prob = as.numeric(days_prob))
aru_table <- aru_table %>% mutate(days_prob = ifelse(is.na(days_prob), 0, days_prob))
aru_table <- aru_table %>% mutate(total_days_rec =(days_dep - days_prob))
# group by site ID, take the earliest start date and latest end date
aru_sites <- aru_table %>% group_by(site_id) %>% summarize(days_rec = max(total_days_rec))

# What is the distribution of this data?
hist(aru_sites$days_rec)
mean(aru_sites$days_rec)
sd(aru_sites$days_rec)
# This has a smaller average days of recording (50 rather than 73, and the monitors were deployed later. This might affect the percentage of calls at each site, but having an effect for year should account for this)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Join data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# join with pos_cuckoo
dat <- left_join(det2, aru_sites, by = "site_id")
# divide days with cuckoo by total days recording to get percentage of days with cuckoo
dat_fin <- dat %>% mutate(pct_days_wcall = round((days_wcall/days_rec)*100,1), 
                          year = "2022")

# What is the distribution of the data?
hist(dat_fin$pct_days_wcall)
# large right skew to this data - Vlad mentioned he had a similar thing. Is this ok for the assumptions of linear models?
pos_dat <- dat_fin[dat_fin$pct_days_wcall > 0,]
hist(pos_dat$pct_days_wcall)


# write this to csv
#write.csv(dat_fin, "./Data/Detection_History/2022_All_ARUs/Outputs/2022_Sites_DayswCalling_8-15.csv", row.names = FALSE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine this data with habitat covariates for use in the model 
# OLD - I need to combine them before I scale them 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#habcovs <- read.csv("./Data/Habitat_Model_Covariates/Occupancy_Covariates/All_2022VegCovs_Scaled_8-15.csv")
#bbcu_days_habcovs <- left_join(dat_fin, habcovs, by = "site_id")
# write this for use in the poisson model
#write.csv(bbcu_days_habcovs, "./Data/Habitat_Model_Covariates/Linear_Model/HabChap_PoissonMod_DaysBBCU_HavCovs.csv", row.names = FALSE)
