# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Format data for us in linear/poisson models ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(ggplot2)
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### All Dates Between June 1-Aug 15th #####
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



#### Only Dates During Peak Time  #####
## First trying one two week period July 1 - July 15th 
#### This removes six sites in 2022
## 10 positives out of 35 sites
## Next trying two two week periods June 17th - July 14th
#### This removes all sites in 2022
## Next trying two ten-day periods??  June 25th - July 15th 
## This removes 21 sites out of 41 sites 
## 4 positives out of 20 sites in data
# Should we prioritize 2023 data since we have more remote sensing data on this?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Format days with calls 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Establish the cutoffs for the sub period you're looking at 
# Old: start_period <- as.Date("2022-06-25")
start_period <- as.Date("2022-07-01")
end_period <- as.Date("2022-07-15")

#### Extract number of days with calls from cleaned annotation data 
cnn_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPBYBCU_5-27.csv")
cnn_22$date_formatted <- as.Date(cnn_22$date_formatted)
## Filter out only dates within period specified 
cnn_22 <- cnn_22[cnn_22$date_formatted <= end_period & cnn_22$date_formatted >= start_period,]
# Get the number of days per site
det1 <- cnn_22 %>% group_by(site_id,date) %>% summarize(bbcu = max(annotation))
test <- det1 %>% group_by(site_id) %>% summarize(bbcu = max(bbcu))
sum(test$bbcu)
det2 <- det1 %>% group_by(site_id) %>% summarize(days_wcall = sum(bbcu))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Look at ARU table
## Generate a number of combined days monitored at the site (include this as a covariate)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aru_table <- readRDS("./Data/Detection_History/2022_All_ARUs/Outputs/2022_DeployPeriodTable_CorrectedEffort_ClipMaskedFinalRec_8-15.RData")
# create site column
aru_table <- create_site_col(aru_table)

# filter only sites in det data
aru_table <- aru_table %>% filter(site_id %in% det2$site_id) 
# were there any problem periods within these? yes. I'll be removing these, since they don't count towards the total days the aru was on/surveying since even if a cuckoo called, it wouldn't have been picked up.
# change posix to dates
aru_table <- aru_table %>% mutate(first_date = as.Date(format(first_rec, "%Y-%m-%d")),
                                  last_date = as.Date(format(last_rec, "%Y-%m-%d")), 
                                  prob_start = as.Date(Problem1_from), 
                                  prob_end = as.Date(Problem1_to))
hist(aru_table$first_date, breaks =10) # most of them were put out around June 27th
hist(aru_table$last_date, breaks =10)
# Why is the adding one day?
# Change the start date to be the same as the start period if the first date is before it
aru_t2 <- aru_table %>% mutate(first_date = if_else(first_date <= start_period, start_period, first_date),
                               last_date = case_when(last_date >= end_period ~ end_period,
                                                     last_date <= end_period & last_date >= start_period ~ last_date,
                                                     last_date <= start_period ~ start_period))
# change the problem days into NA if they fall outside the period of concern
# first create a column for whether or not the problem period is in our period of interest
aru_t2 <- aru_t2 %>% mutate(prob_pofi = ifelse((prob_start >= start_period & prob_start <= end_period) |
                                                 (prob_end >= start_period & prob_end <= end_period), "Y","N"))
# then if it doesn't fall within the period of interest, assign it to NA
aru_t2 <- aru_t2 %>% mutate(prob_start = case_when(prob_pofi == "Y" ~ prob_start,
                                                   prob_pofi == "N" ~ NA,
                                                   is.na(prob_pofi)== TRUE ~ NA),
                            prob_end = case_when(prob_pofi == "Y" ~ prob_end,
                                                 prob_pofi == "N" ~ NA,
                                                 is.na(prob_pofi)== TRUE ~ NA))
# Now, change the problem start to be the start period if it falls outside the period of interest
aru_t2 <- aru_t2 %>% mutate(prob_start = if_else(prob_start < start_period, first_date, prob_start),
                            prob_end = if_else(prob_end > end_period, last_date, prob_end))
# calculate days recorded for each point with (last date - first date) - (end problem - start problem)
aru_t2 <- aru_t2 %>% mutate(days_rec = (last_date - first_date), problem_days = (prob_end - prob_start))
# change NA problem days to 0 days for calculation
aru_t2 <- aru_t2 %>% mutate(problem_days = if_else(is.na(problem_days),as.difftime(0, units = "days"), problem_days))
# Combine these
aru_t2 <- aru_t2 %>% mutate(total_days = days_rec - problem_days)
# Group by site and take the max of total_days (just care if one ARU at the site was recording the whole time), also take the sum of the total_days to give a proxy for hours recorded at the site
aru_t3 <- aru_t2 %>% group_by(site_id) %>% summarize(max_days_rec = max(total_days), combined_days_rec = sum(total_days))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Join data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Join this back to det2
dat <- left_join(det2, aru_t3, by = "site_id")
# Remove any sites that have less than a full recording period
full_days <- as.numeric(end_period - start_period)
dat_fin <- dat %>% filter(max_days_rec == full_days)
removed <- dat %>% filter(!max_days_rec == full_days) # Removed 6 points 2022
# Convert to numeric
dat_fin$max_days_rec <- as.numeric(dat_fin$max_days_rec)
dat_fin$combined_days_rec <- as.numeric(dat_fin$combined_days_rec)

# What is the distribution of the data?
hist(dat_fin$days_wcall)
# large right skew to this data - Vlad mentioned he had a similar thing. Is this ok for the assumptions of linear models?
pos_dat <- dat_fin[dat_fin$days_wcall > 0,] #10 sites with BBCU out of 35
hist(pos_dat$days_wcall)


# write this to csv
# two week period
#write.csv(dat_fin, "./Data/Detection_History/2022_All_ARUs/Outputs/2022_Sites_DayswCalling_July1-15_10-30.csv", row.names = FALSE)
# 20 day period
# write.csv(dat_fin, "./Data/Detection_History/2022_All_ARUs/Outputs/2022_Sites_DayswCalling_Jun25-Jul15_9-26.csv", row.names = FALSE)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine this data with habitat covariates for use in the model 
# OLD - I need to combine them before I scale them 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#habcovs <- read.csv("./Data/Habitat_Model_Covariates/Occupancy_Covariates/All_2022VegCovs_Scaled_8-15.csv")
#bbcu_days_habcovs <- left_join(dat_fin, habcovs, by = "site_id")
# write this for use in the poisson model
#write.csv(bbcu_days_habcovs, "./Data/Habitat_Model_Covariates/Linear_Model/HabChap_PoissonMod_DaysBBCU_HavCovs.csv", row.names = FALSE)
