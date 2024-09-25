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
# Extract number of days with calls from cleaned annotation data 
cnn_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_5-27.csv")
cnn_23$date_formatted <- as.Date(cnn_23$date_formatted)

# Get the number of days per site
det1 <- cnn_23 %>% group_by(site_id,date) %>% summarize(bbcu = max(annotation))
# Looking at distribution of days with calling
look1 <- det1 %>% group_by(date) %>% summarize(num_calls = sum(bbcu))
look1$date <- as.character(look1$date)
look1$date_f <- as.Date(look1$date, format = "%Y%m%d")
look1 %>% ggplot() + 
  geom_bar(aes(x = date_f, y = num_calls), stat = "identity")


det2 <- det1 %>% group_by(site_id) %>% summarize(days_wcall = sum(bbcu))
#Old - only looking at sites with positives
#pos_only <- det2 %>% filter(days_wcall > 0)
# this looks good

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Look at ARU table
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aru_table <- readRDS("./Data/Detection_History/2023_All_ARUs/Outputs/2023_DeployPeriodTable_CorrectedEffort_ClipMaskedFinalRec_4-29.RData")
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Join data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# join with pos_cuckoo
dat <- left_join(det2, aru_sites, by = "site_id")
# divide days with cuckoo by total days recording to get percentage of days with cuckoo
dat_fin <- dat %>% mutate(pct_days_wcall = round((days_wcall/days_rec)*100,1), 
                          year = "2023") %>% select(-grts_grouped)

# What is the distribution of the data?
hist(dat_fin$pct_days_wcall)
# large right skew to this data - Vlad mentioned he had a similar thing. Is this ok for the assumptions of linear models?
pos_dat <- dat_fin[dat_fin$pct_days_wcall > 0,]
hist(pos_dat$pct_days_wcall)


# write this to csv
#write.csv(dat_fin, "./Data/Detection_History/2023_All_ARUs/Outputs/2023_Sites_DayswCalling_8-15.csv", row.names = FALSE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Only Dates During Peak Time  #####
#### First trying July 1 - July 15th 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Format days with calls 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Establish the cutoffs for the sub period you're looking at 
start_period <- as.Date("2023-07-01")
end_period <- as.Date("2023-07-14")

# Extract number of days with calls from cleaned annotation data 
cnn_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_5-27.csv")
cnn_23$date_formatted <- as.Date(cnn_23$date_formatted)
## Filter out only dates within period specified 
cnn_23 <- cnn_23[cnn_23$date_formatted <= end_period & cnn_23$date_formatted >= start_period,]
# See how many unique dates are within each of these
#test <- cnn_23 %>% group_by(site_id) %>% summarize(num_dates = length(unique(date_formatted)))
#test2 <- cnn_23 %>% filter(site_id == "MISO-174")
# Get the number of days with calls per site
det1 <- cnn_23 %>% group_by(site_id,date) %>% summarize(bbcu = max(annotation))
det2 <- det1 %>% group_by(site_id) %>% summarize(days_wcall = sum(bbcu))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Look at ARU table
## Generate a number of combined days monitored at the site (include this as a covariate)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aru_table <- readRDS("./Data/Detection_History/2023_All_ARUs/Outputs/2023_DeployPeriodTable_CorrectedEffort_ClipMaskedFinalRec_4-29.RData")
# create site column
aru_table <- create_site_col(aru_table)
# create sampling ID column
#aru_table <- create_samp_col(aru_table)
# filter only sites in det data
aru_table <- aru_table %>% filter(site_id %in% det2$site_id) 
# were there any problem periods within these? yes. I'll be removing these, since they don't count towards the total days the aru was on/surveying since even if a cuckoo called, it wouldn't have been picked up.
# change posix to dates
aru_table <- aru_table %>% mutate(first_date = as.Date(first_rec),
                                  last_date = as.Date(last_rec), 
                                  prob_start = as.Date(Problem1_from), 
                                  prob_end = as.Date(Problem1_to))
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
aru_t3 <- aru_t2 %>% group_by(site_id) %>% summarize(max_days_rec = max(total_days), combined_days_rec = sum(total_days))#, sampling_design = first(sampling_design))
# Join this back to det2

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Join data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# join with pos_cuckoo
dat <- left_join(det2, aru_t3, by = "site_id")
# Remove any sites that have less than a full recording period
dat_fin <- dat %>% filter(max_days_rec == 13)
# Convert to numeric
dat_fin$max_days_rec <- as.numeric(dat_fin$max_days_rec)
dat_fin$combined_days_rec <- as.numeric(dat_fin$combined_days_rec)

# What is the distribution of the data?
hist(dat_fin$days_wcall)
# large right skew to this data - Vlad mentioned he had a similar thing. Is this ok for the assumptions of linear models?
pos_dat <- dat_fin[dat_fin$days_wcall > 0,] #13 sites with BBCU our of 99
hist(pos_dat$days_wcall)


# write this to csv
write.csv(dat_fin, "./Data/Detection_History/2023_All_ARUs/Outputs/2023_Sites_DayswCalling_July1-15_9-25.csv", row.names = FALSE)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine this data with habitat covariates for use in the model #####
# OLD - I need to combine them before I scale them 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#habcovs <- read.csv("./Data/Habitat_Model_Covariates/Occupancy_Covariates/All_2023VegCovs_Scaled_8-12.csv")
#bbcu_days_habcovs <- left_join(dat_fin, habcovs, by = "site_id")
# write this for use in the poisson model
#write.csv(bbcu_days_habcovs, "./Data/Habitat_Model_Covariates/Linear_Model/HabChap_2023_DaysBBCU_HabCovs.csv", row.names = FALSE)

#### Graveyard #####
# aru_table <- aru_table %>% mutate(days_dep = last_date - first_date,
#                                   days_prob = prob_end - prob_start)
# aru_table <- aru_table %>% mutate(days_dep = as.numeric(days_dep),
#                                   days_prob = as.numeric(days_prob))
# aru_table <- aru_table %>% mutate(days_prob = ifelse(is.na(days_prob), 0, days_prob))
# aru_table <- aru_table %>% mutate(total_days_rec =(days_dep - days_prob))
# 
# ###???????????????????????????????????????????????????????????????????????????????????????????????????????
# # convert the ARU table - summarize by site to take the max of the start date and the min of the end date
# test <- aru_table %>% group_by(site_id) %>% summarize(first_date = min(first_date),
#                                                       last_date = max(last_date))
# # how to do this and filter out problem periods? Maybe we don'tneed to 
# 
# # Filter out the data that has problems during the period of interest
# probs <- aru_table %>% filter((prob_start <= end_period & prob_start >= start_period) | 
#                                 (prob_end <= end_period & prob_end >= start_period) | 
#                                 (first_date <= end_period & first_date >= start_period) |
#                                 (last_date <= end_period & last_date >= start_period)) %>% select(site_id)
# # loss of three sites
# test <- aru_table %>% filter(!site_id %in% probs$site_id)
# ###???????????????????????????????????????????????????????????????????????????????????????????????????????
# 
# # group by site ID, take the earliest start date and latest end date
# aru_sites <- aru_table %>% group_by(site_id) %>% summarize(days_rec = max(total_days_rec))
# 
# # What is the distribution of this data?
# hist(aru_sites$days_rec)
# mean(aru_sites$days_rec)
# sd(aru_sites$days_rec)
