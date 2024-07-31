##### Format data for us in linear/poisson models ####
library(tidyverse)
library(ggplot2)
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")

#### Extract number of days with calls from cleaned annotation data ####
cnn_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_5-27.csv")
cnn_23$date_formatted <- as.Date(cnn_23$date_formatted)

# Get the number of days per site
det1 <- cnn_23 %>% group_by(site_id,date) %>% summarize(bbcu = max(annotation))

det2 <- det1 %>% group_by(site_id) %>% summarize(days_wcall = sum(bbcu))

pos_only <- det2 %>% filter(days_wcall > 0)
# this looks good

# should I create a percentage of days with call in this data?
aru_table <- readRDS("./Data/Detection_History/2023_All_ARUs/Outputs/2023_DeployPeriodTable_CorrectedEffort_ClipMaskedFinalRec_4-29.RData")
# create site column
aru_table <- create_site_col(aru_table)
# filter only sites in pos_only
aru_table <- aru_table %>% filter(site_id %in% pos_only$site_id) 
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

# join with pos_cuckoo
pos_aru <- left_join(pos_only, aru_sites, by = "site_id")
# divide days with cuckoo by total days recording to get percentage of days with cuckoo
pos_aru <- pos_aru %>% mutate(pct_days_wcall = round((days_wcall/days_rec)*100,1))

# write this to csv
#write.csv(pos_aru, "./Data/Detection_History/2023_All_ARUs/Outputs/2023_SiteswBBCU_DayswCalling_7-31.csv", row.names = FALSE)


#### Combine this data with habitat covariates for use in the model #####
habcovs <- read.csv("./Data/Habitat_Model_Covariates/Occupancy_Covariates/All_VegCovs_Scaled_7-24.csv")
bbcu_days_habcovs <- left_join(pos_aru, habcovs, by = "site_id")
# write this for use in the poisson model
#write.csv(bbcu_days_habcovs, "./Data/Habitat_Model_Covariates/Linear_Model/HabChap_PoissonMod_DaysBBCU_HavCovs.csv", row.names = FALSE)
