####### Check Scores Outputs ########

# A script to read in the scores output from the clip extraction 

# Created 1/23/2023
# last modified 1/23/2023

#### Setup ####
packages <- c("tidyverse","janitor", "ggplot2")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)
# Read in cuckoo color palette
source("./R_Scripts/5_Visualization/Create_CuckooColor_HexCodes.R")

#### Code #####
fwpr6_scores <- read.csv('F:/Cuckoo_Acoustic_Data/2023/2023_FWPR6_Data/2023_FWPR6_Clips/2023_FWPR6_topclip_perperiod/2023_FWPR6_topclips_perSiteperPeriod.csv')
# 7378 clips = 36,890 seconds = 10 hours
test1 <- fwpr6_scores %>% filter(!(score < -1))
# filtering out those with a score less than -1, there are 4302 (.58 of the original data) for Region 6
# Does this vary from Region 5, where we had our lowest levels of cuckoo activity?
test2 <- fwpr6_scores %>% filter(!(score < -3))
# filtering out those with a score less than -3, there are 5686 (.77 of the original data) for Region 6

fwpr5_scores <- read.csv('F:/Cuckoo_Acoustic_Data/2023/2023_FWPR5_Data/2023_FWPR5_Clips/2023_FWPR5_topclip_perperiod/2023_FWPR5_topclips_perSiteperPeriod.csv') # 5990

test3 <- fwpr5_scores %>% filter(!(score < 0))
# filtering out those with a score less than -3, there are 4807 (.80 of the original data) for Region 6
# Filtering out those with score less than 0, we would be going through 2907 out of 5412 clips (.53 of data)



#### Add on with finished data ####
# Create a cuckoo palette?
#(Start with just 2023 with a standardized protocol)
clips_21 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2021_AllCollab_topclips_filteredPB_4-12.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver"))
clips_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPB_4-12.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
clips_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPB_4-12.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))


# What did detections look like over the deployment time?
date_sums <- clips_23 %>% group_by(date_formatted) %>% summarize(num_detections = sum(annotation))
ggplot(date_sums, aes(x = date_formatted, y = num_detections)) +
  geom_bar(stat = "identity", fill = eyering_red1) +
  scale_y_continuous(breaks = seq(0,13,by =2)) +
  scale_x_date(date_breaks = "4 days") + # show every 5 days
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  labs(x = "Date",y = "Number of Clips with Detections",title = "Daily Detections Across All 2023 Sites")
# What did detections look like over the deployment time in 2022?
date_sums_22 <- clips_22 %>% group_by(date_formatted) %>% summarize(num_detections = sum(annotation))
ggplot(date_sums_22, aes(x = date_formatted, y = num_detections)) +
  geom_bar(stat = "identity", fill = eyering_red1) +
  scale_y_continuous(breaks = seq(0,13,by =2)) +
  scale_x_date(date_breaks = "4 days") + # show every 5 days
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  labs(x = "Date",y = "Number of Clips with Detections",title = "Daily Detections Across All 2022 Sites")
# Could this look different because the calling was much less regular? Or does it look different becuase the recorders were put out at different times? Try to figure out how to account for this


# When were the most detections - during diurnal or nocturnal periods?
timeperiod <- clips_23 %>% group_by(time_period) %>% summarize(num_detections = sum(annotation))
# timeperiod2 <- spread(timeperiod, time_period, num_detections)
# transposed_table <- as.matrix(spread(timeperiod, time_period, num_detections))
# barplot(transposed_table)
ggplot(timeperiod, aes(x = time_period, y = num_detections)) +
  geom_bar(stat = "identity", fill = eyering_red1) +
  labs(x = "Time Period of Recording", y = "Total Number of Clips with Detections", title = "All Detections Across Time Periods") +
  theme_minimal()

