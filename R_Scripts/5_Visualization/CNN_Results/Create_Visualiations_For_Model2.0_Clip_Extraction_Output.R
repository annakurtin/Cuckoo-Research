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



#### Add on with finished data ####
#(Start with just 2023 with a standardized protocol)
clips_21 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2021_AllCollab_topclips_filteredPB_4-12.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
#clips_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPB_4-12.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
clips_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPBYBCU_4-24.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
#clips_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPB_4-12.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
clips_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_4-24.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
clips_23 <- clips_23 %>%
  mutate(site_id = case_when(
    # If point_id has four letters then a dash then three numbers
    grepl("^[[:alpha:]]{4}-[[:digit:]]{3}$", point_id) ~ point_id,
    # If point_id has two numbers, three numbers, or three letters then a dash then one number
    grepl("^[[:digit:]]{2,3}-[[:digit:]]{1}$", point_id) |
      grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$", point_id) ~ gsub("-.*", "", point_id)
  ))

# Trying to read in formatted data 
full_23 <- readRDS("./Data/Detection_History/2023_All_ARUs/Outputs/2023_BBCUDetections.Rdata")

# What did detections look like over the deployment time?
date_sums <- clips_23 %>% group_by(date_formatted) %>% summarize(num_detections = sum(annotation))
date_recstart_23 <- clips_23 %>% group_by(point_id) %>% summarize(first_date = min(date_formatted))
ggplot(date_sums, aes(x = date_formatted, y = num_detections)) +
  geom_bar(stat = "identity", fill = eyering_red1) +
  geom_vline(xintercept = date_recstart_23$first_date)+
  scale_y_continuous(breaks = seq(0,13,by =2)) +
  scale_x_date(date_breaks = "4 days") + # show every 5 days
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  labs(x = "Date",y = "Number of Clips with Detections",title = "Daily Detections Across All 2023 Sites")
# What did detections look like over the deployment time in 2022?
date_sums_22 <- clips_22 %>% group_by(date_formatted) %>% summarize(num_detections = sum(annotation))
date_recstart_22 <- clips_22 %>% group_by(point_id) %>% summarize(first_date = min(date_formatted))
ggplot(date_sums_22, aes(x = date_formatted, y = num_detections)) +
  geom_bar(stat = "identity", fill = eyering_red1) +
  geom_vline(xintercept = date_recstart_22$first_date)+
  scale_y_continuous(breaks = seq(0,13,by =2)) +
  scale_x_date(date_breaks = "4 days") + # show every 5 days
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  labs(x = "Date",y = "Number of Clips with Detections",title = "Daily Detections Across All 2022 Sites")
# Could this look different because the calling was much less regular? Or does it look different becuase the recorders were put out at different times? Try to figure out how to account for this
# What did detections look like over the deployment time in 2022?
date_sums_21 <- clips_21 %>% group_by(date_formatted) %>% summarize(num_detections = sum(annotation))
date_recstart_21 <- clips_21 %>% group_by(point_id) %>% summarize(first_date = min(date_formatted))
ggplot(date_sums_21, aes(x = date_formatted, y = num_detections)) +
  geom_bar(stat = "identity", fill = eyering_red1) +
  geom_vline(xintercept = date_recstart_21$first_date)+
  scale_y_continuous(breaks = seq(0,13,by =2)) +
  scale_x_date(date_breaks = "4 days") + # show every 5 days
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  labs(x = "Date",y = "Number of Clips with Detections",title = "Daily Detections Across All 2021 Sites")


# When were the most detections - during diurnal or nocturnal periods?
timeperiod_23 <- clips_23 %>% group_by(time_period) %>% summarize(num_detections = sum(annotation))
ggplot(timeperiod_23, aes(x = time_period, y = num_detections)) +
  geom_bar(stat = "identity", fill = eyering_red1) +
  labs(x = "Time Period of Recording", y = "Total Number of Clips with Detections", title = "All Detections Across Time Periods 2023") +
  theme_minimal()

timeperiod_22 <- clips_22 %>% group_by(time_period) %>% summarize(num_detections = sum(annotation))
ggplot(timeperiod_22, aes(x = time_period, y = num_detections)) +
  geom_bar(stat = "identity", fill = eyering_red1) +
  labs(x = "Time Period of Recording", y = "Total Number of Clips with Detections", title = "All Detections Across Time Periods 2022") +
  theme_minimal()



# Look at all of the sites
jpeg(paste("./Deliverables/Cuckoo_Detection_History/BBCU_SiteDetHistory_2023.jpg"), width = 1500, height = 800)
point_det_23 <- clips_23 %>% group_by(point_id, date_formatted) %>% summarize(detection = max(annotation))
#date_recstart_23 <- clips_23 %>% group_by(point_id) %>% summarize(first_date = min(date_formatted))
ggplot(point_det_23, aes(x = date_formatted, y = detection)) +
  geom_bar(stat = "identity", fill = eyering_red1) +
  # geom_vline(xintercept = date_recstart_23$first_date)+
  scale_y_continuous(breaks = seq(0,13,by =2)) +
  scale_x_date(date_breaks = "12 days") + # show every 5 days
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  labs(x = "Date",y = "Detection",title = "Daily Detections At 2023 Sites") +
  facet_wrap(~ point_id, nrow = 10)
dev.off()
# 2022
jpeg(paste("./Deliverables/Cuckoo_Detection_History/BBCU_SiteDetHistory_2022.jpg"), width = 1500, height = 800)
point_det_22 <- clips_22 %>% group_by(point_id, date_formatted) %>% summarize(detection = max(annotation))
date_point_recstart_22 <- clips_22 %>% group_by(point_id) %>% summarize(first_date = min(date_formatted))
ggplot(point_det_22, aes(x = date_formatted, y = detection)) +
  geom_bar(stat = "identity", fill = eyering_red1) +
  #geom_vline(xintercept = date_recstart_22$first_date)+
  scale_y_continuous(breaks = seq(0,13,by =2)) +
  scale_x_date(date_breaks = "12 days") + # show every 5 days
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  labs(x = "Date",y = "Detection",title = "Daily Detections At 2022 Sites") +
  facet_wrap(~ point_id, nrow = 10)
dev.off()


# Create a graphic of detections for the YBCU data in 2023
ybcu_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_YBCU_Clips_4-24.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
ybcu_det_23 <- ybcu_23 %>% group_by(point_id, date_formatted) %>% summarize(detection = max(ybcu))
jpeg(paste("./Deliverables/Cuckoo_Detection_History/YBCU_SiteDetHistory_2023.jpg"), width = 1500, height = 800)
ggplot(ybcu_det_23, aes(x = date_formatted, y = detection)) +
  geom_bar(stat = "identity", fill = 'darkgoldenrod2') +
  scale_y_continuous(breaks = seq(0,13,by =2)) +
  scale_x_date(date_breaks = "2 days") + # show every 5 days
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  labs(x = "Date",y = "Detection",title = "Daily Detections At 2023 Sites") +
  facet_wrap(~ point_id, nrow = 10)
dev.off()

# Create a graphic of detections for the YBCU data in 2022
ybcu_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_YBCU_Clips_4-24.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
ybcu_det_22 <- ybcu_22 %>% group_by(point_id, date_formatted) %>% summarize(detection = max(ybcu))
jpeg(paste("./Deliverables/Cuckoo_Detection_History/YBCU_SiteDetHistory_2022.jpg"), width = 1500, height = 800)
ggplot(ybcu_det_22, aes(x = date_formatted, y = detection)) +
  geom_bar(stat = "identity", fill = 'darkgoldenrod2') +
  scale_y_continuous(breaks = seq(0,13,by =2)) +
  scale_x_date(date_breaks = "2 days") + # show every 5 days
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  labs(x = "Date",y = "Detection",title = "Daily Detections At 2023 Sites") +
  facet_wrap(~ point_id, nrow = 10)
dev.off()



# Look at individual sites
detections_graphic <- clips_23%>% group_by(site_id, date_formatted) %>% summarize(detection = max(annotation))
# pull out sites with detections
points_with_dets <- detections_graphic %>% filter(detection ==1)
sites_pos <- unique(points_with_dets$site_id)
daily_det_forgraphic <- detections_graphic %>% filter(site_id %in% sites_pos)

#date_recstart_23 <- clips_23 %>% group_by(point_id) %>% summarize(first_date = min(date_formatted))
ggplot(daily_det_forgraphic, aes(x = date_formatted, y = detection)) +
  geom_bar(stat = "identity", fill = eyering_red1) +
  # geom_vline(xintercept = date_recstart_23$first_date)+
  scale_y_continuous(breaks = seq(0,13,by =2)) +
  scale_x_date(date_breaks = "12 days") + # show every 5 days
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  labs(x = "Date",y = "Detection",title = "Daily Detections At 2023 Sites") +
  facet_wrap(~ site_id, nrow = 10)


# Visualizing the formatted data
## What is the distribution of calls throughout the breeding season?
test <- full_23 %>% group_by(date_formatted) %>% summarize(bbcu_sum = sum(annotation))
ggplot(test) + geom_bar(aes(x = date_formatted, y = bbcu_sum), stat = "identity")
# We would want this to be a negative quadratic

#### Archive Code: Preliminary Data #####
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


##### Code Graveyard ####
# timeperiod2 <- spread(timeperiod, time_period, num_detections)
# transposed_table <- as.matrix(spread(timeperiod, time_period, num_detections))
# barplot(transposed_table)

# Making the date a 45 degree angle theme(axis.text.x = element_text(angle = 45,hjust = 1)) +