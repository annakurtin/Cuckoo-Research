#### Visualize Days with Detections ####

library(ggplot2)
library(tidyverse)
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")
source("./R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")

clips_21 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2021_AllCollab_topclips_filteredPBYBCU_5-27.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d")) %>% create_site_col()

clips_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPBYBCU_5-27.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d")) %>% create_site_col()

clips_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_5-27.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d")) %>%   mutate(month_day = format(date_formatted, "%m-%d")) %>% create_site_col()


# Create visualizations
det_23 <- clips_23 %>% group_by(site_id, month_day) %>% summarize(detection = max(annotation))
# Calculate total detections for each site and reorder site_id
site_totals <- det_23 %>%
  group_by(site_id) %>%
  summarize(total_detections = sum(detection)) %>%
  arrange(total_detections)
# Reorder the site_id factor based on the total detections
det_23 <- det_23 %>%
  mutate(site_id = factor(site_id, levels = site_totals$site_id))

# Look at individual sites
sites_wdets <- det_23 %>% filter(detection ==1)
sites_pos <- unique(sites_wdets$site_id)
sites_positive <- det_23 %>% filter(site_id %in% sites_pos)
# Reorder based on total detections
sites_positive <- sites_positive %>%
  mutate(site_id = factor(site_id, levels = site_totals$site_id))

ggplot(sites_positive, aes(x = month_day, y = detection)) +
  geom_bar(stat = "identity", fill = cuckoo_palette[1]) +
  scale_y_continuous(breaks = seq(0,13,by =2)) +
  #scale_x_date(date_breaks = "12 days") + # show every 5 days
  scale_x_discrete(breaks = unique(sites_positive$month_day)[seq(1, length(unique(sites_positive$month_day)), by = 5)]) +
  theme(axis.text.x = element_text(angle = 90,hjust = 1, size = 10), 
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 15),  # Size for "Date"
        axis.title.y = element_text(size = 15)) +
  labs(x = "Date",y = "Detection") +
  facet_wrap(~ site_id, nrow = 10)

# to remove the labels for site_id strip.text = element_blank()
ggsave("./Deliverables/Cuckoo_Detection_History/Daily_Des_2023_PosSites_Arranged.jpg", width=8, height=10)


# Old visualization
ggplot(det_23, aes(x = date_formatted, y = detection)) +
  geom_bar(stat = "identity", fill = cuckoo_palette[1]) +
  scale_y_continuous(breaks = seq(0,13,by =2)) +
  scale_x_date(date_breaks = "12 days") + # show every 5 days
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  labs(x = "Date",y = "Detection",title = "Daily Detections At 2023 Sites") +
  facet_wrap(~ site_id, nrow = 10)
