## Histograms of Calls Over Dates ####

library(tidyverse)
library(ggplot2)
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 2023 Data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# From Plot_BBCU_calls_per_date_22-23.R
cnn_23 <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_5-27.csv")
cnn_23$date_formatted <- as.Date(cnn_23$date_formatted)

# Establish start and end periods for periods of interest (for habchap intensity of use model)
start_period <- as.Date("2023-07-01")
end_period <- as.Date("2023-07-15")


all23 <- cnn_23 %>% group_by(site_id, date_formatted) %>% summarize(bbcu = sum(annotation))
# Looking at distribution of days with calling
all23 <- all23 %>% group_by(date_formatted) %>% summarize(num_calls = sum(bbcu))
start_23 <- min(all23$date_formatted)
end_23 <- max(all23$date_formatted)
all23 %>% ggplot() + 
  geom_bar(aes(x = date_formatted, y = num_calls), stat = "identity", fill = cuckoo_palette[1])+
  scale_x_date(limits = c(start_23, end_23), date_breaks = "4 days", date_labels = "%b %d") +
  scale_y_continuous(limits = c(0, 13), breaks = seq(0,12, by = 2)) +
  labs(x = "Date", y = "Number of Calls", title = "Daily Call Rates 2023", subtitle = "Max 4 Calls Per Day Per Site") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60,hjust = 1))
# Save plot
#ggsave("./Deliverables/MetChap_ModelVisualizations/Hist_2023_CallsOverTime.jpg", width=10, height=6)


# How can I also represent the number of the total that were on?


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 2022 Data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cnn_22 <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPBYBCU_5-27.csv")
cnn_22$date_formatted <- as.Date(cnn_22$date_formatted)

# Establish periods of interest (for habchap intensity of use model)
start_period <- as.Date("2022-07-01")
end_period <- as.Date("2022-07-15")

all22 <- cnn_22 %>% group_by(site_id, date_formatted) %>% summarize(bbcu = sum(annotation))
all22 <- all22 %>% group_by(date_formatted) %>% summarize(num_calls = sum(bbcu))
start_22 <- as.Date("2022-06-01")
end_22 <- as.Date("2022-08-15")
all22 %>% ggplot() + 
  geom_bar(aes(x = date_formatted, y = num_calls), stat = "identity", fill = cuckoo_palette[1]) +
  scale_x_date(limits = c(start_22, end_22), date_breaks = "4 days", date_labels = "%b %d") +
  scale_y_continuous(limits = c(0, 13), breaks = seq(0,12, by = 2)) +
  labs(x = "Date", y = "Number of Calls", title = "Daily Call Rates 2022", subtitle = "Max 4 Calls Per Day Per Site") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60,hjust = 1))
