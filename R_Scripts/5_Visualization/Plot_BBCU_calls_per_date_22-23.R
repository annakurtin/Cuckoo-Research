### Plot which dates had how many BBCU ####

library(tidyverse)
library(ggplot2)
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")



# Extract number of days with calls from cleaned annotation data 2023 ####
cnn_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_5-27.csv")
cnn_23$date_formatted <- as.Date(cnn_23$date_formatted)
# Establish start and end periods for periods of interest
start_period <- as.Date("2023-07-01")
end_period <- as.Date("2023-07-15")

# Get the number of calls per site 1 ####
# det23 <- cnn_23 %>% group_by(site_id,date_formatted) %>% summarize(bbcu = max(annotation))
# all23 <- cnn_23 %>% group_by(site_id, date_formatted) %>% summarize(bbcu = sum(annotation))
# # Looking at distribution of days with calling
# det23 <- det23 %>% group_by(date_formatted) %>% summarize(num_calls = sum(bbcu))
# det23 %>% ggplot() + 
#   geom_bar(aes(x = date_formatted, y = num_calls), stat = "identity", fill = cuckoo_palette[1])+
#   scale_x_date(date_breaks = "5 days", date_labels = "%b %d") +
#   labs(x = "Date", y = "Number of Calls", title = "Daily Call Rates 2023", subtitle = "Max 1 Call Per Day Per Site") +
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 60,hjust = 1))

# Get number of calls per site 2 ####
# Try looking not just at 1/0 for whether there was a call there that day but instead look at all those labeled as cuckoo calls
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
  theme(axis.text.x = element_text(angle = 60,hjust = 1))+
  geom_vline(xintercept = start_period) +
  geom_vline(xintercept = end_period)


#### Extract number of days with calls from cleaned annotation data 2022  ####
cnn_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPBYBCU_5-27.csv")
cnn_22$date_formatted <- as.Date(cnn_22$date_formatted)
# Establish periods of interest
start_period <- as.Date("2022-07-01")
end_period <- as.Date("2022-07-15")

# Plot number of calls 1 ####
# det22 <- cnn_22 %>% group_by(site_id,date_formatted) %>% summarize(bbcu = max(annotation))
# det22 <- det22 %>% group_by(date_formatted) %>% summarize(num_calls = sum(bbcu))
# det22 %>% ggplot() + 
#   geom_bar(aes(x = date_formatted, y = num_calls), stat = "identity", fill = cuckoo_palette[1]) +
#   scale_x_date(date_breaks = "5 days", date_labels = "%b %d") +
#   labs(x = "Date", y = "Number of Calls", title = "Daily Call Rates 2022", subtitle = "Max 1 Call Per Day Per Site") +
#   theme_minimal()+
#   theme(axis.text.x = element_text(angle = 60,hjust = 1))

# Plot number of calls 2 ####
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
  theme(axis.text.x = element_text(angle = 60,hjust = 1)) +
  geom_vline(xintercept = start_period) +
  geom_vline(xintercept = end_period)




#look23$date <- as.character(look23$date)
#look23$date_f <- as.Date(look23$date, format = "%Y%m%d")
#look22$date <- as.character(look22$date)
#look22$date_f <- as.Date(look22$date_formatted, format = "%Y%m%d")