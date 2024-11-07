# Script to find out the date that the monitors were pulled, exlucing the one that we dropped
dat <- read.csv("./Data/Metadata/Outputs/2023_ARURetrieval_MetadataFull_Cleaned10-24.csv")
# format as date
dat$date_formatted <- as.Date(dat$date_retrieved, format = "%m/%d/%Y")
# find the second to last one
max(dat$date_formatted)
tail(dat$date_formatted)


full_dat <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_UNSCALED_10-25.csv")
mean(full_dat$sd_subcan_core, na.rm = TRUE)
sd(full_dat$sd_subcan_core, na.rm = TRUE)
min(full_dat$sd_subcan_core, na.rm = TRUE)
max(full_dat$sd_subcan_core, na.rm = TRUE)

