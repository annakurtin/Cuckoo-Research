## Add a BBCU column for ARcGIS

library(tidyverse)
full_dat <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_UNSCALED_9-11.csv")
# Make a detection column
full_dat$bbcu <- apply(full_dat[4:9], 1, max, na.rm = TRUE)
write.csv(full_dat,"C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_DetOccCovsFullUnscaled_wBBCUforArcGIS.csv", row.names = FALSE)