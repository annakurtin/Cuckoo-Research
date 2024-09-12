#### Check difference between two dataframes - why are there three extra data points in the new data?? ####

## read in data with 107 sites
full_dat1 <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_SCALED_8-12.csv")

full_dat2 <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_SCALED_9-4.csv")


full_dat3 <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_ClassData.csv")


# From the linear model
full_dat4 <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Linear_Model/HabChap_22-23_DaysCalls_HabCovs.csv")
test <- full_dat4 %>% filter(year == 2023)


# From habchap_detdatacovs cleaning script
full_dat5 <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_UNSCALED_9-4.csv")

sites_107 <- full_dat2$site_id
sites_110 <- full_dat5$site_id
setdiff(sites_110,sites_107)

# See what the difference is
setdiff(full_dat2$site_id, full_dat3$site_id)
