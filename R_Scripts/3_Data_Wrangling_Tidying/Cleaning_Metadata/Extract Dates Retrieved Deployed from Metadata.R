#### deployment dates#######

# Purpose: to extract the first day deployed and the first day retrieved from the metadata pre 2023

# Created: UNK (a while ago)

# Last modified: 5/28/2023

#################### Setup ##########################

# Libraries
library(tidyverse)
library(here)
library(janitor)



#### Updated Code ################################################
umbel_22 <- read.csv("./Data/Metadata/Outputs/2022_ARUDeployment_Retrieval_Metadata_UMBEL_Cleaned1-22.csv")
fwp_22 <- read.csv("./Data/Metadata/Outputs/2022_ARUDeployment_Retrieval_Metadata_FWPALL_Cleaned1-22.csv")
umbel_21 <- read.csv("./Data/Metadata/Outputs/2021_ARUDeployment_Retrieval_Metadata_UMBEL_Cleaned2-21.csv")

all_22 <- rbind(umbel_22,fwp_22)
all_22 <- all_22 %>% mutate(date_deployed = as.Date(date_deployed, format = "%Y-%m-%d"),
                            date_retrieved = as.Date(date_retrieved, format = "%Y-%m-%d"))
# Find the range of dates of deployment
min(all_22$date_deployed) # 2022-06-19
max(all_22$date_deployed) # 2022-07-14
# Find the range of the dates of retrieval
min(all_22$date_retrieved) # 2022-06-28
max(all_22$date_retrieved) # 2022-09-23
# Find the duration they were out
all_22 <- all_22 %>% mutate(time_dep = date_retrieved - date_deployed)
min(all_22$time_dep) # 6 days
max(all_22$time_dep) # 88 days
mean(all_22$time_dep)# 67 days 


# Look at 2021 data
all_21 <- umbel_21 %>% mutate(date_deployed = as.Date(date_deployed, format = "%m/%d/%Y"),
                              date_retrieved = as.Date(date_retrieved, format = "%m/%d/%Y"))
# Find the range of dates of deployment
min(all_21$date_deployed, na.rm = TRUE) # 2021-05-25
max(all_21$date_deployed, na.rm = TRUE) # 2022-06-25
# Find the range of the dates of retrieval
min(all_21$date_retrieved, na.rm = TRUE) # 2021-08-24
max(all_21$date_retrieved, na.rm = TRUE) # 2021-09-02
# Find the duration they were out
all_21 <- all_21 %>% mutate(time_dep = date_retrieved - date_deployed)
min(all_21$time_dep, na.rm = TRUE) # 60 days
max(all_21$time_dep, na.rm = TRUE) # 88 days
mean(all_21$time_dep, na.rm = TRUE)# 79 days 


# Look at 2023 data
all_23_dep <- read.csv("./Data/Metadata/Outputs/2023_ARUDeployment_MetadataFull_Cleaned10-24.csv")
all_23_dep <- all_23_dep %>% mutate(date_deployed = as.Date(date_deployed, format = "%m/%d/%Y"))
# Filter out the ones that were deployed again 
all_23_dep <- all_23_dep %>% slice(-(155:160))
dep_only <- all_23_dep %>% select(point_id, date_deployed)
# Retrieval
all_23_ret <- read.csv("./Data/Metadata/Outputs/2023_ARURetrieval_MetadataFull_Cleaned10-24.csv")
all_23_ret <- all_23_ret %>% mutate(date_retrieved = as.Date(date_retrieved, format = "%m/%d/%Y"))
ret_only <- all_23_ret %>% select(point_id, date_retrieved)
all_23 <- left_join(dep_only, ret_only, by = "point_id")
# any NA?
all_23 %>% filter(is.na(date_deployed)|is.na(date_retrieved))
# Find the range of dates of deployment
min(all_23$date_deployed, na.rm = TRUE) # 2021-05-05
max(all_23$date_deployed, na.rm = TRUE) # 2022-06-02
# Find the range of the dates of retrieval
min(all_23$date_retrieved, na.rm = TRUE) # 2021-08-15
max(all_23$date_retrieved, na.rm = TRUE) # 2021-10-11
# Find the duration they were out
all_23 <- all_23 %>% mutate(time_dep = date_retrieved - date_deployed)
min(all_23$time_dep, na.rm = TRUE) # 77 days
max(all_23$time_dep, na.rm = TRUE) # 134 days
mean(all_23$time_dep, na.rm = TRUE)# 91 days 





#### Archived Code #########################################3
UMBEL_2022 <- read_csv("./Data/Metadata/2022_ARUDeployment_Metadata_UMBEL.csv")
Skone_2022 <- read_csv("./Data/Metadata/2022_ARUDeployment_Metadata_FWPR7.csv")
Hussey_2022 <- read_csv("./Data/Metadata/2022_ARUDeployment_Metadata_FWPR6.csv")
UMBEL_2021 <- read_csv("./Data/Metadata/2021_ARUDeployment_Metadata_UMBEL.csv")
UMBELFWP_2020 <- read_csv("./Data/2020_ARUDeploymentMetadata_ARUandPlaybackResults_UMBEL_FWP.csv")

UMBEL_2022 <- UMBEL_2022 %>% clean_names() 
UMBEL_2022 <- UMBEL_2022 %>% rename(point_id=point_ID)
UMBEL_2022 <- UMBEL_2022 %>% select(point_id,date_deployed,date_retrieved) 


Skone_2022 <- Skone_2022 %>% clean_names() %>% select(point_id,date_deployed,date_retrieved)

Hussey_2022 <- Hussey_2022 %>% clean_names() %>% select(point_id,date_deployed,date_retrieved)

UMBEL_2021 <- UMBEL_2021 %>% clean_names() %>% select(point_id,date_deployed,date_retrieved)
# need to reformat dates

UMBELFWP_2020 <- UMBELFWP_2020 %>% rename(date_retrieved=date_retrived)
UMBELFWP_2020 <- UMBELFWP_2020 %>% clean_names() %>% select(point_id,date_deployed,date_retrieved)


# Put all point IDs into one dataframe
all_points <- rbind(UMBEL_2022,Skone_2022,Hussey_2022,UMBELFWP_2020)
min(all_points$date_deployed)
max(all_points$date_deployed)
