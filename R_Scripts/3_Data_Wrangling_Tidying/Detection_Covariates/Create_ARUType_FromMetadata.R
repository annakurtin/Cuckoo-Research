##### Create Datasheet with ARU Type for Detections ######

# Going through the metadata and pulling out ARU type at each point

#### Setup #####

library(tidyverse)
library(ggplot2)
source("./R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")

## Questions to answer:
# What about ARUs that were switched out to a different type mid season?
# what about sites with different ARUs at different points?
# How does other literature address this?



#### Read in Data ######
met23d <- read.csv("./Data/Metadata/Outputs/2023_ARUDeployment_MetadataFull_Cleaned10-24.csv")
met23r <- read.csv("./Data/Metadata/Outputs/2023_ARURetrieval_MetadataFull_Cleaned10-24.csv")

met22fwp <- read.csv("./Data/Metadata/Outputs/2022_ARUDeployment_Retrieval_Metadata_FWPALL_Cleaned1-22.csv")
met22umbel <- read.csv("./Data/Metadata/Outputs/2022_ARUDeployment_Retrieval_Metadata_UMBEL_Cleaned1-22.csv")



#### Extract ARU Type #####

#### 2022 ####
# Create data for UMBEL
# join met22fwp and met22umbel
# met22umbel is missing 4 rows, join this with the aru testing data - these rows are missing ARU data too 
met22umbel <- met22umbel %>% mutate(aru_mbinary = case_when(aru_model == "AM1.2.0" ~ 0,
                                                            aru_model == "AM1.1.0" ~ 0,
                                                            aru_model == "AM1.0.0" ~ 0,
                                                            is.na(aru_model) ~ 0,
                                                            aru_model == "SMM" ~ 1))
met22umbel <- met22umbel %>% create_site_col()
met22umbel <- met22umbel %>% select(site_id, point_id, aru_model, aru_mbinary)
# see how many unique types of aru models there are
umbel22 <- met22umbel %>% group_by(site_id) %>% summarize(num_arutype = n_distinct(aru_model, na.rm = TRUE))

# met23fwp is good in aru model 
met22fwp <- met22fwp %>% mutate(aru_mbinary = case_when(aru_model == "AM1.2.0" ~ 0,
                                                        aru_model == "SMM" ~ 1))
met22fwp <- met22fwp %>% create_site_col()
met22fwp <- met22fwp %>% select(site_id, point_id, aru_model, aru_mbinary)
# see how many unique types of aru models there are
fwp22 <- met22fwp %>% group_by(site_id) %>% summarize(num_arutype = n_distinct(aru_model, na.rm = TRUE))


# join umbel and fwp 22
met22all <- rbind(met22fwp, met22umbel)
# add year
met22all$year <- '2022'
# write this data
#write.csv(met22all, "./Data/Habitat_Model_Covariates/Detection_Covariates/ARUModelByPoint_22_11-4.csv", row.names = FALSE)

# how many sites had songmeters?
# summarize by site
met22all_SM <- met22all %>% group_by(site_id) %>% summarize(SM_present = max(aru_mbinary),num_arutype = n_distinct(aru_mbinary, na.rm = TRUE))
hist(met22all_SM$SM_present)
met22all_SM$year <- "2022"

# # look at the distribution of these covariates - summarize if they had just one type or more than one
# all22 <- met22all %>% group_by(site_id) %>% summarize(num_arutype = n_distinct(aru_mbinary, na.rm = TRUE))
# # how many sites had more than one ARU type?
# nrow(all22[all22$num_arutype == 2,]) # 1 site - only KIN
# hist(all22$num_arutype)





#### 2023 ####
# Turn AudioMoth into NA
# combine the columns into one
# create aru_model, use case_when to turn audiomoth1.0.0 into AM 1.0.0 and if it's NA into SongMeter micro
met23d <- met23d %>%
  mutate(across(c(audiomoth_version), ~ na_if(.x, "")))
met23d <- met23d %>% mutate(aru_model = case_when(audiomoth_version == "AudioMoth 1.2.0" ~ "AM1.2.0",
                                                    audiomoth_version == "AudioMoth 1.1.0" ~ "AM1.1.0",
                                                    audiomoth_version == "AudioMoth 1.0.0" ~ "AM1.0.0",
                                                    is.na(audiomoth_version) ~ "SMM"))
met23d <- met23d %>% mutate(aru_mbinary = case_when(aru_model == "AM1.2.0" ~ 0,
                                                    aru_model == "AM1.1.0" ~ 0,
                                                    aru_model == "AM1.0.0" ~ 0,
                                                    is.na(aru_model) ~ 0,
                                                    aru_model == "SMM" ~ 1))
met23d <- met23d %>% create_site_col()
met23d <- met23d %>% select(site_id, point_id, aru_model, aru_mbinary)
# add year
met23d$year <- "2023"
# write this to csv
#write.csv(met23d, "./Data/Habitat_Model_Covariates/Detection_Covariates/ARUModelByPoint_23_11-4.csv", row.names = FALSE)

# how many sites had songmeters?
# summarize by site
met23_SM <- met23d %>% group_by(site_id) %>% summarize(SM_present = max(aru_mbinary),num_arutype = n_distinct(aru_mbinary, na.rm = TRUE))
nrow(met23_SM[met23_SM$SM_present == 1,]) # 40 sites with songmeter
hist(met23_SM$SM_present)
nrow(met23_SM[met23_SM$num_arutype == 2,]) # 18 sites - 16%

# look at the distribution of these covariates
all23 <- met23d %>% group_by(site_id) %>% summarize(num_arutype = n_distinct(aru_mbinary, na.rm = TRUE))
# how many sites had more than one ARU type?
nrow(all23[all23$num_arutype == 2,]) # 10 sites - 16%
hist(all23$num_arutype)

# this should also have the points that were redeployed - look at which ones are repeats and whether or not the same monitor was deployed
dup <- met23d %>% filter(duplicated(point_id))
dup_point <- dup$point_id
redeploy <- met23d %>% filter(point_id %in% dup_point)
# 6 changed out, 4 changed to a different ARU
# How to create a composite???

met23_SM$year <- "2023"


#### Combine to export #####
aru_type <- rbind(met23_SM,met22all_SM)
#write.csv(aru_type, "./Data/Habitat_Model_Covariates/Detection_Covariates/ARUtype_22-23.csv", row.names = FALSE)



## Not actually needed
# met23r <- met23r %>%
#   mutate(across(c(audiomoth_version), ~ na_if(.x, "")))
# met23r <- met23r %>% mutate(aru_model = case_when(audiomoth_version == "AudioMoth 1.2.0" ~ "AM1.2.0",
#                                                   audiomoth_version == "AudioMoth 1.1.0" ~ "AM1.1.0",
#                                                   audiomoth_version == "AudioMoth 1.0.0" ~ "AM1.0.0",
#                                                   is.na(audiomoth_version) ~ "SMM"))
# met23r <- met23r %>% mutate(aru_mbinary = case_when(aru_model == "AM1.2.0" ~ 0,
#                                                     aru_model == "AM1.1.0" ~ 0,
#                                                     aru_model == "AM1.0.0" ~ 0,
#                                                     is.na(aru_model) ~ 0,
#                                                     aru_model == "SMM" ~ 1))
# met23r <- met23r %>% create_site_col()
# met23r <- met23r %>% select(site_id, point_id, aru_model, aru_mbinary)


# see if there's a difference between the deployed and retrieved 



#### Visualize the distribution of this#####
# How many sites have SMM? 
# How many sites had a combination of ARUs?