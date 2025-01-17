##### Descriptive stats ARU types at points #####

# How many AM and SMM at 2022? How many at 2023 sites?

library(tidyverse)

## Read in metadata

m23 <- read.csv("./Data/Metadata/Outputs/2023_ARUDeployment_MetadataFull_Cleaned10-24.csv")

fwp22 <- read.csv("./Data/Metadata/Outputs/2022_ARUDeployment_Retrieval_Metadata_FWPALL_Cleaned1-22.csv")

umbel22 <- read.csv("./Data/Metadata/Outputs/2022_ARUDeployment_Retrieval_Metadata_UMBEL_Cleaned1-22.csv")
m22 <- rbind(fwp22,umbel22)

# 2023: summarize ARU model
m23 %>% count(aru_model)

m22 %>% count(aru_type)
