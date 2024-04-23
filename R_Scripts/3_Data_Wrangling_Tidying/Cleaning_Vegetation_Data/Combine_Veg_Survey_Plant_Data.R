#### Combine Veg Survey and Plant Spp ###################

## Purpose: to read in the Survey123 data files from the veg data and combine them with the plant data 

# Created 4/23/2023

# Last modified: 4/23/2024


#### Setup #################################
packages <- c("tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)


##################### Main Data Sheet #############################
# read in current veg data
veg_parent <- read.csv("./Data/Vegetation_Data/Outputs/2023_VegSurveyData_Cleaned4-23.csv")
veg <- veg %>% rename(parent_global_id = global_id)

# Read in the plant data for cleaning
tree <- read.csv("./Data/Vegetation_Data/Raw_Data/Trees_1.csv") %>% clean_names()
tree <- tree %>% select(-c(creator,editor,to_add_another_tree_species,specify_other,object_id,for_each_tree_species_choose_the_most_dominant_in_the_plot_up_to_4))
# Rename globalid to child_globalid
tree <- tree %>% rename(child_global_id = global_id)

# Read in the shrub data
shrub <- read.csv("./Data/Vegetation_Data/Raw_Data/ShrubCover_2.csv") %>% clean_names()
shrub <- shrub %>% select(-c(object_id,for_each_shrub_species_choose_the_most_dominant_in_the_plot_up_to_8,to_add_another_shrub_species, creator, editor))
unique(shrub$specify_other)
shrub <- shrub %>% rename(child_global_id = global_id,
                          percent_cover = x_cover)
# Trying to plot this - come back to this later
hist(shrub$x_cover, breaks = 10)
shrub %>% ggplot()+
  geom_histogram(x=x_cover)


# try joining to parent data
veg_shrub <- left_join(veg,shrub,by = "parent_global_id")
# figure out later how to do this


# export this as a detection layer for the habitat chapter
detection_shrubs <- veg_shrub %>% filter(shrub_height_m >= 1)
# group by point id and summarize the percent of tall shrub cover (over 1 m high)
detection_shrubs <- detection_shrubs %>% group_by(point_id, sampling_design) %>% summarize(percent_tallshrubcov_summed = sum(percent_cover))
# filter out only the habitat points you need and only the columns you need
detection_shrubs <- detection_shrubs %>% filter(sampling_design %in% c("habitat_grts","mmr_grts")) %>% select(point_id,sampling_design, percent_tallshrubcov_summed)
# write this for use in the detection model 
#write.csv(detection_shrubs, "./Data/Habitat_Model_Covariates/Detection_Covariates/TallShrub_SummedPercentCover.csv",row.names = FALSE)
