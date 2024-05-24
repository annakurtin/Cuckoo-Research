#### Clean 2023 Vegetation Survey Data ###################

## Purpose: to read in the Survey123 data files from the veg data, clean them, and output the cleaned data into a new folder

# Created 10/24/2023

# Last modified: 5/24/2024


#### Setup #################################
packages <- c("tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_SiteColumn_fromPointID.R")
load_packages(packages)


##################### Main Data Sheet #############################
# load in data
veg <- read.csv("./Data/Vegetation_Data/Raw_Data/2023_Vegetation_Survey_Data.csv") %>% clean_names()
# remove unnecessary columns
veg <- veg %>% select(-c(object_id, point_information,can_you_complete_this_survey, trees_vs_shrubs, if_the_point_has_an_aru_the_vegetation_plot_will_be_centered_on_a_point_9_m_in_front_of_the_aru_in_the_direction_that_the_microphone_is_facing,plot_veg_data,subplot_dir, select_a_reason_or_choose_other_to_describe_the_situation_please_state_in_the_notes_whether_someone_should_return_later_to_complete_the_veg,specify_other,subplot_no_why_collated,if_you_can_do_the_subplots_enter_data_below_if_not_leave_no_selected_above_and_submit_your_form_then_return_to_collector,x11_3m_radius_subplot,description_of_the_canopy,nice_work_please_submit_your_survey,creator,edit_date,editor))
# Test with unique which values only have NA or a single value
unique(veg$can_you_complete_this_survey)

# rename the long names
veg <- veg %>% rename(aru_present = does_this_site_have_an_aru, 
               snags_present = are_there_any_snags_present_in_your_11_3m_subplot, 
               total_percent_shrub_cover = total_shrub_cover_all_species_combined,
               notes = notes_from_this_subplot,
               bearing_to_center = which_bearing_was_used_to_find_the_center_of_the_vegetation_plot)

# Point ID column: 
# Convert point ID to all upper case
veg$point_id <- toupper(veg$point_id)
# change _ to -
veg <- veg %>% mutate(point_id = str_replace(point_id, "_", "-"))
# Correct a misspelling
veg <- veg %>% mutate(point_id = str_replace(point_id, "MIS0", "MISO"))
veg <- veg %>% mutate(point_id = str_replace(point_id, "YMA", "YWM"))
# Remove the old SIP sites and replace with new ones
veg <- veg %>% filter(!point_id %in% c("SIP-1", "SIP-2"))
veg <- veg %>% mutate(point_id = str_replace(point_id, " NEW", ""))
# Edit points that need correction
veg[19,2] <- "YELL-060" # Missing the first 0
# Fix MAN-3 duplication
veg[75,2] <- "MAN-2"
# Fix the JDD-1 duplication
veg[37,2] <- "JDD-2"
# Fix the CLA-1 duplication
veg[103,2] <- "CLA-2"
# Fix the error that labeled MISO-091 as MISO-097 (found out by looking at the coordinates from the deployment data)
veg[172,2] <- "MISO-091"
#veg$point_id == "YELL-032"
# Asked Daniel about the AME-1 duplication ********PENDING**********

# Remove the points that were collected at Seacross Ranch
veg <- veg %>% filter(!point_id %in% c("SRA-1","SRA-2","SRA-3","SRB-1","SRB-2","SRB-3","LGC-1","LGC-2","LGC-3"))

# Convert datetime to datetime 
veg <- veg %>% mutate(datetime = as.POSIXct(date, format = "%m/%d/%Y %H:%M"),
                      date_formatted = as.Date(datetime))

# Observer column: not likely to use this for anything, leaving as is

# aru_present: again not likely to use this column, leaving as is

# canopy height and canopy cover columns: formatted as number, some NAs but overall looks fine

# Grazing intensity
# Which column has a blank (no data)
veg[veg$grazing_intensity == "",]
# Replace blanks with NAs
veg <- veg %>% mutate(grazing_intensity = case_when(grazing_intensity == "0" ~ "0",
                                             grazing_intensity == "L" ~ "L",
                                             grazing_intensity == "H" ~ "H",
                                             grazing_intensity == "" ~ NA))
## Could also go and look at the pictures of this plot ???????
# change grazing intensity to a factor
veg$grazing_intensity <- factor(veg$grazing_intensity, levels = c("0","L","H"))

# Grazing status
unique(veg$grazing_status)
veg <- veg %>% mutate(grazing_status = case_when(grazing_status == "Historic" ~ "Historic",
                                                 grazing_status == "Current" ~ "Current",
                                                 grazing_status == "" ~ NA))
## Could also go and look at the pictures of this plot ???????
# change grazing intensity to a factor
veg$grazing_status <- factor(veg$grazing_status, levels = c("Historic","Current"))

# Snags present column - do we need to convert this to a factor????????????????????
#veg$snags_present <- factor(veg$snags_present, levels = c("no","yes"))

# Snag size - leave this for now b/c not sure if I'm going to use it

# Total percent shrub cover: looks good
# Notes: looks good
# Creation date: looks good
# Bearing to center; looks good

# x and y: change to standard names I'm using
veg <- veg %>% rename(long= x, lat =  y)

# Create classifications for the data to use in the model
## When it was a GRTS point selected for habitat chapter, sampling design = habitat_grts
### When the point is four letters then a dash then three numbers
## When it was a GRTS point selected for long term UMBEl monitoring, sampling design = mmr_grts
### When the point is 2-3 numbers then a dash then one numbers
## When it was a non randomly selected point just used in long-term cuckoo monitoring, sampling design = selectedcu_nonrand
### When the point ID is 3 letters then a dash then one number
veg <- veg %>%
  mutate(sampling_design = case_when(
    grepl("^[[:alpha:]]{4}-[[:digit:]]{3}$", point_id) ~ "habitat_grts",
    grepl("^[[:digit:]]{2,3}-[[:digit:]]{1}$", point_id) ~ "mmr_grts",
    grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$", point_id) ~ "selectedcu_nonrand",
    TRUE ~ NA_character_  # Default case if none of the above conditions match
  ))
# create a site_id column
veg <- veg %>% create_site_col()
#test %>% select(point_id, sampling_design)
# For writing the layers in google earth engine 
# pull the habitat and mmr grts points 
hab_chap <- veg %>% filter(sampling_design %in% c("habitat_grts","mmr_grts"))
# was there an ARU at each of these points?
unique(hab_chap$aru_present)
# yes
# Write this for use in your habitat chapter(can be updated post other cleaning)
#write.csv(hab_chap,"./Data/Vegetation_Data/Outputs/2023_VegData_HabMMRGRTS_4-22.csv", row.names = FALSE)
# Create and write a datasheet of coordinates for use in google earth engine
hab_chap_red <- hab_chap %>% select(point_id, long, lat)
#write.csv(hab_chap_red,"./Data/Monitoring_Points/Outputs/2023_VegSurveyCoords_HabMMRGRTS_4-22.csv", row.names = FALSE)

# write the cleaned data
#write.csv(veg,"./Data/Vegetation_Data/Outputs/2023_VegSurvey_MainData_Cleaned5-24.csv", row.names = FALSE)

# Checking cleaning:
veg <- read.csv("./Data/Vegetation_Data/Outputs/2023_VegSurvey_MainData_Cleaned5-24.csv")
veg[duplicated(veg$point_id)==TRUE,]
#Checking continuity 
#old_veg <- read.csv("./Data/Vegetation_Data/Outputs/Archive/2023_VegSurvey_MainData_Cleaned5-20.csv")
#old_vegpts <- unique(old_veg$point_id)
#setdiff(deployed_points,old_vegpts)
deploy <- read.csv("./Data/Metadata/Outputs/2023_ARUDeployment_MetadataFull_Cleaned10-24.csv")
deployed_points <- unique(deploy$point_id)
# Figuring out duplicates
veg_pts <- unique(veg$point_id)
# Points in deployment not in veg
setdiff(deployed_points,veg_pts)
# Points in veg not in deployment
setdiff(veg_pts,deployed_points)
# Looks good



############ Tree and Shrub Data Sheets ###########################

# Cleaned veg:
veg <- read.csv("./Data/Vegetation_Data/Outputs/2023_VegSurvey_MainData_Cleaned5-24.csv")
# Read in the veg data and select global ID and point id
veg_red <- veg %>% select(global_id,point_id,site_id,sampling_design,notes) %>% rename(parent_global_id = global_id)

#### Tree #
# Read in tree data
tree <- read.csv("./Data/Vegetation_Data/Raw_Data/Trees_1.csv") %>% clean_names()
tree <- tree %>% select(-c(object_id,
                           for_each_tree_species_choose_the_most_dominant_in_the_plot_up_to_4,
                           specify_other,
                           to_add_another_tree_species,
                           creator,edit_date,editor)) %>% rename(child_global_id = global_id)
# Missing SIP-2 and SIP-1?
# Join by global_id to assign point ID to the veg and shrub data
tree_comb <- left_join(tree, veg_red, by = "parent_global_id")
unique(tree_comb$tree_species)
# Can remove parent id 
tree_comb <- tree_comb %>% filter(!parent_global_id %in% c("82943a7a-df88-4201-be28-1ea293c143ab", # LGC-1
                                                       "67d61f98-50c2-47e4-9301-a9c1ece108a0", # LGC-2
                                                       "056a959c-40b6-4297-a94b-9c228516aa4f", # LGC-3
                                                       "3286a2ef-c6ed-4e12-ae22-5b209a44ba62", # SRA-3
                                                       "b0aef62a-0023-4403-8140-7d61d188ba39", # SRA-2
                                                       "6fdf473b-48aa-4ee4-8d62-b03256bdf740", # SRA-1
                                                       "3ba9681c-ea03-4908-a6d5-97616c6a4a58", # SRB-1
                                                       "d732c359-610d-44cb-b916-2b2faacbe42a", # SRB-2
                                                       "01a02c05-95ae-4c60-8c39-4ad30a916aa0", #SRB-3
                                                       "819e0291-9369-4c34-b24f-eabd22fc1a7b", # Old SIP-1
                                                       "eae7cc54-5b91-4574-b4d7-c3608a73fc13" # Old SIP-2
))
# Check if there are any in veg that aren't in tree
setdiff(veg$point_id,tree_comb$point_id)
  
#### Shrub #
shrub <- read.csv("./Data/Vegetation_Data/Raw_Data/ShrubCover_2.csv") %>% clean_names()
shrub <- shrub %>% select(-c(object_id, 
                             for_each_shrub_species_choose_the_most_dominant_in_the_plot_up_to_8,
                             specify_other, 
                             to_add_another_shrub_species, 
                             creator, 
                             edit_date,
                             editor)) %>% rename(child_global_id = global_id)

# Join by global_id to assign point ID to the veg and shrub data
shrub_comb <- left_join(shrub, veg_red, by = "parent_global_id")
# First: replace the blank spaces with NA so they don't get removed in later calculations
shrub_comb <- shrub_comb %>%
  mutate(across(c(shrub_species,shrub_sp_other_collated),~na_if(., "")))
# Can remove parent id 
shrub_comb <- shrub_comb %>% filter(!parent_global_id %in% c("82943a7a-df88-4201-be28-1ea293c143ab", # LGC-1
                                                       "67d61f98-50c2-47e4-9301-a9c1ece108a0", # LGC-2
                                                       "056a959c-40b6-4297-a94b-9c228516aa4f", # LGC-3
                                                       "3286a2ef-c6ed-4e12-ae22-5b209a44ba62", # SRA-3
                                                       "b0aef62a-0023-4403-8140-7d61d188ba39", # SRA-2
                                                       "6fdf473b-48aa-4ee4-8d62-b03256bdf740", # SRA-1
                                                       "3ba9681c-ea03-4908-a6d5-97616c6a4a58", # SRB-1
                                                       "d732c359-610d-44cb-b916-2b2faacbe42a", # SRB-2
                                                       "01a02c05-95ae-4c60-8c39-4ad30a916aa0", #SRB-3
                                                       "819e0291-9369-4c34-b24f-eabd22fc1a7b", # Old SIP-1
                                                       "eae7cc54-5b91-4574-b4d7-c3608a73fc13" # Old SIP-2
                                                       ))
#unique(shrub_comb$point_id)
setdiff(veg$point_id,shrub_comb$point_id)
# Missing JDO-2 but this doesn't have any shrubs??? Just move on with it for now
#veg %>% filter(total_percent_shrub_cover == 0)
#veg[veg$point_id == "JDO-2",]

# Clean up the shrub "other" and UNSH based on comments and sp other column
# Change based on notes for AME-1
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(point_id == "AME-1" & x_cover == 1.0,"TAMA",shrub_species))
## Based on comments: GMW-3 change UNSH at .5 m high to RIBE and NA at 1m high to TAMA
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(point_id == "GMW-3" & shrub_height_m == 0.50,"RIBE",shrub_species))
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(point_id == "GMW-3" & shrub_height_m == 1.00,"TAMA",shrub_species))
## Change UNSH LMA-2 to JUHO based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "6bf44fb3-2c46-41ac-861b-75e35fc5bde7","JUHO",shrub_species))
## SNO-2 - change UNSH to LONI for honeysuckle spp
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(point_id == "SNO-2" & shrub_height_m == 0.50 & x_cover == 1.00,"LONI",shrub_species))
## SNO-3 - change UNSH to LONI for honeysuckle
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(point_id == "SNO-3" & shrub_height_m == 0.75 & x_cover == 1.00,"LONI",shrub_species))
## Change MISO-204 to SHAR for silver buffaloberry
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(point_id == "MISO-204" & shrub_height_m == 2.25,"SHAR",shrub_species))
## MISO-150 - change UNSH to UNBR - look at the picture from the veg survey, this was counted as narrowleaf marshelder but their range isn't in MT
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(point_id == "MISO-150" & shrub_height_m == 1.00,"UNBR",shrub_species))
## UNSH 5% at YELL-211 is TAMA and other is wormwood
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(point_id == "YELL-211" & x_cover == 5.00,"TAMA",shrub_species))
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(point_id == "YELL-211" & x_cover == 2.00,"ARAB",shrub_species))
## Change MISO-152 based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "39e20095-1994-4228-8908-4a802bc409b1","ARAB",shrub_species))
## Changing MISO-119 based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(point_id == "MISO-119" & x_cover == 3.00,"YUGA",shrub_species))
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "5d000c00-bb28-4807-8334-e6f5a5cac224","GUSA",shrub_species))
## Changing MISO-187
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "acf4206c-9b06-41ab-8759-57a380d480be","ARLU",shrub_species))
## Changing MISO-125 based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "7d216a8f-b9d3-446a-8f7e-92cc54231f59","ATCO",shrub_species))
## Changing MISO-057 based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "5788e4b0-452b-4c93-b37d-fc0ad0061dec","YUGA",shrub_species))
## Changing MISO-016 based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "da0f688c-1c06-4586-a26c-0d04fd8dd8ba","ATCO",shrub_species))
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "a467807f-e93e-427b-b498-7008307f4ec7","GUSA",shrub_species))
## Changing MISO-015 based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "7770bf32-d881-48bc-b22b-a883876d8d85","ATCO",shrub_species))
## Changing MISO-122 based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "77dc8087-7374-49ae-ba25-d125ae7a6c09","ATCO",shrub_species))
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "7acd2c43-c313-452f-b82a-09efe03bf67e","KRLA",shrub_species))
## Changing MISO-098 based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "1a22ea3d-c008-4121-8152-af054d338d22","YUGA",shrub_species))
## Changing MISO-094 based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "376e6452-1913-45fe-9c5c-b57cff977a83","KRLA",shrub_species))
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "c0583f1f-65e2-439a-b6a1-b696cfe59ee0","ATCO",shrub_species))
## Changing MISO-188 based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "25f37262-202d-4999-adb2-0486c15cf5ec","ARLU",shrub_species))
## Changing MISO-196 based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "790f1d75-636d-4e63-bd3a-02914dd0c47c","ARLU",shrub_species))
## CHanging MISO-017 based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "291550b1-d873-4dd2-9b47-37006a869fcc","YUGA",shrub_species))
## Changing MISO-177 based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "4328663f-af2b-45cc-b600-cd47307d6c15","ARLU",shrub_species))
## Changing MISO-064 based on comments
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "6f24e66c-7389-440c-a9ef-b111ef30d256","GUSA",shrub_species))
# Add tamarisk from the other YELL-024
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "bcdb0934-64cf-460c-9578-165b5c65a164","TAMA",shrub_species))
## Add tamarisk from other YELL-217
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "fc54436e-14e2-4e29-9a4c-d4a75458bb61","TAMA",shrub_species))
# Change MISO-062 to UNK Broadleaf
shrub_comb <- shrub_comb %>% mutate(shrub_species = ifelse(child_global_id == "b4aaf2f8-1d49-446b-8c96-29b0ac4ab4dd","UNBR",shrub_species))

## Remove bc herbaceous, not shrub
## Ground cherry ## Sweet clover ## Licorice
## NOTE: not including Parthenocissus inserta (was listed in the comments) since this is a vine and not woody veg/shrub
shrub_comb <- shrub_comb %>% filter(!child_global_id %in% c("13ec2366-696f-4aa7-808e-af5968a63100","59d77312-4429-4e77-a7a2-326af085f0cf","8b05d257-e3fa-43da-b4ca-95d67eed2e01" ))

# Take a look at what is represented by the UNK species
unk_only <- shrub_comb %>% filter(shrub_species == "UNSH") 
# No UNSH left

#unique(shrub_comb$shrub_species)



# Mutate any rows that have values for cover and height but no spp to UNSH
# After doing this, look and see what the total coverage/distribution of the UNSH is and potentially label them as misc_broadleaf or remove them

# Write this to .csv
#write.csv(tree_comb,"./Data/Vegetation_Data/Outputs/2023_VegSurvey_TreeData_Cleaned5-24.csv", row.names = FALSE)
#write.csv(shrub_comb,"./Data/Vegetation_Data/Outputs/2023_VegSurvey_ShrubData_Cleaned5-24.csv", row.names = FALSE)




#### CODE GRAVEYARD ####
#veg[114,2] <- "MISO-032" # Spelled MIS0 rather than MISO
# unk_shrub_plot <- unk_only %>% ggplot() +
#   geom_histogram(x= x_cover)
# unk_shrub_cover <- hist(unk_only$x_cover)