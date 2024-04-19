#### Clean 2023 Vegetation Survey Data ###################

## Purpose: to read in the Survey123 data files from the veg data, clean them, and output the cleaned data into a new folder

# Created 10/24/2023

# Last modified: 1/15/2024


#### Setup #################################
packages <- c("tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
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
#veg$point_id == "YELL-032"

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

# write the cleaned data
#write.csv(veg,"./Data/Vegetation_Data/Outputs/2023_VegSurveyData_Cleaned1-15.csv", row.names = FALSE)




############ Veg Data Sheets ###########################
# Cleaning veg data
#lump across species



#### CODE GRAVEYARD ####
#veg[114,2] <- "MISO-032" # Spelled MIS0 rather than MISO