#### Filter Out YBCU Audio From CNN2.0 Clips ###################

## Purpose: to read in the clips sheets and filter out calls that are YBCU

# Created 4/12/2023
# Last modified 4/18/2023

#### Setup #################################
packages <- c("data.table","tidyverse","janitor","chron")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)

### Read in data #####
clips_21 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2021_AllCollab_topclips_filteredPB_4-12.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver"))
clips_22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2022_AllCollab_topclips_filteredPB_4-12.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver"))
clips_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPB_4-12.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver"))
ybcu_clips <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/Clip_Annotation_YBCU_Files_4-12.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver"))
# 2022 FAR-2, 2022 FAR-3, 2022 HOL-1, 2022 ELI-1, and 2023 203-2 don't actually have any YBCU, these were changed to UNK

# Make a unique identifier
clips_21 <- unite(clips_21,clip_unique,c(point_id,datetime),sep=" ",remove=FALSE)
clips_22 <- unite(clips_22,clip_unique,c(point_id,datetime),sep=" ",remove=FALSE)
clips_23 <- unite(clips_23,clip_unique,c(point_id,datetime),sep=" ",remove=FALSE)
ybcu_clips <- unite(ybcu_clips,clip_unique,c(point_id,datetime),sep=" ",remove=FALSE)

clips_21 <- clips_21 %>% mutate(species = case_when(
  annotation == 1 & clip_unique %in% ybcu_clips$clip_unique ~ "UNK",
  annotation == 1 & !(clip_unique %in% ybcu_clips$clip_unique) ~ "BBCU",
  annotation == 0 ~ NA
))
clips_22 <- clips_22 %>% mutate(species = case_when(
  annotation == 1 & clip_unique %in% ybcu_clips$clip_unique ~ "UNK",
  annotation == 1 & !(clip_unique %in% ybcu_clips$clip_unique) ~ "BBCU",
  annotation == 0 ~ NA
))
clips_23 <- clips_23 %>% mutate(species = case_when(
  annotation == 1 & clip_unique %in% ybcu_clips$clip_unique ~ "UNK",
  annotation == 1 & !(clip_unique %in% ybcu_clips$clip_unique) ~ "BBCU",
  annotation == 0 ~ NA
))
#test <- clips_23 %>% filter(species == "BBCU")
# Determine which sites you need to check
sites_years_tocheck <- ybcu_clips %>% unite(point_year,c(point_id, year), sep = "_") 
sites_years_tocheck <-unique(sites_years_tocheck$point_year)
# Determine which clips have potential YBCU in them 
# method for doing this?
# Assign a new column for spaced_coo that is 1 if the file name is in the potential YBCU files from the google drive
# OR mutate call_type to spaced_coo if the file name is in the potential YBCU files from the google drive 

# visualize these for each site with date on the x axis, number (sum of annotation?) on the y axis and color corresponding to type of call 


# Make it into a function
create_detection_plot <- function(current_point,current_year, year_clips){
  clips_tocheck <- year_clips %>% filter(point_id == current_point)
  # Plot this
  ggplot() +
    geom_bar(data = clips_tocheck, mapping = aes(x = datetime, y = annotation, fill= species), position = "stack", stat = "identity") + 
    labs(x = "Date", y = "Annotation", title = paste("Species Detection History for ",current_point, current_year)) +  # Labels for axes
    scale_x_datetime(date_breaks = "4 days") + # show every 5 days
    scale_fill_manual(values = c("BBCU" = "darkblue","UNK" = "darkgoldenrod2"))+
    theme(axis.text.x = element_text(angle = 45,hjust = 1)) 
}

call_type_plot <- function(current_point,current_year, year_clips){
  clips_tocheck <- year_clips %>% filter(point_id == current_point)
  # Plot this
  ggplot() +
    geom_bar(data = clips_tocheck, mapping = aes(x = datetime, y = annotation, fill= call_type), position = "stack", stat = "identity") + 
    labs(x = "Date", y = "Annotation", title = paste("Call Type Detection History for ",current_point, current_year)) +  # Labels for axes
    scale_x_datetime(date_breaks = "4 days") + # show every 5 days
    theme(axis.text.x = element_text(angle = 45,hjust = 1)) 
}

# Change these 
current_point <- "203-2"
current_year <- "2023"
year_clips <- clips_23
clips_tocheck <- year_clips %>% filter(point_id == current_point)
clips_tocheck %>% filter(annotation == 1)

jpeg(paste("./Data/Classifier_Results/Model2.0/Outputs/Plots_Filtering_Out_YBCU/Spp_Det_",current_point,".jpg"), width = 720, height = 577)
create_detection_plot(current_point = current_point,
                      current_year = current_year,
                      year_clips = year_clips)
dev.off()
call_type_plot(current_point = current_point,
                      current_year = current_year,
                      year_clips = year_clips)



 

# Make a facet wrap plot of the cuckoo detectionhttp://127.0.0.1:13745/graphics/plot_zoom_png?width=967&height=900
# filter out ybcu points from 2023
ybcu_23 <- ybcu_clips %>% filter(year == 2023)
ybcu_points_23 <- unique(ybcu_23$point_id)
# remove the ones that don't have any data
ybcu_points_23 <- ybcu_points_23[ybcu_points_23 != "203-2"]
# take a list of the points
# filter clips_23 to only be those points
clips_vis_23 <- clips_23 %>% filter(point_id %in% ybcu_points_23)
unique(clips_vis_23$point_id) # looks good
#add in the ggplot data
ggplot() +
  geom_bar(data = clips_vis_23, mapping = aes(x = datetime, y = annotation, fill= species), position = "stack", stat = "identity") + 
  labs(x = "Date", y = "Annotation", title = "Species Detection History 2023") +  # Labels for axes
  scale_x_datetime(date_breaks = "4 days") + # show every 5 days
  scale_fill_manual(values = c("BBCU" = "darkblue","UNK" = "darkgoldenrod2"))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  facet_wrap(~point_id, nrow = 5)
# dimensions to save this are 1500, 800
ggplot() +
  geom_bar(data = clips_vis_23, mapping = aes(x = datetime, y = annotation, fill= call_type), position = "stack", stat = "identity") + 
  labs(x = "Date", y = "Annotation", title = "Call Types 2023") +  # Labels for axes
  scale_x_datetime(date_breaks = "4 days") + # show every 5 days
  scale_fill_manual(values = c("cadence_coo" = "turquoise4","rattle" = "coral4"))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  facet_wrap(~point_id, nrow = 5)

ybcu_22 <- ybcu_clips %>% filter(year == 2022)
ybcu_points_22 <- unique(ybcu_22$point_id)
# Remove the ones that don't have any data
ybcu_points_22 <- ybcu_points_22[ybcu_points_22!= "FAR-2"]
ybcu_points_22 <- ybcu_points_22[ybcu_points_22!= "FAR-3"]
ybcu_points_22 <- ybcu_points_22[ybcu_points_22!= "HOL-1"]
ybcu_points_22 <- ybcu_points_22[ybcu_points_22!= "ELI-1"]
# take a list of the points
# filter clips_23 to only be those points
clips_vis_22 <- clips_22 %>% filter(point_id %in% ybcu_points_22)
unique(clips_vis_22$point_id) # looks good
#add in the ggplot data
ggplot() +
  geom_bar(data = clips_vis_22, mapping = aes(x = datetime, y = annotation, fill= species), position = "stack", stat = "identity") + 
  labs(x = "Date", y = "Annotation", title = "Species Detection History 2022") +  # Labels for axes
  scale_x_datetime(date_breaks = "4 days") + # show every 5 days
  scale_fill_manual(values = c("BBCU" = "darkblue","UNK" = "darkgoldenrod2"))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  facet_wrap(~point_id, nrow = 5)
# Make a plot for call types
ggplot() +
  geom_bar(data = clips_vis_22, mapping = aes(x = datetime, y = annotation, fill= call_type), position = "stack", stat = "identity") + 
  labs(x = "Date", y = "Annotation", title = "Call Types 2022") +  # Labels for axes
  scale_x_datetime(date_breaks = "4 days") + # show every 5 days
  scale_fill_manual(values = c("cadence_coo" = "turquoise4","rattle" = "coral4"))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  facet_wrap(~point_id, nrow = 5)

clips_22 %>% filter(point_id == "SID-1") %>% filter(annotation == 1)
#### Code Graveyard ####

# Original graphing
# # Select the site that has the potential YBCU
# current_point <- "PRD-1"
# current_year <- "2022"
# year_clips <- clips_22
# 
# clips_tocheck <- year_clips %>% filter(point_id == current_point)
# #pot_ybcu <- ybcu_clips %>% filter(point_id == current_point)
# 
# # Plot this
# ggplot() +
#   geom_bar(data = clips_tocheck, mapping = aes(x = datetime, y = annotation, fill= species), position = "stack", stat = "identity") + 
#   labs(x = "Date", y = "Annotation", title = paste("Detection History for ",current_point)) +  # Labels for axes
#   theme_minimal() + theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
#   scale_x_datetime(date_breaks = "5 days") # show every 5 days




# # replicate data:
# annotation <- c(1,0,0,1)
# date <- c("2023-06-06 09:00:00","2023-06-06 23:00:00","2023-06-07 07:00:00","2023-06-07 09:00:00")
# barplot(clips_to_check$annotation)
# barplot(table(iris$Species), ylab = "Frequency", main = "Species of Plant", xlab = "Species", col = "dark blue")


# 
# year_clips <- unite(year_clips,clip_unique,c(point_id,datetime),sep=" ",remove=FALSE)
# pot_ybcu <- unite(pot_ybcu,clip_unique,c(point_id,datetime),sep=" ",remove=FALSE)
# test <- year_clips %>% mutate(species = case_when(
#   annotation == 1 & clip_unique %in% pot_ybcu$clip_unique ~ "UNK",
#   annotation == 1 & !(clip_unique %in% pot_ybcu$clip_unique) ~ "BBCU",
#   annotation == 0 ~ "none"
# ))
# 
# year_clips %>% mutate(clip_unique = )

# ggplot(clips_tocheck, aes(x = date, y = annotation)) +
#   geom_bar(stat="identity") +  # Use points for each data point
#   labs(x = "Date", y = "Annotation") +  # Labels for axes
#   theme_minimal()  # Minimalist theme
# 
# 
# ggplot() +
#   geom_bar(data = clips_tocheck, mapping = aes(x = datetime, y = annotation),stat="identity", fill= "darkblue") + 
#   #geom_bar(data = pot_ybcu, mapping = aes(x = datetime), fill = "yellow3") +
#   labs(x = "Date", y = "Annotation") +  # Labels for axes
#   theme_minimal()  # Minimalist theme


