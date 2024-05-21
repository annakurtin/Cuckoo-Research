#### Create Community Comp Metric from Veg Survey Data ###################

## Purpose: to read in the cleaned data and try creating a variable for community composition of trees (and shrubs?)

# Created 5/20/2024

# Last modified: 5/20/2024

#TODO
## 

#### Setup #################################
packages <- c("tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_SiteColumn_fromPointID.R")
load_packages(packages)



#### Read in Data ###################################
tree <- read.csv("./Data/Vegetation_Data/Outputs/2023_VegSurvey_TreeData_Cleaned5-20.csv")
shrub <- read.csv("./Data/Vegetation_Data/Outputs/2023_VegSurvey_ShrubData_Cleaned5-20.csv")



#### Tree Data ######################################
# What are the unique values of species?
unique(tree$tree_species) # 12 unique tree species
spp_freq_t <- tree %>%
  group_by(tree_species) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# This is sites with that tree, not trees in the data

# Create a bar plot
ggplot(spp_freq_t, aes(x = reorder(tree_species, -count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Sites with Tree Species",
       x = "Tree Species",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# What is the distribution of the data?


# Have a variable for how many trees and what percentage of them are in each age class? again standardize to the site with the most trees




#### Shrub Data ###########################
# Have a value for the dominant shrub species in an area
dominant_shrub <- shrub %>% group_by(site_id) %>%
  reframe(
    max_x_cover = max(x_cover),
    shrub_species_max_x_cover = shrub_species[which.max(x_cover)]
  )
# Look at the distribution of this covariate
hist(dominant_shrub$max_x_cover)



#### ChatGPT Musings ######################


# Simulate data
point_id <- c("PRD-1","PRD-1","SNO-2","SNO-2","SNO-2")
shrub_species <- c("ARCA","ELAN","ROSA","SYAL","ELAN") # want a factor for each combination of species at a point
x_cover <- c(1,3,14,2,20) # want the sum
shrub_height_m <- c(0.50,0.75,1,0.5,2) # Want the standard deviation and the mean by point 
shrub_dat <- data.frame(point_id,shrub_species,x_cover,shrub_height_m)

# Testing chatgpts code
# Define a function to create the species combination factor
species_combination <- function(species) {
  # Define all possible species
  all_species <- c("ARCA", "ELAN", "ROSA", "SYAL")
  # Create a binary indicator for the presence of each species
  binary_vector <- as.integer(all_species %in% species)
  # Collapse to a single string
  paste(binary_vector, collapse = "")
}

# Summarize the data
result <- shrub_dat %>%
  group_by(point_id) %>%
  summarise(
    sum_x_cover = sum(x_cover),
    sd_shrub_height_m = sd(shrub_height_m),
    mean_shrub_height_m = mean(shrub_height_m),
    species_combination = species_combination(shrub_species)
  )
# Would a factor for species present be more informative or would a species diversity metric?
# Create metric by summarizing across point ID (other sheet)

test <- shrub %>% group_by(point_id) %>%
  summarise(
    max_x_cover = max(x_cover),
    shrub_species_max_x_cover = shrub_species[which.max(x_cover)]
  )