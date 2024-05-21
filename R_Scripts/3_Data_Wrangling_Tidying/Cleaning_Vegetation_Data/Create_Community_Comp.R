#### Create Community Comp Metric from Veg Survey Data ###################

## Purpose: to read in the cleaned data and try creating a variable for community composition of trees (and shrubs?)

# Created 5/20/2024

# Last modified: 5/20/2024

#TODO
## Figure out why some of the entires don't have a point_id
## Potentially use process of elimination to figure out which is the missing point
## Then write the output to csv

#### Setup #################################
packages <- c("tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_SiteColumn_fromPointID.R")
load_packages(packages)



#### Read in Data ###################################
tree <- read.csv("./Data/Vegetation_Data/Outputs/2023_VegSurvey_TreeData_Cleaned5-20.csv")
#### Why do some of these entries not have a point_id??????????????????????????
shrub <- read.csv("./Data/Vegetation_Data/Outputs/2023_VegSurvey_ShrubData_Cleaned5-20.csv")
#### Why do some of these entries not have a point_id??????????????????????????

#### Tree Data ######################################
# What are the unique values of species?
unique(tree$tree_species) # 12 unique tree species
## To include: Native Broadleaf Species
# PDEL: plains cottonwood
# PANG: narrowleaf cottonwood
# ACNE: boxelder
# POPU: unk cottonwood
# FRPE: green ash
# SAMY: peach leaf willow
# PTRI: black cottonwood
## To exclude: non natives and conifers
# JUSC: Rocky Mtn Juniper
# PIPO: ponderosa pine
# PSME: douglas fir
# ELAN: Russian Olive
# UNTR: unknown (at YELL-182 and SRB-2)
# *no data*

tree[tree$tree_species == "UNTR",]
tree[tree$tree_species == "PIPO",]

spp_freq_t <- tree %>%
  group_by(tree_species) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# This is sites with that tree, not trees in the data

# What is the distribution of the data?
ggplot(spp_freq_t, aes(x = reorder(tree_species, -count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Sites with Tree Species",
       x = "Tree Species",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Test if tree species is same as tree_sp_other_collated
test <- tree %>% mutate(same = ifelse(tree_species == tree_sp_other_collated,"Y","N"))
test[test$same == "N"]
# Nothing, this is good 

# Creating native broadleaf species richness
native_broadleaf <-  c("PDEL", "PANG","ACNE", "POPU","FRPE", "SAMY", "PTRI")
tree <- tree %>% mutate(native_broadl = ifelse(tree_species %in% native_broadleaf,1,0))
tree2 <- tree %>% filter(native_broadl ==1)
tree_richness <- tree2 %>% group_by(site_id) %>% summarize(native_broadl = n_distinct(tree_species))
# Test out if it worked correctly
tree2[tree2$site_id == "203",]
tree_richness[tree_richness$site_id == "203",]
# Looks good
# Look at distribution
hist(tree_richness$native_broadl)
# Not as wide of a distribution, make sure there are multiple points for each number
tree_richness %>% group_by(native_broadl) %>% summarize(n=n()) 
# There are only two points with four different types of broadleaf species - is this enough?

# Write this to .csv



#### Shrub Data ###########################
# Have a value for the dominant shrub species in an area
dominant_shrub <- shrub %>% group_by(site_id) %>%
  reframe(
    max_x_cover = max(x_cover),
    shrub_species_max_x_cover = shrub_species[which.max(x_cover)]
  )
# Look at the distribution of this covariate
hist(dominant_shrub$max_x_cover)
unique(shrub$shrub_species)
# Should I include ELAN in these????

floodplain_spp <- c("SALI", "SAMY", "SAEX")
misc_broadl_spp <- c("ELAN","ROSA", "SYAL", "PRVI", "RIBE","TOXI", "SHCA", "COST", "FRPE", "ACNE", "CRDO")
upland_spp <- c("ARCA", "JUSC", "ARTR", "JUCO", "SAVE", "PIPO", "PSME")
# ARCA: silver sagebrush - upland
# ELAN: russian olive - other broadleaf
# ROSA: rose spp - other broadleaf
# SYAL: snowberry - other broadleaf
# PRVI: choke cherry - other broadleaf
# RIBE: currant spp - other broadleaf
# TOXI: poison ivy - other broadleaf
# RHTR?????????
# POPU: cottonwood spp - other broadleaf ??????????????????
# UNSH: unknown (exclude)
# JUSC: rocky mtn juniper - upland
# SHCA: Buffaloberry - other broadleaf
# ARTR: big sagebrush - upland
# SALI: willow spp - floodplain
# COST: dogwood - other broadleaf
# FRPE: green ash - other broadleaf
# SAMY: peach leaf willow - floodplain
# PDEL: plains cottonwood - other broadleaf???????????????????????????
# PANG: narrowleaf cottonwood - other broadleaf ?????????????????
# SAEX: sandbar willow - floodplain
# JUCO: common juniper - upland
# ACNE: box elder - other broadleaf
# PTRI: black cottonwood - other broadleaf??????????????
# CRDO: black hawthorn -other broadleaf
# SAVE: greasewood - upland 
# PIPO: ponderosa pine- upland
# PSME: doug fir - upland
# other: ?????
test <- shrub %>% mutate(shrub_comm = case_when(
  shrub_species %in% floodplain_spp ~ "floodplain",
  shrub_species %in% misc_broadl_spp ~ "misc_broadleaf",
  shrub_species %in% upland_spp ~ "upland"
))

test <- test %>% group_by(site_id,shrub_comm) %>% summarize(total_cover = sum(x_cover))

dominant_shrub <- test %>% group_by(site_id) %>%
  reframe(
    max_cover = max(total_cover),
    dominant_community = shrub_comm[which.max(total_cover)]
  )
# Refine this once you know which species go in which communities for sure


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


#### Code Graveyard ########################
#tree_richness1 <- tree %>% group_by(site_id) %>% summarize(native_broadl_tree = sum(native_broadl))
