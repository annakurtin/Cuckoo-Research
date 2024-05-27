#### Create Community Comp Metric from Veg Survey Data ###################

## Purpose: to read in the cleaned data and try creating a variable for community composition of trees (and shrubs?)

# Created 5/20/2024

# Last modified: 5/242024

#### Setup #################################
packages <- c("tidyverse","janitor","forcats")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_SiteColumn_fromPointID.R")
source("./R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")
load_packages(packages)



#### Read in Data ###################################
tree_orig <- read.csv("./Data/Vegetation_Data/Outputs/2023_VegSurvey_TreeData_Cleaned5-24.csv")
#tree_points <- unique(tree$point_id)
shrub_orig <- read.csv("./Data/Vegetation_Data/Outputs/2023_VegSurvey_ShrubData_Cleaned5-24.csv")
#shrub_points <- unique(shrub$point_id)

# Remove the points that don't have an ARU at it
veg <- read.csv("./Data/Vegetation_Data/Outputs/2023_VegSurvey_MainData_Cleaned5-24.csv")
veg_waru <- veg %>% filter(aru_present == "yes")
points_waru <- veg_waru$point_id

tree <- tree_orig %>% filter(point_id %in% points_waru)
shrub <- shrub_orig %>% filter(point_id %in% points_waru)



#### Tree Data ######################################
# What are the unique values of species?
length(unique(tree$tree_species)) # 13 unique tree species
#tree[tree$tree_species == "UNTR",]
#tree[tree$tree_species == "PIPO",]

# Look at how sampling design affects the distribution of the tree species
tree_counts <- tree %>% group_by(site_id) %>% count(tree_species, sampling_design) %>% arrange(desc(n))
# Reorder
tree_counts <- tree_counts %>%
  mutate(tree_species = fct_reorder(tree_species, -n))
# Is there skew with non random points included?
ggplot(tree_counts, aes(x = tree_species, y = n, fill = sampling_design)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Sites with Tree Species",
       x = "Tree Species",
       y = "Number of Sites Present") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("habitat_grts" = palette_5[1], "mmr_grts" = palette_5[3], "selectedcu_nonrand" = palette_5[5]))
#### Something seems off to me about this graph????????????????????????????????????????????????

# See if the sampling design affects the distribution of the tree community 
# Creating native broadleaf species richness
native_broadleaf <-  c("ACNE","FRPE", "PANG","PDEL","POPU","PTRI","SAMY") 
non_native <- c("ELAN")
conifer <- c("CRDO","JUSC","PIPO","PSME")
tree <- tree %>% mutate(tree_comm = case_when(
  tree_species %in% native_broadleaf ~ "native_broadleaf",
  tree_species %in% non_native ~ "non_native",
  tree_species %in% conifer ~ "conifer"
))

# Create count of species richness
spp_rich_tree1 <- tree %>% filter(tree_comm == "native_broadleaf") %>% group_by(site_id) %>% reframe(spp_richness = length(unique(tree_species)))
# Join the sampling design to this
site_sampd <- tree %>% group_by(site_id) %>% reframe(sampling_design = first(sampling_design))
spp_rich_tree <- left_join(spp_rich_tree1, site_sampd, by = "site_id")
hist(spp_rich_tree$spp_richness)
# Not as wide of a distribution, make sure there are multiple points for each number
richness_table <- spp_rich_tree %>% group_by(spp_richness) %>% summarize(n=n()) 
richness_table
# Lots of points with only one species, only a handful with 3 or 4. Is there enough data to fit this?
ggplot(spp_rich_tree, aes(x = spp_richness, fill = sampling_design)) +
  geom_histogram(position = "dodge") +
  labs(title = "Distribution of Tree Species Richness",
       x = "Tree Species Richness") +
  scale_fill_manual(values = c("habitat_grts" = palette_5[1], "mmr_grts" = palette_5[3], "selectedcu_nonrand" = palette_5[5]))
# It doesn't look like the nonrandom sites are introducing major bias into this

# Write this to .csv
write.csv(spp_rich_tree,"./Data/Habitat_Model_Covariates/Occupancy_Covariates/2023_ARUSites_TreeSppRich_5-24.csv",row.names = FALSE)



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
# Look at the distribution of percent cover per species
ggplot(data = shrub, mapping = aes(x = shrub_species, y = x_cover)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
# Look at the distribution of height per species
ggplot(data = shrub, mapping = aes(x = shrub_species, y = shrub_height_m)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
# Save these plots?
# Make sure that ISA and YWM-1, MUSH-184, MUSH-060, MUSH-169 has data even though it didn't have any shrubs present
# Looks good

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Where should invasive species go?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Based on Veg_Codes_Cheatsheet in resources folder
floodplain_spp <- c("PANG","PDEL","POPU","PTRI","SAEX","SALI","SAMY")
misc_broadl_spp <- c("FRPE","CRDO","COST","ATCO","ACNE","LONI","PRUN","PRVI","RHTR","RIBE","ROSA","SHCA","SHAR","SYAL","TOXI","UNBR")
upland_spp <- c("ARCA","ARTR","ARAB","ARLU","GUSA","JUSC","JUCO","JUHO","KRLA","PIPO","PSME","SAVE","YUGA")
invasives <- c("ELAN","TAMA")
broadleaf_winvasives <- c("FRPE","CRDO","COST","ATCO","ACNE","LONI","PRUN","PRVI","RHTR","RIBE","ROSA","SHCA","SHAR","SYAL","TOXI","UNBR","ELAN","TAMA")

##### First: treating the invasives separately 
shrub_invas_sep <- shrub %>% mutate(shrub_comm = case_when(
  shrub_species %in% floodplain_spp ~ "floodplain",
  shrub_species %in% misc_broadl_spp ~ "misc_broadleaf",
  shrub_species %in% upland_spp ~ "upland",
  shrub_species %in% invasives ~ "invasive"
))
# Are there any sites that are majority invasive?
shrub_cov_sum_sep <- shrub_invas_sep %>% group_by(site_id,shrub_comm) %>% summarize(total_cover = sum(x_cover))
# Create dominant shrub community
dominant_shrub_sep <- shrub_cov_sum_sep %>% group_by(site_id) %>%
  reframe(
    max_cover = max(total_cover),
    dominant_community = shrub_comm[which.max(total_cover)]
  )
shrub_samp <- shrub %>% group_by(site_id) %>% reframe(sampling_design = first(sampling_design))
dominant_shrub_sep <- left_join(dominant_shrub_sep, shrub_samp, by = "site_id")

# Plot this
jpeg(paste("./Deliverables/Create_Habitat_Covariates/Habitat_Chapter/Shrub_Dominant_Community/Shrub_Community_Invasives_Separated.jpg"), width = 800, height = 600)
ggplot(dominant_shrub_sep, aes(x = fct_relevel(dominant_community, "floodplain","misc_broadleaf","upland","invasive"), fill = dominant_community)) +
  geom_histogram(stat = "count") +
  labs(title = "Shrub Communities - Invasives Separated",
       x = "Shrub Community",
       y = "Number of Sites") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  scale_fill_manual(values = c("floodplain" = palette_8[2],"misc_broadleaf" = palette_8[4], "upland" = palette_8[6], "invasive" = palette_8[8])) +
  scale_y_continuous(limits = c(0,60),breaks = c(10,20,30,40,50,60))
dev.off()

# Look at the distribution of sampling design
jpeg(paste("./Deliverables/Create_Habitat_Covariates/Habitat_Chapter/Shrub_Dominant_Community/Shrub_Community_by_Sampling_Design_Invasives_Separated.jpg"), width = 800, height = 600)
ggplot(dominant_shrub_sep, aes(x = fct_relevel(dominant_community, "floodplain","misc_broadleaf","upland","invasive"), fill = sampling_design)) +
  geom_histogram(stat = "count", position = "dodge") +
  labs(title = "Community by Sampling Design - Invasives Separated",
       x = "Shrub Community",
       y = "Number of Sites") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  scale_y_continuous(limits = c(0,60),breaks = c(10,20,30,40,50,60))+
  scale_fill_manual(values = c("habitat_grts" = palette_5[1], "mmr_grts" = palette_5[3], "selectedcu_nonrand" = palette_5[5]))
dev.off()

##### What about if we combine the invasives with the broadleaf, does this significantly change the communities?
shrub_invas_comb <-  shrub %>% mutate(shrub_comm = case_when(
  shrub_species %in% floodplain_spp ~ "floodplain",
  shrub_species %in% broadleaf_winvasives ~ "broadleaf_winvasive",
  shrub_species %in% upland_spp ~ "upland"
))
#
shrub_cov_sum_comb <- shrub_invas_comb %>% group_by(site_id,shrub_comm) %>% summarize(total_cover = sum(x_cover))
# Create dominant shrub community 
dominant_shrub_comb <- shrub_cov_sum_comb %>% group_by(site_id) %>%
  reframe(
    max_cover = max(total_cover),
    dominant_community = shrub_comm[which.max(total_cover)]
  )
dominant_shrub_comb <- left_join(dominant_shrub_comb, shrub_samp, by = "site_id")
# Plot this
jpeg(paste("./Deliverables/Create_Habitat_Covariates/Habitat_Chapter/Shrub_Dominant_Community/Shrub_Community_Invasives_Combined.jpg"), width = 800, height = 600)
ggplot(dominant_shrub_comb, aes(x = fct_relevel(dominant_community, "floodplain","broadleaf_winvasive","upland"), fill = dominant_community)) +
  geom_histogram(stat = "count") +
  labs(title = "Shrub Communities - Invasives Included with Broadleaf",
       x = "Shrub Community",
       y = "Number of Sites") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  scale_fill_manual(values = c("floodplain" = palette_8[2],"broadleaf_winvasive" = palette_8[4], "upland" = palette_8[6])) +
  scale_y_continuous(limits = c(0,60),breaks = c(10,20,30,40,50,60))
dev.off()

# Look at the distribution of sampling design
jpeg(paste("./Deliverables/Create_Habitat_Covariates/Habitat_Chapter/Shrub_Dominant_Community/Shrub_Community_by_Sampling_Design_Invasives_Combined.jpg"), width = 800, height = 600)
ggplot(dominant_shrub_comb, aes(x = fct_relevel(dominant_community, "floodplain","broadleaf_winvasive","upland"), fill = sampling_design)) +
  geom_histogram(stat = "count", position = "dodge") +
  labs(title = "Community by Sampling Design - Invasives in Broadleaf",
       x = "Shrub Community",
       y = "Number of Sites") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  scale_y_continuous(limits = c(0,60),breaks = c(10,20,30,40,50,60))+
  scale_fill_manual(values = c("habitat_grts" = palette_5[1], "mmr_grts" = palette_5[3], "selectedcu_nonrand" = palette_5[5]))
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Invasives are the dominant shrub community at 10 sites
# Since this is a fairly large category, it makes sense to me to leave this separate
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Write the one you're going to use to .csv
write.csv(dominant_shrub_sep,"./Data/Habitat_Model_Covariates/Occupancy_Covariates/2023_ARUSites_ShrubDominantCommunity_5-24.csv",row.names = FALSE)






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

# # Refine this once you know which species go in which communities for sure
# dominant_shrub %>% group_by(dominant_community) %>% summarize(n=n())
# # There are 23 + points for these

# Old visualization code
ggplot(dominant_shrub_sep, aes(x = dominant_community, fill = dominant_community)) +
  geom_histogram(stat = "count") +
  labs(title = "Distribution of Shrub Communities Invasives Separated",
       x = "Shrub Community",
       y = "Number of Sites") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  scale_fill_manual(values = c("floodplain" = col1,"misc_broadleaf" = col2, "upland" = col3, "invasive" = col4))


# Looking at the distribution of the trees
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

# # Test if tree species is same as tree_sp_other_collated
# test <- tree %>% mutate(same = ifelse(tree_species == tree_sp_other_collated,"Y","N"))
# test[test$same == "N"]
# # Nothing, this is good 

# tree <- tree %>% mutate(native_broadl = ifelse(tree_species %in% native_broadleaf,1,0))
# tree2 <- tree %>% filter(native_broadl ==1)
# tree_richness <- tree2 %>% group_by(site_id) %>% summarize(native_broadl = n_distinct(tree_species))
# # Test out if it worked correctly
# tree2[tree2$site_id == "203",]
# tree_richness[tree_richness$site_id == "203",]
# Looks good
# Look at distribution

# tree_species <- c("ELAN","PDEL","PDEL","FRPE")
# sampling_design <- c("nonrand","grts","grts","nonrand")
#survey_met_spp <- tree %>% group_by(tree_species,sampling_design) %>% summarize(n=n()) %>% arrange(desc(n))
# Better way

#native_broadleaf <-  c("PDEL", "PANG","ACNE", "POPU","FRPE", "SAMY", "PTRI")
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

# # Compare this to all the points
# point_dat <- read.csv("./Data/Monitoring_Points/2023_AllARUPoints_FromDeploymentData.csv")
# all_points <- unique(point_dat$point_id)
# veg <- read.csv("./Data/Vegetation_Data/Outputs/2023_VegSurvey_MainData_Cleaned5-24.csv")
# all_veg <- unique(veg$point_id)
# # 195 veg points total
# # Which veg plots are missing from the tree points?
# # Values in veg that aren't in tree
# notree_points <- setdiff(all_veg, tree_points)
# veg %>% filter(site_id == "ISA")
# shrub %>% filter(is.na(shrub_species))
# # Go back and look at the veg surveys for these points to make sure they aren't supposed to have a tree
# # Figure out where the overlap is here ??????????????????????????????????

# Old designations
# floodplain_spp <- c("SALI", "SAMY", "SAEX", "POPU", "PDEL", "PANG", "PTRI")
# misc_broadl_spp <- c("ROSA", "SYAL", "PRVI", "RIBE","TOXI", "SHCA", "COST", "FRPE", "ACNE", "CRDO")
# upland_spp <- c("ARCA", "JUSC", "ARTR", "JUCO", "SAVE", "PIPO", "PSME")
# olive <- "ELAN"
# all_other_broadleaf <- c("ELAN", "ROSA", "SYAL", "PRVI", "RIBE","TOXI", "SHCA", "COST", "FRPE", "ACNE", "CRDO")
#tree_richness1 <- tree %>% group_by(site_id) %>% summarize(native_broadl_tree = sum(native_broadl))

# ARCA: silver sagebrush - upland
# ELAN: russian olive - other broadleaf
# ROSA: rose spp - other broadleaf
# SYAL: snowberry - other broadleaf
# PRVI: choke cherry - other broadleaf
# RIBE: currant spp - other broadleaf
# TOXI: poison ivy - other broadleaf
# RHTR????????? skunkbush sumac - rhus trilobata
# POPU: cottonwood spp - floodplain
# UNSH: unknown (exclude)
# JUSC: rocky mtn juniper - upland
# SHCA: Buffaloberry - other broadleaf
# ARTR: big sagebrush - upland
# SALI: willow spp - floodplain
# COST: dogwood - other broadleaf
# FRPE: green ash - other broadleaf
# SAMY: peach leaf willow - floodplain
# PDEL: plains cottonwood - floodplain
# PANG: narrowleaf cottonwood - floodplain
# SAEX: sandbar willow - floodplain
# JUCO: common juniper - upland
# ACNE: box elder - other broadleaf
# PTRI: black cottonwood - floodplain
# CRDO: black hawthorn -other broadleaf
# SAVE: greasewood - upland 
# PIPO: ponderosa pine- upland
# PSME: doug fir - upland
# other: ?????
