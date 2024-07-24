###### Evaluate Distributino LiDAR Derivatives #####

# This is a script to read in the cleaned lidar derivatives of canopy height and canopy cover and evaluate how they align with random vs nonrandom points

# date create 7/18/20234
# last modified 7/23/2024

##### packages and functions #####
packages <- c("tidyverse","janitor","ggplot2","corrgram")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")
source("./R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")
load_packages(packages)

# Andy wants to see what the covariate distributions look like excluding or including those points
## He would worry if including those piotns created a totally separated/bimodal distribution of the covariates## if its making a slightly shifted or broader distribution thats fine
## slight oversampling of the points

##### data ######
all <- read.csv("./Data/Vegetation_Data/Outputs/AllPoints_AllScales_LiDARMetrics_7-23-24.csv")
all <- all %>% separate(alt_point_id, into = c('point_id','year'), sep = "_", remove = FALSE)
all <- all %>% create_site_col()
all <- all %>% create_samp_col()
all <- all %>% mutate(grts_grouped = case_when(sampling_design == "nonrand" ~ "nonrand",
                                               sampling_design == "mmr_grts"~ "grts",
                                               sampling_design == "habitat_grts" ~ "grts"))
# How many sites in each category?
mmr <- all[all$sampling_design == "mmr_grts",] # 165 sites
length(unique(mmr$site_id)) #13
habitat <- all[all$sampling_design == "habitat_grts",] # 251 sites
length(unique(habitat$site_id)) #89
nonrand <- all[all$sampling_design == "nonrand",]
length(unique(nonrand$site_id)) #37


make_hist <- function(data = all,cov, min_x=0){
  plot_data <- data[[cov]]
  max_x <- max(plot_data, na.rm = TRUE)
  brks <- 20
  # pink is nonrandom, yellow is habitat or MMR GRTS points
  hist(plot_data, col = NA, border = "black", lwd = 2, xlim = c(min_x,max_x), breaks = brks, main = cov)
  hist(plot_data[data$grts_grouped == "grts"], col = alpha(palette_5[3],.5), xlim = c(min_x,max_x), breaks = brks, add = TRUE)
  hist(plot_data[data$grts_grouped == "nonrand"], col = alpha(palette_5[5],.5), xlim = c(min_x,max_x), breaks = brks, add = TRUE)
  legend("topright", legend = c("Overall", "Nonrandom", "GRTS"), col = c("black", palette_5[5], palette_5[3]), lwd = 2, title = "Sampling Design")
}


### Look at distributions ####
# Put this into a markdown to show advisors and have the boxplots side by side to each other (in heading set out.width = 48%)
# landscape
# Visual comparison
boxplot(all$percent_canopy_landsc~all$sampling_design, main = "% Canopy Landscape")
length(all[all$sampling_design == "mmr_grts",]) # 12 sitesboxplot(all$percent_canopy_landsc~all$grts_grouped, main = "% Canopy Landscape")
# is this an artifact of having 30 of the GRTS points in the upper missouri breaks?
# Perform Kruskal-Wallis test if assumptions are not met
kruskal.test(percent_canopy_landsc ~ sampling_design, data = all) # significant
make_hist(all, "percent_canopy_landsc")

# Percent Subcanopy Landscape
boxplot(all$percent_subcan_landsc~all$sampling_design, main = "% Subcan Landscape")
boxplot(all$percent_subcan_landsc~all$grts_grouped, main = "% Subcan Landscape")
kruskal.test(percent_subcan_landsc ~ sampling_design, data = all) # not significant
make_hist(cov ="percent_subcan_landsc")

# Percent Canopy Core
boxplot(all$percent_canopy_core~all$sampling_design, main = "% Canopy Core")
boxplot(all$percent_canopy_core~all$grts_grouped, main = "% Canopy Core")
kruskal.test(percent_canopy_core ~ sampling_design, data = all) # significant
make_hist(cov = "percent_canopy_core")

# Percent Subcanopy Core
boxplot(all$percent_subcan_core~all$sampling_design, main = "% Subcan Core")
boxplot(all$percent_subcan_core~all$grts_grouped, main = "% Subcan Core")
kruskal.test(percent_subcan_core ~ sampling_design, data = all) # significant
make_hist(cov = "percent_subcan_core")

# Average Canopy Height of Core Area
boxplot(all$canopy_avgheight_core~all$sampling_design, main = "Canopy Height Core")
# it seems to me that the difference in this is because the mmr_grts are all just in an area that has tall old growth cottonwood forest. What would this look like if we combined the GRTS classifications together?
boxplot(all$canopy_avgheight_core~all$grts_grouped, main = "Canopy Height Core")
kruskal.test(canopy_avgheight_core ~ sampling_design, data = all)
kruskal.test(canopy_avgheight_core ~ grts_grouped, data = all) # still a significant difference between the group
make_hist(cov = "canopy_avgheight_core", min_x = 6)

# Average Subcanopy Height of Core Area
boxplot(all$subcan_avgheight_core~all$sampling_design, main = "Subcanopy Height Core")
boxplot(all$subcan_avgheight_core~all$grts_grouped, main = "Subcanopy Height Core")
kruskal.test(canopy_avgheight_core ~ sampling_design, data = all)
make_hist(cov = "subcan_avgheight_core")

# Subcanopy Standard Deviation of Core Area
boxplot(all$subcan_stdev_core~all$sampling_design, main = "Subcanopy St Dev Core")
boxplot(all$subcan_stdev_core~all$grts_grouped, main = "Subcanopy St Dev Core")
kruskal.test(subcan_stdev_core ~ sampling_design, data = all) # significant
make_hist(cov = "subcan_stdev_core")

# All Veg Standard Deviation of Core Area
boxplot(all$all_stdev_core~all$sampling_design, main = "Subcanopy St Dev Core")
boxplot(all$all_stdev_core~all$grts_grouped, main = "Subcanopy St Dev Core")
kruskal.test(all_stdev_core ~ sampling_design, data = all) # significant
make_hist(cov = "all_stdev_core")



##### CODE GRAVEYARD #####

max_1 <- max(all$percent_canopy_landsc)
brks <- 20
# pink is nonrandom, purple is habitat or MMR GRTS points
hist(all$percent_canopy_landsc, col = NA, border = "black", xlim = c(0,max_1), breaks = brks, main = "% Canopy Landscape")
hist(all$percent_canopy_landsc[all$grts_grouped == "nonrand"], col = palette_5[2], border = NA, xlim = c(0,max_1), breaks = brks, add = TRUE)
hist(all$percent_canopy_landsc[all$grts_grouped == "grts"], col = palette_5[3], border = NA, xlim = c(0,max_1), breaks = brks, add = TRUE)
cov <- "percent_canopy"



# Trying out the anova
# shapiro.test(all$percent_canopy_landsc[all$sampling_design == "habitat_grts"])
# hist(all$percent_canopy_landsc[all$sampling_design == "habitat_grts"]) # not normal, can't do an ANOVA test
# # I could center and scale this covariate and then test it?
# test <- all %>% mutate(p_canopy_scaled = scale(percent_canopy_landsc))
# shapiro.test(test$p_canopy_scaled[test$sampling_design == "habitat_grts"])
# hist(test$p_canopy_scaled[test$sampling_design == "habitat_grts"])
# # Centering and scaling this doesn't fix/make it normal
# # Try kruskal test

#all %>% create_site_col()
# test <- all %>%
#   mutate(sampling_design = case_when(
#     # If point_id has four letters then a dash then three numbers
#     grepl("^[[:alpha:]]{4}-[[:digit:]]{3}$", point_id) ~ "habitat_grts",
#     # If point_id has two numbers, three numbers, or three letters then a dash then one number
#     grepl("^[[:digit:]]{2,3}-[[:digit:]]{1}$", point_id) ~ "mmr_grts",
#     grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$", point_id) ~ "nonrand"
#   ))
