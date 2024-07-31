#### Look at distribution of covariates across shrub type ####

packages <- c("tidyverse","janitor","ggplot2","corrgram","ggpubr")
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")

# Read in data
all_dat <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_SCALED_7-30.csv")


palette_5[2]

create_shrub_fig <- function(dataframe, shrub, ylab){
  # % Canopy Cover
  f1 <- ggplot(data = dataframe, aes(x = pct_can_landsc, y = shrub)) +
    geom_point(color = palette_5[1]) +
    stat_smooth(method = "glm", method.args = list(family = "binomial"),se = FALSE,color = palette_5[1]) +
    labs(y = ylab) +
    scale_x_continuous(limits = c(-2,2), breaks = seq(-2,2, by =1))
  # % Subcanopy Landsc
  f2 <- ggplot(data = dataframe, aes(x = pct_subcan_landsc, y = shrub)) +
    geom_point(color = palette_5[2]) +
    stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = palette_5[2])+
    labs(y = ylab) +
    scale_x_continuous(limits = c(-2,2), breaks = seq(-2,2, by =1))
  # % Subcanopy Core
  f3 <- ggplot(data = dataframe, aes(x = pct_subcan_core, y = shrub)) +
    geom_point(color = palette_5[3]) +
    stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = palette_5[3])+
    labs(y = ylab) +
    scale_x_continuous(limits = c(-2,2), breaks = seq(-2,2, by =1))
  # All Veg SD
  f4 <- ggplot(data = dataframe, aes(x = sd_allveg_core, y = shrub)) +
    geom_point(color = palette_5[4]) +
    stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = palette_5[4])+
    labs(y = ylab) +
    scale_x_continuous(limits = c(-2,2), breaks = seq(-2,2, by =1))
  # Subcanopy height
  f5 <- ggplot(data = dataframe, aes(x = ht_subcan_core, y = shrub)) +
    geom_point(color = palette_5[5]) +
    stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = palette_5[5])+
    labs(y = ylab) +
    scale_x_continuous(limits = c(-2,2), breaks = seq(-2,2, by =1))
  
  fig <- ggarrange(f1, f2, f3, f4, f5, 
                             labels = c("A", "B", "C", "D", "E"),
                             ncol = 3, nrow = 2)
  return(fig)
}


broadleaf <- create_shrub_fig(all_dat, all_dat$broadleaf_shrub, "broadleaf shrub")
invasive <- create_shrub_fig(all_dat, all_dat$invasive_shrub, "invasive shrub")
upland <- create_shrub_fig(all_dat, all_dat$upland_shrub, "upland shrub")
floodplain <- create_shrub_fig(all_dat, all_dat$floodplain_shrub, "floodplain shrub")

# How many of each kind?
sum(all_dat$broadleaf_shrub, na.rm = TRUE)
sum(all_dat$invasive_shrub, na.rm = TRUE)
sum(all_dat$upland_shrub, na.rm = TRUE)
sum(all_dat$floodplain_shrub, na.rm = TRUE)

# How many presences are there?
broadl <- all_dat %>% filter(broadleaf_shrub == 1) 
sum(broadl)


# look at tree species richness with other covariates
plot(all_dat)






# Archive
# plot(all_dat$broadleaf_shrub ~ all_dat$pct_can_landsc)
# 
# # % Canopy Cover
#m1 <- glm(pct_can_landsc ~ broadleaf_shrub, data = all_dat, family = "binomial")
 bl1 <- ggplot(data = all_dat, aes(x = pct_can_landsc, y = broadleaf_shrub)) +
    geom_point(color = "#332288") +
    stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) 
# # % Subcanopy Landsc
# bl2 <- ggplot(data = all_dat, aes(x = pct_subcan_landsc, y = broadleaf_shrub)) +
#   geom_point() +
#   stat_smooth(method = "glm", se = FALSE)
# # % Subcanopy Core
# bl3 <- ggplot(data = all_dat, aes(x = pct_subcan_core, y = broadleaf_shrub)) +
#   geom_point() +
#   stat_smooth(method = "glm", se = FALSE)
# # All Veg SD
# bl4 <- ggplot(data = all_dat, aes(x = sd_allveg_core, y = broadleaf_shrub)) +
#   geom_point() +
#   stat_smooth(method = "glm", se = FALSE)
# # Subcanopy height
# bl5 <- ggplot(data = all_dat, aes(x = ht_subcan_core, y = broadleaf_shrub)) +
#   geom_point() +
#   stat_smooth(method = "glm", se = FALSE)
# 
# broadleaf_fig <- ggarrange(bl1, bl2, bl3, bl4, bl5, 
#                            labels = c("A", "B", "C", "D", "E"),
#                            ncol = 3, nrow = 2)
