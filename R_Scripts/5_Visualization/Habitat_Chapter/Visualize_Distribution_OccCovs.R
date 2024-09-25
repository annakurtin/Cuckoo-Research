# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Look at distribution of unscaled habitat chapter covariates ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Used in the modeling methods section of the habitat chapter.

# read in data
us_dat <- read.csv("./Data/Habitat_Model_Covariates/Occupancy_Covariates/All_2023Veg_Covariates_9-24.csv")
# REMOVE THE SITES THAT WE DIDNT ACTUALLY LOOK AT

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Point Scale Covariates ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hist(us_dat$dtree_spp_rich, main = "Decid Tree Spp Richness", xlab = "Deciduous Tree Richness (# spp)")
mean(us_dat$dtree_spp_rich, na.rm = TRUE)
sd(us_dat$dtree_spp_rich, na.rm = TRUE)
min(us_dat$dtree_spp_rich, na.rm = TRUE)
max(us_dat$dtree_spp_rich, na.rm = TRUE)

hist(us_dat$ctree_spp_rich, main = "Conifer Tree Spp Richness", xlab = "Conifer Richness (# spp)")
mean(us_dat$ctree_spp_rich, na.rm = TRUE)
sd(us_dat$ctree_spp_rich, na.rm = TRUE)
min(us_dat$ctree_spp_rich, na.rm = TRUE)
max(us_dat$ctree_spp_rich, na.rm = TRUE)

floodplain_sites <- nrow(us_dat[us_dat$floodplain_shrub == 1,])
(floodplain_sites/nrow(us_dat)) * 100

broadleaf_sites <- nrow(us_dat[us_dat$broadleaf_shrub == 1,])
(broadleaf_sites/nrow(us_dat)) * 100

# Broadleaf and floodplain shrub are binary covariates


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Landscape Scale Covariates ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hist(us_dat$pct_can_landsc, main = "Percent Canopy Landscape", xlab = "Canopy Density (%)")
mean(us_dat$pct_can_landsc, na.rm = TRUE)
sd(us_dat$pct_can_landsc, na.rm = TRUE)
min(us_dat$pct_can_landsc, na.rm = TRUE)
max(us_dat$pct_can_landsc, na.rm = TRUE)

hist(us_dat$pct_subcan_landsc, main = "Percent Subcanopy Landscape", xlab = "Subcanopy Density (%)")
mean(us_dat$pct_subcan_landsc, na.rm = TRUE)
sd(us_dat$pct_subcan_landsc, na.rm = TRUE)

hist(us_dat$pct_openshrub_landsc, main = "Percent Open Shrub Landscape", xlab = "Open Shrub Density (%)")
mean(us_dat$pct_openshrub_landsc, na.rm = TRUE)
sd(us_dat$pct_openshrub_landsc, na.rm = TRUE)
min(us_dat$pct_openshrub_landsc, na.rm = TRUE)
max(us_dat$pct_openshrub_landsc, na.rm = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Core Scale Covariates ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hist(us_dat$pct_openshrub_core, main = "Percent Shrub/Eary Successional Core")
mean(us_dat$pct_openshrub_core, na.rm = TRUE)
sd(us_dat$pct_openshrub_core, na.rm = TRUE)
min(us_dat$pct_openshrub_core, na.rm = TRUE)
max(us_dat$pct_openshrub_core, na.rm = TRUE)

hist(us_dat$ht_can_core, main = "Canopy Height Core")
mean(us_dat$ht_can_core, na.rm = TRUE)
sd(us_dat$ht_can_core, na.rm = TRUE)
min(us_dat$ht_can_core, na.rm = TRUE)
max(us_dat$ht_can_core, na.rm = TRUE)

hist(us_dat$ht_subcan_core, main = "Subcanopy Height Core")
mean(us_dat$ht_subcan_core, na.rm = TRUE)
sd(us_dat$ht_subcan_core, na.rm = TRUE)
min(us_dat$ht_subcan_core, na.rm = TRUE)
max(us_dat$ht_subcan_core, na.rm = TRUE)

hist(us_dat$veg_sd_resid, main = "St Dev All Veg Core Residuals from Regression with Canopy Height")
mean(us_dat$veg_sd_resid, na.rm = TRUE)
sd(us_dat$veg_sd_resid, na.rm = TRUE)
min(us_dat$veg_sd_resid, na.rm = TRUE)
max(us_dat$veg_sd_resid, na.rm = TRUE)

hist(us_dat$pct_can_core, main = "Percent Canopy Core")
mean(us_dat$pct_can_core, na.rm = TRUE)
sd(us_dat$pct_can_core, na.rm = TRUE)

hist(us_dat$pct_subcan_core, main = "Percent Subcanopy Core")
mean(us_dat$pct_subcan_core, na.rm = TRUE)
sd(us_dat$pct_subcan_core, na.rm = TRUE)

hist(us_dat$sd_allveg_core, main = "St Dev All Veg Core")
mean(us_dat$sd_allveg_core, na.rm = TRUE)
sd(us_dat$sd_allveg_core, na.rm = TRUE)

hist(us_dat$veg_sd_resid, main = "St Dev All Veg Core Residuals from Regression with Canopy Height")
mean(us_dat$veg_sd_resid, na.rm = TRUE)
sd(us_dat$veg_sd_resid, na.rm = TRUE)

hist(us_dat$sd_subcan_core, main = "St Dev Subcanopy Core")
mean(us_dat$sd_subcan_core, na.rm = TRUE)
sd(us_dat$sd_subcan_core, na.rm = TRUE)
