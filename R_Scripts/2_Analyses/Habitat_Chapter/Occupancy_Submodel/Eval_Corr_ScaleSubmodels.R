### Creating Occupancy Sub Models Based on Biological Scales of Interest ####


all_dat <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Occupancy_Covariates/All_Veg_Covariates_7-24.csv")
library(corrgram)
library(tidyverse)

# Create dummy variables for shrub community
unique(all_dat$dominant_community) # four categories
all_dat <- all_dat %>% mutate(broadleaf_shrub = case_when(dominant_community == "misc_broadleaf" ~ 1,
                                               dominant_community == "invasive" ~ 0,
                                               dominant_community == "upland" ~ 0,
                                               dominant_community == "floodplain" ~ 0),
                           invasive_shrub = case_when(dominant_community == "misc_broadleaf" ~ 0,
                                                       dominant_community == "invasive" ~ 1,
                                                       dominant_community == "upland" ~ 0,
                                                       dominant_community == "floodplain" ~ 0),
                           upland_shrub = case_when(dominant_community == "misc_broadleaf" ~ 0,
                                                       dominant_community == "invasive" ~ 0,
                                                       dominant_community == "upland" ~ 1,
                                                       dominant_community == "floodplain" ~ 0),
                           floodplain_shrub = case_when(dominant_community == "misc_broadleaf" ~ 0,
                                                       dominant_community == "invasive" ~ 0,
                                                       dominant_community == "upland" ~ 0,
                                                       dominant_community == "floodplain" ~ 1))
#test <- test %>% select(dominant_community, broadleaf_shrub, invasive_shrub, upland_shrub, floodplain_shrub)

#### Sub Model B1: Point scale covariates ####
# Dominant shrub community at the point scale
# Tree richness at the point scale

boxplot(all_dat$spp_richness~all_dat$dominant_community)
ggplot(all_dat) +
  geom_histogram(aes(x=spp_richness)) +
  facet_wrap(~ dominant_community)



#### Sub Model B2: Landscape scale covariates ####
# Percent canopy cover at landscape scale
# Percent subcanopy cover and landscape scale

landsc <- all_dat[,7:8]
corrgram(landsc, order=TRUE, lower.panel=panel.cor,
         upper.panel=panel.pts, text.panel=panel.txt,
         main="Correlations in Landscape Scale Metrics")
#There are no colinear covariates to account for in this submodel. 



#### Sub Model B3: Core home range scale covariates ####
# Percent canopy cover at core scale
# Percent subcanopy cover at core scale
# Average height of canopy at core scale
# Average height of subcanopy at core scale
# Standard deviation of subcanopy at core scale

core <- all_dat[,12:18]
corrgram(core, order=TRUE, lower.panel=panel.cor,
         upper.panel=panel.pts, text.panel=panel.txt,
         main="Correlations in Veg Density Metrics")
#There are three colinear relationships to account for: subcanopy standard deviation and height, subcanopy height and percent canopy cover, and percent canopy cover and canopy height.
# make a regression 
# make a new covariate for veg standard deviation using the residuals from a regression with canopy height 
# create the regression model
reg <- glm(formula = sd_allveg_core ~ ht_can_core, data = core)
# predict new values of y based on regression model
sd_predict <- predict(reg, core)
# take differene between actual and predicted values
sd_resid <- core$sd_allveg_core - sd_predict
# add it to the data
core$veg_sd_resid <- round(sd_resid,2)
plot(core$sd_allveg_core ~ core$ht_can_core)
abline(reg)
plot(sd_predict ~ core$sd_allveg_core)

plot(core$veg_sd_resid ~ core$ht_can_core)
# no longer colinear! 