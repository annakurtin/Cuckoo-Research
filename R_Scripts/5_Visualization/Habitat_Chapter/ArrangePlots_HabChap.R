##### Arrange Methods Chapter Plots ####
library(ggplot2)
library(patchwork)
source("./R_Scripts/5_Visualization/Habitat_Chapter/CreatePlots_PosteriorDist_OccLinMod.R")
source("./R_Scripts/5_Visualization/Habitat_Chapter/CreatePlots_PredictedEffects_OccLinMod.R")

# 10/31/2024: adding in all of the poisterior effects

dense_detocc
# Haven't created predicted effects for covariates that vary by survey - if I decide that I want to include these plots I'll need to figure this out

# Plot posterior density and predicted effects of significant covariates
# Point scale 
dense_pointocc / (pe_decid | pe_conif | pe_broadl | pe_flood)

# Core scale
dense_coreocc / (pe_vegcom | pe_sesheight | pe_canheight | pe_sescore)

# Landscape scale
dense_lanscocc / (pe_canlandsc | pe_seslandsc)

# Linear model 
dense_lm

(pe_lm_vegdense | pe_lm_noise | pe_lm_recdays) / (pe_lm_conif | pe_lm_sescore | pe_lm_vegcom) / (pe_lm_seslansc | pe_lm_canlansc)

# Total: 6 figures