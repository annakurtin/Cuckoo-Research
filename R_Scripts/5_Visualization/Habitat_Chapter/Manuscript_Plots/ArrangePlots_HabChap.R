##### Arrange Habitat Chapter Plots ####
library(ggplot2)
library(patchwork)
source("./R_Scripts/5_Visualization/Habitat_Chapter/Manuscript_Plots/CreatePlots_PosteriorDist_OccLinMod.R")
source("./R_Scripts/5_Visualization/Habitat_Chapter/Manuscript_Plots/CreatePlots_PredictedEffects_OccLinMod.R")

# As a note, this uses the patchwork package to arrange plots. For custom layouts and arrangements, see this reference: https://patchwork.data-imaginist.com/articles/guides/layout.html

dense_detocc
ggsave("./Deliverables/HabChap_ModelVisualizations/DensityPlot_DetSubmod.jpg", width=8, height=6)
# Haven't created predicted effects for covariates that vary by survey - if I decide that I want to include these plots I'll need to figure this out

# Plot posterior density and predicted effects of significant covariates
# Point scale 
dense_pointocc / free(pe_decid | pe_conif | pe_broadl | pe_flood)
ggsave("./Deliverables/HabChap_ModelVisualizations/DensityPredEffectsPlot_PointScale.jpg", width=12, height=8)

# Core scale
dense_coreocc / free(pe_vegcom | pe_sesheight | pe_canheight | pe_sescore)
ggsave("./Deliverables/HabChap_ModelVisualizations/DensityPredEffectsPlot_CoreScale.jpg", width=12, height=8)

# Landscape scale
dense_lanscocc / free(pe_canlandsc | pe_seslandsc)
ggsave("./Deliverables/HabChap_ModelVisualizations/DensityPredEffectsPlot_LandscScale.jpg", width=12, height=8)

# Linear model 
dense_lm
ggsave("./Deliverables/HabChap_ModelVisualizations/DensityPlot_LinearMod.jpg", width=8, height=6)

(pe_lm_vegdense | pe_lm_noise | pe_lm_recdays) / (pe_lm_conif | pe_lm_sescore | pe_lm_vegcom) / (pe_lm_seslansc | pe_lm_canlansc | plot_spacer())
ggsave("./Deliverables/HabChap_ModelVisualizations/PredEffectsPlot_LinearMod.jpg", width=8, height=6)

# Just trying this
#dense_lm / ((pe_lm_vegdense | pe_lm_noise | pe_lm_recdays) / (pe_lm_conif | pe_lm_sescore | pe_lm_vegcom) / (pe_lm_seslansc | pe_lm_canlansc))
# UGLY!!!!

# Total: 6 figures