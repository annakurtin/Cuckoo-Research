##### Arrange Habitat Chapter Plots ####
library(ggplot2)
library(patchwork)
source("./R_Scripts/5_Visualization/Habitat_Chapter/Manuscript_Plots/CreatePlots_PosteriorDist_OccLinMod.R")
source("./R_Scripts/5_Visualization/Habitat_Chapter/Manuscript_Plots/CreatePlots_PredictedEffects_OccLinMod.R")

# As a note, this uses the patchwork package to arrange plots. For custom layouts and arrangements, see this reference: https://patchwork.data-imaginist.com/articles/guides/layout.html

dense_detocc
ggsave("./Deliverables/HabChap_ModelVisualizations/DensityPlot_DetSubmod_7-14.jpg", width=8, height=6)
# Haven't created predicted effects for covariates that vary by survey - if I decide that I want to include these plots I'll need to figure this out

# Plot posterior density and predicted effects of significant covariates
# Point scale 
#dense_pointocc / free(pe_decid | pe_conif | pe_broadl | pe_flood)+ plot_layout(widths = c(1,1,1,1), axes = 'collect')
dense_pointocc / free(pe_decid | pe_conif | pe_broadl | pe_flood)
#dense_pointocc / (pe_decid | pe_conif | pe_broadl | pe_flood) + plot_layout(widths = c(1,1,1,1), heights = c(1, 1), axes = "collect")
ggsave("./Deliverables/HabChap_ModelVisualizations/DensityPredEffectsPlot_PointScale_7-14.jpg", width=14, height=8)

# Core scale
dense_coreocc / free(pe_vegcom | pe_sesheight | pe_canheight | pe_sescore)
ggsave("./Deliverables/HabChap_ModelVisualizations/DensityPredEffectsPlot_CoreScale_7-14.jpg", width=14, height=8)

# Landscape scale
dense_lanscocc / free(plot_spacer() |pe_canlandsc | pe_seslandsc | plot_spacer())
ggsave("./Deliverables/HabChap_ModelVisualizations/DensityPredEffectsPlot_LandscScale_7-14.jpg", width=14, height=8)

# Linear model 
dense_lm
ggsave("./Deliverables/HabChap_ModelVisualizations/DensityPlot_LinearMod_7-14.jpg", width=8, height=6)

(pe_lm_vegdense | pe_lm_noise | pe_lm_recdays) / (pe_lm_conif | pe_lm_sescore | pe_lm_vegcom) / (pe_lm_seslansc | pe_lm_canlansc | plot_spacer())
ggsave("./Deliverables/HabChap_ModelVisualizations/PredEffectsPlot_LinearMod_7-14.jpg", width=8, height=6)

# Just trying this
#dense_lm / ((pe_lm_vegdense | pe_lm_noise | pe_lm_recdays) / (pe_lm_conif | pe_lm_sescore | pe_lm_vegcom) / (pe_lm_seslansc | pe_lm_canlansc))
# UGLY!!!!

# Total: 6 figures