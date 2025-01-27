#### Create Predicted Effects For Occupancy and Linear Model Covariates ####


library(tidyverse)
library(ggplot2)
source("./R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")
source("./R_Scripts/6_Function_Scripts/Create_Function_Visualize_OccPredicted_Effects_forTalk.R")
#source("./R_Scripts/6_Function_Scripts/Create_Function_Visualize_LMPredicted_Effects.R")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in models #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Detection model
habdet <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Detection_Submodel/Models_Ran_Outputs/Actual_Data/DetSubModel_Final4.Rdata")

# Landscape Scale Model 
fit_B <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Occupancy_Models/Models_Ran_Outputs/StateSubModel_BFinal3.Rdata")

# Core Scale Model
fit_C <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Occupancy_Models/Models_Ran_Outputs/StateSubModel_CFinal3.Rdata")

# Point Scale Model
fit_A <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Occupancy_Models/Models_Ran_Outputs/StateSubModel_AFinal3.Rdata")

# Linear model
fit_gpois <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Poisson_Model/Models_Ran_Outputs/LMod15_GamPois_14Day.Rdata")

# Read in unscaled data
us_dat_occ <- read.csv("./Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_UNSCALED_10-25.csv")
us_dat_lm <- read.csv("./Data/Habitat_Model_Covariates/Linear_Model/DaysCalls_14DayPer_HabCovsUNSCALED_22-23_10-30.csv")


#### Detection ####
# pe_detaru <- vis_effect_occ(covariate = "SM_present",
#                                dataframe = us_dat_occ,
#                                model = habdet,
#                                intercept = "b0",
#                                model_cov = "b5",
#                                xlabel = "ARU Type",
#                                color_pal = d_palette[10])
# 
# pe_detveg <- vis_effect_occ(covariate = "veg_density_avg",
#                                dataframe = us_dat_occ,
#                                model = habdet,
#                                 intercept = "b0",
#                                model_cov = "b1",
#                                xlabel = "Vegetation Density",
#                                ylabel = NULL,
#                                color_pal = d_palette[8])
#### How to do this with covariates that vary by survey???
# pe_deteffort <- vis_effect_ci_det(covariate = "effort",
#                                dataframe = us_dat_occ,
#                                model = habdet,
#                                model_cov = "b3",
#                                xlabel = "Survey Effort (Combined Hours)",
#                                ylabel = NULL,
#                                color_pal = d_palette[8])


#### Landscape ####
pe_canlandsc <- vis_effect_occ(covariate = "pct_can_landsc",
                              dataframe = us_dat_occ,
                              model = fit_B,
                              intercept = "a0",
                              model_cov = "a1",
                              xlabel = "% Canopy Cover",
                              ylabel = NULL,
                              color_pal = l_palette[7])
ggsave("./Deliverables/HabChap_ModelVisualizations/Occ_Mods_Effect_Plots_Separated/L_PctCan.jpg", width=4, height=6)


pe_seslandsc <- vis_effect_occ(covariate = "pct_openshrub_landsc",
                              dataframe = us_dat_occ,
                              model = fit_B,
                              intercept = "a0",
                              model_cov = "a2",
                              xlabel = "% Open Shrub Cover",
                              ylabel = NULL,
                              color_pal = l_palette[5])
ggsave("./Deliverables/HabChap_ModelVisualizations/Occ_Mods_Effect_Plots_Separated/L_PctSES.jpg", width=4, height=6)


#### Core Area ####
pe_vegcom <- vis_effect_occ(covariate = "veg_sd_resid",
                           dataframe = us_dat_occ,
                           model = fit_C,
                           intercept = "a0",
                           model_cov = "a4",
                           xlabel = "Vegetation Complexity",
                           ylabel = NULL,
                           color_pal = c_palette[6])
ggsave("./Deliverables/HabChap_ModelVisualizations/Occ_Mods_Effect_Plots_Separated/C_VegComplex.jpg", width=4, height=6)

pe_sesheight <- vis_effect_occ(covariate = "ht_subcan_core",
                            dataframe = us_dat_occ,
                            model = fit_C,
                            intercept = "a0",
                            model_cov = "a3",
                            xlabel = "Open Shrub Height (m)",
                            ylabel = NULL,
                            color_pal = c_palette[4])
ggsave("./Deliverables/HabChap_ModelVisualizations/Occ_Mods_Effect_Plots_Separated/C_SEShHt.jpg", width=4, height=6)

pe_canheight <- vis_effect_occ(covariate = "ht_can_core",
                              dataframe = us_dat_occ,
                              model = fit_C,
                              intercept = "a0",
                              model_cov = "a2",
                              xlabel = "Canopy Height (m)",
                              ylabel = NULL,
                              color_pal = c_palette[3])
ggsave("./Deliverables/HabChap_ModelVisualizations/Occ_Mods_Effect_Plots_Separated/C_CanHt.jpg", width=4, height=6)

pe_sescore <- vis_effect_occ(covariate = "pct_openshrub_core",
                            dataframe = us_dat_occ,
                            model = fit_C,
                            intercept = "a0",
                            model_cov = "a1",
                            xlabel = "% Open Shrub Cover",
                            ylabel = NULL,
                            color_pal = c_palette[1])
ggsave("./Deliverables/HabChap_ModelVisualizations/Occ_Mods_Effect_Plots_Separated/C_PctSES.jpg", width=4, height=6)



### Point ####
pe_decid <- vis_effect_occ(covariate = "dtree_spp_rich",
                          dataframe = us_dat_occ,
                          model = fit_A,
                          intercept = "a0",
                          model_cov = "a1",
                          xlabel = "Deciduous Tree Richness",
                          ylabel = NULL,
                          color_pal = p_palette[9])
ggsave("./Deliverables/HabChap_ModelVisualizations/Occ_Mods_Effect_Plots_Separated/P_DecidRich.jpg", width=4, height=6)

pe_conif <- vis_effect_occ(covariate = "ctree_spp_rich",
                          dataframe = us_dat_occ,
                          model = fit_A,
                          intercept = "a0",
                          model_cov = "a2",
                          xlabel = "Conifer Tree Richness",
                          ylabel = NULL,
                          color_pal = p_palette[7])
ggsave("./Deliverables/HabChap_ModelVisualizations/Occ_Mods_Effect_Plots_Separated/P_ConifRich.jpg", width=4, height=6)

pe_flood <- vis_effect_occ(covariate = "floodplain_shrub",
                          dataframe = us_dat_occ,
                          model = fit_A,
                          intercept = "a0",
                          model_cov = "a3",
                          xlabel = "Floodplain Shrub",
                          ylabel = NULL,
                          color_pal = p_palette[3])
ggsave("./Deliverables/HabChap_ModelVisualizations/Occ_Mods_Effect_Plots_Separated/P_FloodShrub.jpg", width=4, height=6)

pe_broadl <- vis_effect_occ(covariate = "broadleaf_shrub",
                          dataframe = us_dat_occ,
                          model = fit_A,
                          intercept = "a0",
                          model_cov = "a4",
                          xlabel = "Broadleaf Shrub",
                          ylabel = NULL,
                          color_pal = p_palette[5])
ggsave("./Deliverables/HabChap_ModelVisualizations/Occ_Mods_Effect_Plots_Separated/P_BroadShrub.jpg", width=4, height=6)



#### Linear model ####
pe_lm_vegdense <- vis_effect_lm(covariate = "avg_vegdense",
                                dataframe = us_dat_lm,
                                model = fit_gpois,
                                intercept = "beta0",
                                model_cov = "beta8",
                                xlabel = "Vegetation Density",
                                color_pal = d_palette[8])

pe_lm_noise <- vis_effect_lm(covariate = "avg_db",
                                   dataframe = us_dat_lm,
                                   model = fit_gpois,
                                   intercept = "beta0",
                                   model_cov = "beta7",
                                   xlabel = "Background Noise (dB)",
                                   ylabel = NULL,
                                   color_pal = d_palette[4])
# This looks weird????

pe_lm_recdays <- vis_effect_lm(covariate = "combined_days_rec",
                                  dataframe = us_dat_lm,
                                  model = fit_gpois,
                                  intercept = "beta0",
                                  model_cov = "beta6",
                                  xlabel = "Recording Days",
                                  ylabel = NULL,
                                  color_pal = d_palette[6])

pe_lm_conif <- vis_effect_lm(covariate = "ctree_spp_rich",
                                  dataframe = us_dat_lm,
                                  model = fit_gpois,
                                  intercept = "beta0",
                                  model_cov = "beta5",
                                  xlabel = "Conifer Tree Richness",
                                   ylabel = NULL,
                                  color_pal = p_palette[7])

pe_lm_sescore <- vis_effect_lm(covariate = "pct_openshrub_core",
                                dataframe = us_dat_lm,
                                model = fit_gpois,
                                intercept = "beta0",
                                model_cov = "beta4",
                                xlabel = "% SES Core",
                                ylabel = NULL,
                                color_pal = c_palette[1])

pe_lm_vegcom <- vis_effect_lm(covariate = "veg_sd_resid",
                                  dataframe = us_dat_lm,
                                  model = fit_gpois,
                                  intercept = "beta0",
                                  model_cov = "beta3",
                                  xlabel = "Vegetation Complexity",
                                  ylabel = NULL,
                                  color_pal = c_palette[6])

pe_lm_seslansc <- vis_effect_lm(covariate = "pct_openshrub_landsc",
                                 dataframe = us_dat_lm,
                                 model = fit_gpois,
                                 intercept = "beta0",
                                 model_cov = "beta2",
                                 xlabel = "% SES Landscape",
                                  ylabel = NULL,
                                 color_pal = l_palette[5])

pe_lm_canlansc <- vis_effect_lm(covariate = "pct_can_landsc",
                                   dataframe = us_dat_lm,
                                   model = fit_gpois,
                                   intercept = "beta0",
                                   model_cov = "beta1",
                                   xlabel = "% Canopy Landscape",
                                   ylabel = NULL,
                                   color_pal = l_palette[7])



# Read these into a different file to format them with patchwork and make a multipanel figure



