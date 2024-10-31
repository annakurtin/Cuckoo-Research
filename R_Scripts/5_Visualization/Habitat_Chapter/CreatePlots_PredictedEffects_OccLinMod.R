#### Create Predicted Effects For Occupancy and Linear Model Covariates ####


library(tidyverse)
library(ggplot2)
source("./R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")
source("./R_Scripts/6_Function_Scripts/Create_Function_Visualize_Predicted_Effects.R")


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
pe_detaru <- vis_effect(covariate = "SM_present",
                               dataframe = us_dat_occ,
                               model = habdet,
                               intercept = "b0",
                               model_cov = "b5",
                               xlabel = "SongMeter Present (1/0)",
                               color_pal = d_palette[10])

pe_detveg <- vis_effect(covariate = "veg_density_avg",
                               dataframe = us_dat_occ,
                               model = habdet,
                                intercept = "b0",
                               model_cov = "b1",
                               xlabel = "Composite Veg Density",
                               ylabel = NULL,
                               color_pal = d_palette[8])
#### How to do this with covariates that vary by survey???
# pe_deteffort <- vis_effect_ci_det(covariate = "effort",
#                                dataframe = us_dat_occ,
#                                model = habdet,
#                                model_cov = "b3",
#                                xlabel = "Survey Effort (Combined Hours)",
#                                ylabel = NULL,
#                                color_pal = d_palette[8])


#### Landscape ####
pe_canlandsc <- vis_effect(covariate = "pct_can_landsc",
                              dataframe = us_dat_occ,
                              model = fit_B,
                              intercept = "a0",
                              model_cov = "a1",
                              xlabel = "% Canopy",
                              color_pal = l_palette[7])

pe_seslandsc <- vis_effect(covariate = "pct_openshrub_landsc",
                              dataframe = us_dat_occ,
                              model = fit_B,
                              intercept = "a0",
                              model_cov = "a2",
                              xlabel = "% SES",
                              ylabel = NULL,
                              color_pal = l_palette[5])


#### Core Area ####
pe_vegcom <- vis_effect(covariate = "veg_sd_resid",
                           dataframe = us_dat_occ,
                           model = fit_C,
                           intercept = "a0",
                           model_cov = "a4",
                           xlabel = "Veg Complexity",
                           color_pal = c_palette[6])

pe_sesheight <- vis_effect(covariate = "ht_subcan_core",
                            dataframe = us_dat_occ,
                            model = fit_C,
                            intercept = "a0",
                            model_cov = "a3",
                            xlabel = "SES Height (m)",
                            ylabel = NULL,
                            color_pal = c_palette[4])

pe_canheight <- vis_effect(covariate = "ht_can_core",
                              dataframe = us_dat_occ,
                              model = fit_C,
                              intercept = "a0",
                              model_cov = "a2",
                              xlabel = "Canopy Height (m)",
                              ylabel = NULL,
                              color_pal = c_palette[3])

pe_sescore <- vis_effect(covariate = "pct_openshrub_core",
                            dataframe = us_dat_occ,
                            model = fit_C,
                            intercept = "a0",
                            model_cov = "a1",
                            xlabel = "% SES",
                            ylabel = NULL,
                            color_pal = c_palette[1])



### Point ####
pe_decid <- vis_effect(covariate = "dtree_spp_rich",
                          dataframe = us_dat_occ,
                          model = fit_A,
                          intercept = "a0",
                          model_cov = "a1",
                          xlabel = "Decid Spp Richness",
                          color_pal = p_palette[9])

pe_conif <- vis_effect(covariate = "ctree_spp_rich",
                          dataframe = us_dat_occ,
                          model = fit_A,
                          intercept = "a0",
                          model_cov = "a2",
                          xlabel = "Conifer Spp Richness",
                          ylabel = NULL,
                          color_pal = p_palette[7])

pe_flood <- vis_effect(covariate = "floodplain_shrub",
                          dataframe = us_dat_occ,
                          model = fit_A,
                          intercept = "a0",
                          model_cov = "a3",
                          xlabel = "Floodplain Shrub Dominated",
                          ylabel = NULL,
                          color_pal = p_palette[3])

pe_broadl <- vis_effect(covariate = "broadleaf_shrub",
                          dataframe = us_dat_occ,
                          model = fit_A,
                          intercept = "a0",
                          model_cov = "a4",
                          xlabel = "Broadleaf Shrub Dominated",
                          ylabel = NULL,
                          color_pal = p_palette[5])


# Add on linear model plots
pe_lm_vegdense <- vis_effect(covariate = "avg_vegdense",
                                dataframe = us_dat_lm,
                                model = fit_gpois,
                                intercept = "beta0",
                                model_cov = "beta8",
                                xlabel = "Composite Veg Density",
                                color_pal = d_palette[8])

pe_lm_noise <- vis_effect(covariate = "avg_db",
                                   dataframe = us_dat_lm,
                                   model = fit_gpois,
                                   intercept = "beta0",
                                   model_cov = "beta7",
                                   xlabel = "Avg Background Noise (dB)",
                                   ylabel = NULL,
                                   color_pal = d_palette[4])
# This looks weird????

pe_lm_recdays <- vis_effect(covariate = "combined_days_rec",
                                  dataframe = us_dat_lm,
                                  model = fit_gpois,
                                  intercept = "beta0",
                                  model_cov = "beta6",
                                  xlabel = "Combined Days Recorded at Site",
                                  ylabel = NULL,
                                  color_pal = d_palette[6])

pe_lm_conif <- vis_effect(covariate = "ctree_spp_rich",
                                  dataframe = us_dat_lm,
                                  model = fit_gpois,
                                  intercept = "beta0",
                                  model_cov = "beta5",
                                  xlabel = "Conifer Spp Richness",
                                  color_pal = p_palette[7])

pe_lm_sescore <- vis_effect(covariate = "pct_openshrub_core",
                                dataframe = us_dat_lm,
                                model = fit_gpois,
                                intercept = "beta0",
                                model_cov = "beta4",
                                xlabel = "% SES Core Area",
                                ylabel = NULL,
                                color_pal = c_palette[1])

pe_lm_vegcom <- vis_effect(covariate = "veg_sd_resid",
                                  dataframe = us_dat_lm,
                                  model = fit_gpois,
                                  intercept = "beta0",
                                  model_cov = "beta3",
                                  xlabel = "Vegetation Complexity",
                                  ylabel = NULL,
                                  color_pal = c_palette[6])

pe_lm_seslansc <- vis_effect(covariate = "pct_openshrub_landsc",
                                 dataframe = us_dat_lm,
                                 model = fit_gpois,
                                 intercept = "beta0",
                                 model_cov = "beta2",
                                 xlabel = "% SES Landscape",
                                 color_pal = l_palette[5])

pe_lm_canlansc <- vis_effect(covariate = "pct_can_landsc",
                                   dataframe = us_dat_lm,
                                   model = fit_gpois,
                                   intercept = "beta0",
                                   model_cov = "beta1",
                                   xlabel = "% Canopy Landscape",
                                   ylabel = NULL,
                                   color_pal = l_palette[7])



# Read these into a different file to format them with patchwork and make a multipanel figure





#### Graveyard ####
# Old data: don't think it's needed since it's in the function
# # Pull out the number of samples
# mcmc_samples_A <- fit_A$mcmc.info$n.samples
# mcmc_samples_B <- fit_B$mcmc.info$n.samples
# mcmc_samples_C <- fit_C$mcmc.info$n.samples
# mcmc_samples_lm <- fit_gpois$mcmc.info$n.samples

# Old function: now in Function_Scripts Create Function Visualize Predicted Effects
# vis_effect_ci <- function(covariate, dataframe, model, model_cov, xlabel, color_pal){
#   # Pull out the number of samples from the model
#   num_samples <- model$mcmc.info$n.samples
#   # Pull out standardization values
#   mean_cov <- mean(dataframe[[covariate]], na.rm = TRUE)
#   #return(mean_cov)
#   sd_cov <- sd(dataframe[[covariate]], na.rm = TRUE)
#   max_cov <- max(dataframe[[covariate]], na.rm = TRUE)
#   
#   # Generate data within the range of the covariate
#   predict_dat <- seq(0, max_cov, length.out = 30)
#   # Scale generated data
#   pd_scale <- (predict_dat - mean_cov)/sd_cov
#   # Pull the covariate effect from the model 
#   cov_effect <- plogis(model$q50$a0 + model$q50[[model_cov]] * pd_scale)
#   
#   # Pull the credible interval
#   ci_lower <- plogis(model$q50$a0 + model$q2.5[[model_cov]] * pd_scale)
#   ci_upper <- plogis(model$q50$a0 + model$q97.5[[model_cov]] * pd_scale)
#   
#   # Plot original unscaled values against their predicted effects on probability of use
#   plot(predict_dat, 
#        cov_effect, 
#        ylab = "Intensity of Use", 
#        xlab = xlabel, 
#        ylim = c(0,1.5),
#        type = "l",
#        lwd = 3,
#        frame.plot = FALSE)
#   # Add shaded credible interval
#   polygon(c(predict_dat, rev(predict_dat)),  # X-coordinates: original and reversed
#           c(ci_upper, rev(ci_lower)),        # Y-coordinates: upper and reversed lower CI
#           col = "gray", border = NA)         # Fill color is gray, no border
#   lines(predict_dat, cov_effect, type = "l", lwd = 3, col = color_pal)
# }