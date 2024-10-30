#### Create Predicted Effects For Occupancy and Linear Model Covariates ####


library(tidyverse)
library(ggplot2)
source("./R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")


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
fit_gpois <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Poisson_Model/Models_Ran_Outputs/LMod14_GamPois_14Day.Rdata")



# Pull out the number of samples
mcmc_samples_A <- fit_A$mcmc.info$n.samples
mcmc_samples_B <- fit_B$mcmc.info$n.samples
mcmc_samples_C <- fit_C$mcmc.info$n.samples


# Read in unscaled data
us_dat_occ <- read.csv("./Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_UNSCALED_10-25.csv")
us_dat_lm <- read.csv("./Data/Habitat_Model_Covariates/Linear_Model/DaysCalls_14DayPer_HabCovsUNSCALED_22-23_10-30.csv")



# Create a function for visualization
vis_effect_ci <- function(covariate, dataframe, model, model_cov, xlabel, color_pal){
  # Pull out the number of samples from the model
  num_samples <- model$mcmc.info$n.samples
  # Pull out standardization values
  mean_cov <- mean(dataframe[[covariate]], na.rm = TRUE)
  #return(mean_cov)
  sd_cov <- sd(dataframe[[covariate]], na.rm = TRUE)
  max_cov <- max(dataframe[[covariate]], na.rm = TRUE)
  
  # Generate data within the range of the covariate
  predict_dat <- seq(0, max_cov, length.out = 30)
  # Scale generated data
  pd_scale <- (predict_dat - mean_cov)/sd_cov
  # Pull the covariate effect from the model 
  cov_effect <- plogis(model$q50$a0 + model$q50[[model_cov]] * pd_scale)
  
  # Pull the credible interval
  ci_lower <- plogis(model$q50$a0 + model$q2.5[[model_cov]] * pd_scale)
  ci_upper <- plogis(model$q50$a0 + model$q97.5[[model_cov]] * pd_scale)
  
  # Plot original unscaled values against their predicted effects on probability of use
  plot(predict_dat, 
       cov_effect, 
       ylab = "Intensity of Use", 
       xlab = xlabel, 
       ylim = c(0,1.5),
       type = "l",
       lwd = 3,
       frame.plot = FALSE)
  # Add shaded credible interval
  polygon(c(predict_dat, rev(predict_dat)),  # X-coordinates: original and reversed
          c(ci_upper, rev(ci_lower)),        # Y-coordinates: upper and reversed lower CI
          col = "gray", border = NA)         # Fill color is gray, no border
  lines(predict_dat, cov_effect, type = "l", lwd = 3, col = color_pal)
}


pe_canlandsc <- vis_effect_ci(covariate = "pct_can_landsc",
              dataframe = us_dat_occ,
              model = fit_B,
              model_cov = "a1",
              xlabel = "% Canopy Landscape",
              color_pal = l_palette[7])

pe_seslandsc <- vis_effect_ci(covariate = "pct_openshrub_landsc",
              dataframe = us_dat_occ,
              model = fit_B,
              model_cov = "a2",
              xlabel = "% SES Landscape",
              color_pal = l_palette[5])


pe_sescore <- vis_effect_ci(covariate = "pct_openshrub_core",
              dataframe = us_dat_occ,
              model = fit_C,
              model_cov = "a1",
              xlabel = "% Open Shrub at Core Scale",
              color_pal = c_palette[1])

pe_vegcom <- vis_effect_ci(covariate = "veg_sd_resid",
              dataframe = us_dat_occ,
              model = fit_C,
              model_cov = "a4",
              xlabel = "Veg Complexity",
              color_pal = c_palette[6])


pe_conif <- vis_effect_ci(covariate = "ctree_spp_rich",
              dataframe = us_dat_occ,
              model = fit_A,
              model_cov = "a2",
              xlabel = "Conifer Spp Richness Point Scale",
              color_pal = p_palette[7])


# Add on linear model plots 

# Read these into a different file to format them with patchwork and make a multipanel figure