# Pull out/troubleshoot visualization code
library(tidyverse)
library(ggplot2)


# Landscape Scale Model 
fit_B <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Occupancy_Models/Models_Ran_Outputs/StateSubModel_BFinal3.Rdata")
# Read in unscaled data
us_dat_occ <- read.csv("./Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_UNSCALED_10-25.csv")


vis_effect <- function(covariate, dataframe, model, model_cov, xlabel, color_pal){
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
  
  # change this to be all the samples not just 200, then pull out the 2.5 to 97.5 intervals here
  # Initialize an empty array for the predicted effects of the generated data
  pred_array <- array(NA, dim = c(length(pd_scale), num_samples))
  # Iterate through each mcmc sample and take the predicted effects
  for(i in 1:num_samples){
    pred_array[,i] = plogis(model$sims.list$a0[i] + model$sims.list[[model_cov]][i]*pd_scale)
  }
  
  # Sample a subset of these observations for plotting 
  #subset <- sort(sample(1:num_samples, size = 200))
  
  # Plot original unscaled values against their predicted effects on probability of use
  plot(predict_dat, 
       cov_effect, 
       ylab = "Probability of Use", 
       xlab = xlabel, 
       ylim = c(0,1),
       type = "l",
       lwd = 3,
       frame.plot = FALSE)
  # Plot other draws from a distribution to get a sense of what the distribution looks like
  for(i in num_samples){
    lines(predict_dat, pred_array[,i], type = "l", lwd = 1, col = "gray")
  }
  lines(predict_dat, cov_effect, type = "l", lwd = 3, col = color_pal)
}


pe_canlandsc <- vis_effect(covariate = "pct_can_landsc",
                           dataframe = us_dat_occ,
                           model = fit_B,
                           model_cov = "a1",
                           xlabel = "% Canopy Landscape",
                           color_pal = l_palette[7])

# Why is this only plotting on line?