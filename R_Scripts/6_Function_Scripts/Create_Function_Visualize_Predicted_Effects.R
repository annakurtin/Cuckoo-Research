#### Create Function to Plot Predicted Effects ####

# Inputs:
# covariate: the name of the covariate of interest within the dataframe you're using (character)
# datafram: the unscaled data to use to plot the predicted effects (dataframe)
# model: the JAGS model from which to pull predicted effects (model file)
# model_cov: the name of the parameter within the model that corresponds to the covaraite (character)
# xlabel: the label to write on the x axis for the unscaled covariate, including units if applicable (character)
# ylabel: the label to write on the y axis, created with a default so it can be removed if wanted for the multipanel figures
# color_pal: the color palette and the color within the palette to use to visualize 


# Create a function for visualization
vis_effect_ci <- function(covariate, dataframe, model, model_cov, xlabel, ylabel = "Intensity of Use",color_pal){
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
  
  # Create a data frame for ggplot
  plot_data <- data.frame(
    predict_dat = predict_dat,
    cov_effect = cov_effect,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
  
  # Plot using ggplot2
  ggplot(plot_data, aes(x = predict_dat, y = cov_effect)) +
    # Add shaded credible interval
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "gray", alpha = 0.5) +
    # Add main effect line
    geom_line(color = color_pal, linewidth = 1.5) +
    # Labels and theme adjustments
    labs(x = xlabel, y = ylabel) +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 1.5)) +
    theme(axis.line = element_line(color = "black", size = 0.8),
          axis.text.x = element_text(size = 13), 
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13))
}


# Create a function for visualization of linear models (just changed the intercept value in the model to reflect the linear model)
vis_effect_ci_lm <- function(covariate, dataframe, model, model_cov, xlabel,ylabel = "Intensity of Use", color_pal){
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
  cov_effect <- plogis(model$q50$beta0 + model$q50[[model_cov]] * pd_scale)
  
  # Pull the credible interval
  ci_lower <- plogis(model$q50$beta0 + model$q2.5[[model_cov]] * pd_scale)
  ci_upper <- plogis(model$q50$beta0 + model$q97.5[[model_cov]] * pd_scale)
  
  # Create a data frame for ggplot
  plot_data <- data.frame(
    predict_dat = predict_dat,
    cov_effect = cov_effect,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
  
  # Plot using ggplot2
  ggplot(plot_data, aes(x = predict_dat, y = cov_effect)) +
    # Add shaded credible interval
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "gray", alpha = 0.5) +
    # Add main effect line
    geom_line(color = color_pal, linewidth = 1.5) +
    # Labels and theme adjustments
    labs(x = xlabel, y = ylabel) +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 1.5)) +
    theme(axis.line = element_line(color = "black", size = 0.8),
          axis.text.x = element_text(size = 13), 
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13))
}


# Create a function for visualization of linear models (just changed the intercept value in the model to reflect the detection model)
vis_effect_ci_det <- function(covariate, dataframe, model, model_cov, xlabel,ylabel = "Intensity of Use", color_pal){
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
  cov_effect <- plogis(model$q50$b0 + model$q50[[model_cov]] * pd_scale)
  
  # Pull the credible interval
  ci_lower <- plogis(model$q50$b0 + model$q2.5[[model_cov]] * pd_scale)
  ci_upper <- plogis(model$q50$b0 + model$q97.5[[model_cov]] * pd_scale)
  
  # Create a data frame for ggplot
  plot_data <- data.frame(
    predict_dat = predict_dat,
    cov_effect = cov_effect,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
  
  # Plot using ggplot2
  ggplot(plot_data, aes(x = predict_dat, y = cov_effect)) +
    # Add shaded credible interval
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "gray", alpha = 0.5) +
    # Add main effect line
    geom_line(color = color_pal, linewidth = 1.5) +
    # Labels and theme adjustments
    labs(x = xlabel, y = ylabel) +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 1.5)) +
    theme(axis.line = element_line(color = "black", size = 0.8),
          axis.text.x = element_text(size = 13), 
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 13),
          axis.title.y = element_text(size = 13))
}
  
# Original: baseplot after line 22
  # # Plot original unscaled values against their predicted effects on probability of use
  # plot(predict_dat, 
  #      cov_effect, 
  #      ylab = "Intensity of Use", 
  #      xlab = xlabel, 
  #      ylim = c(0,1.5),
  #      type = "l",
  #      lwd = 3,
  #      frame.plot = FALSE)
  # # Add shaded credible interval
  # polygon(c(predict_dat, rev(predict_dat)),  # X-coordinates: original and reversed
  #         c(ci_upper, rev(ci_lower)),        # Y-coordinates: upper and reversed lower CI
  #         col = "gray", border = NA)         # Fill color is gray, no border
  # lines(predict_dat, cov_effect, type = "l", lwd = 3, col = color_pal)

