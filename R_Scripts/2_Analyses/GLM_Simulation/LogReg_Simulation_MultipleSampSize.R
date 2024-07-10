#### GLM Script ####

# Date created: 7/10/2024
# Last modified: 7/10/2024

# This is a script to run a glm on different amounts of data and parameters in order to get a sense of how many data points we need to estimate relationships of a binary response variable with eight covariates
## This would be an example of data collected on something like the probability of nest success (1/0) in an area as a response of different environmental covariates

#### Only change these lines, then run the rest ####
# Specify the range of sample sizes you want to test
sites_range <- seq(20,1000,by=20)
# Specify the number of covariates you want to model
number_covariates <- 8

#### Establish Functions ####
# Simulate the data. For now, just sticking with continuous numeric data for simplicity. 
create_cov_matrix <- function(n_site,n_covs){
  # Initialize empty matrix to store values
  cov_matrix <- matrix(nrow = n_site, ncol = n_covs)
  for(c in 1:n_covs){
    # Populate matrix with data drawn from a normal distribution (assuming all centered and scaled), one column for each covariate
    cov_matrix[,c] <- rnorm(n_site,0,1)
  }
  return(cov_matrix)
}

# Simulate probability of success
# function arguments, n_site, betas, data
create_prob_success <- function(n_site,betas,data){
  probs <- rep(NA, n_site)
  for (s in 1:n_site){
    # Pull out the row of observations for that site
    data_rep <- data[s,]
    #prob_site <- sum(betas*data_rep)
    # Calculate a linear predictor from the beta values and the data
    linear_pred <- baseline_prob_success + sum(betas * data_rep)
    # Transform this into probability of success
    prob_site <- plogis(linear_pred)
    probs[s] <- prob_site
  }
  return(probs)
}


#### Simulate Data and Run Model #####
# Initialize empty for storing values within the loop
simulation_diffs <- rep(NA, length(sites_range))
simulation_se <- rep(NA, length(sites_range))
simulation_var <- rep(NA, length(sites_range))

for(sim_num in 1:length(sites_range)){
  # Establish the number of sites
  n_site <- sites_range[sim_num]
  
  # Establish number of covariates
  n_covs <- number_covariates
  
  # Call the function to create a matrix of observed data
  data <- create_cov_matrix(n_site,n_covs)
  
  # Simulate true effects of covariates
  baseline_prob_success <- .6
  betas <- rnorm(n_covs, 0, 1) # Should I be drawing from a beta distribution here so they're between 0 and 1???????????????????????????
  
  # Create probability of success 
  probs <- create_prob_success(n_site,betas,data)
  
  # Simulate a binary response variable
  y_obs <- rbinom(n_site, 1, probs)
  
  # Run a GLM #
  data_df <- data.frame(data)
  data_df$y <- y_obs
  
  # Create the formula dynamically
  covariate_names <- paste0("X", 1:n_covs)
  formula <- as.formula(paste("y_obs ~", paste(covariate_names, collapse = " + ")))
  colnames(data_df)[1:n_covs] <- covariate_names
  
  # Call model 
  fit<-glm(formula, data=data_df, family=binomial(link="logit"))
  
  # Initialize empty vectors
  diffs<- rep(NA, length(betas))
  se <- rep(NA, length(betas))
  #params <- rep(NA, length(betas))
  
  for (b in 1:length(betas)){
    # extract intercepts
    model_param <- summary(fit)$coefficients[b+1,1]
    #params[b] <- model_param
    # get the difference between the actual parameters and the observed parameters (absolute value so it's positive)
    diffs[b] <- abs(model_param-betas[b])
    # extract the standard error
    se[b] <- summary(fit)$coefficients[b+1,"Std. Error"]
  }
  # Add and subtract the standard error to get the confidence interval 
  #upper_ci <- params + se
  #lower_ci <- params - se
  
  # Take the average difference between the estimated and the actual parameters
  avg_diff <- mean(diffs)
  simulation_diffs[sim_num] <- avg_diff
  # Take the average standard error in the estimates (as sample size increases this should decrease)
  avg_se <- mean(se)
  simulation_se[sim_num] <- avg_se
  # get the variance of the data
  var_covs <- var(data)
  avg_var <- mean(var_covs)
  simulation_var[sim_num] <- avg_var
}

# Plot these results
plot(sites_range,simulation_diffs, 
     main = "Average Difference Between Estimated and Actual Parameter", 
     ylab = "Absolute Value of Difference", 
     xlab = "Sample Size", 
     ylim = c(0,1.5))
abline(h= 0, col = "red")
mtext(paste("Number of Covariates:", n_covs))
# Plot the standard error and the variance
plot(sites_range,simulation_se, 
     main = "Average Standard Error in Estimates", 
     ylab = "Standard Error", 
     xlab = "Sample Size",
     ylim = c(0,1.5))
points(sites_range,simulation_var,
       col = "red")
mtext(paste("Number of Covariates:", n_covs))

