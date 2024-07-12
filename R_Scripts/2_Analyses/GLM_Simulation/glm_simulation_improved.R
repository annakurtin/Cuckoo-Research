##### Code to simulate a GLM with varying number of sites ########
# I sent LogReg_Simultion_MultipleSampSize to Thomas and he sent back an updated script glm_simulation. I've edited it to be able to handle multiple covariates here.

#### Create functions ########
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
create_prob_success <- function(n_site,betas,data, baseline_prob){
  probs <- rep(NA, n_site)
  for (s in 1:n_site){
    # Pull out the row of observations for that site
    data_rep <- data[s,]
    # Calculate a linear predictor from the beta values and the data
    linear_pred <- baseline_prob + sum(betas * data_rep)
    # Transform this into probability of success
    prob_site <- plogis(linear_pred)
    probs[s] <- prob_site
  }
  return(probs)
}

#### Establish Simulation Parameters ########
# gradient of sample sizes
sites_range <- seq(20,500, by = 20)
res <- length(sites_range)
# number of simulations at each sample size
nSims <- 100
# number of covariates
nCov <- 8

#### Run Simulation ####
est <- array(NA, dim = c(nSims, res, nCov))
se <- array(NA, dim = c(nSims, res, nCov))
tru <- array(NA, dim = c(nSims, res, nCov))
bias <- array(NA, dim = c(nSims, res, nCov))


for (ii in 1:nSims){
  # ii is the current simulation that we're on
  for (jj in 1:res){
    #sites_range[jj] is the current number of sites being simulated
    beta <- rnorm(nCov, 0, 1)
    x <- create_cov_matrix(sites_range[jj],nCov)
    # probability of whatever (theta)
    baseline_prob <- 0
    theta <- create_prob_success(sites_range[jj],beta,x, baseline_prob)
    y <- rbinom(sites_range[jj], 1, theta)
    
    # Create the formula dynamically
    covariate_names <- paste0("V", 1:nCov)
    df <- as.data.frame(x)
    cbind(df,y)
    formula <- as.formula(paste("y ~", paste(covariate_names, collapse = " + ")))
    
    # Call model 
    fit<-glm(formula, data = df, family=binomial(link="logit"))
    
    for (c in 1:nCov){
      est[ii,jj,c] <- summary(fit)$coefficients[c+1,1]
      se[ii,jj,c] <- summary(fit)$coefficients[c+1,2]
      tru[ii,jj,c] <- beta[c]
      bias[ii,jj,c] <- est[ii,jj,c] - beta[c]
    }
  }
}


#### Visualize Results ########
# Define the number of rows and columns for the plot layout
nrows <- ceiling(sqrt(nCov))
ncols <- ceiling(nCov / nrows)
par(mfrow = c(nrows, ncols), mar = c(5, 5, 4, 2))

# Plot Bias
for (c in 1:nCov){
  boxplot(bias[,,c], names = sites_range,
          ylab = expression(hat(beta[c])~'-'~beta[c]), xlab = 'Simulated sites',
          cex.lab = 1.5, 
          ylim = c(-2,2))
  title(main = paste("Bias for Covariate", c))
}
# Plot Standard Error
par(mfrow = c(nrows, ncols), mar = c(5, 5, 4, 2))
for (c in 1:nCov){
  boxplot(se[,,c], names = sites_range, 
          ylab = 'Standard error (se)', 
          xlab = 'Simulated sites',
          ylim = c(0,4))
  title(main = paste("St. Error for Covariate", c))
}