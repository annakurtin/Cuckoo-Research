# Code to simulate a GLM with varying number of sites
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
    #prob_site <- sum(betas*data_rep)
    # Calculate a linear predictor from the beta values and the data
    linear_pred <- baseline_prob + sum(betas * data_rep)
    # Transform this into probability of success
    prob_site <- plogis(linear_pred)
    probs[s] <- prob_site
  }
  return(probs)
}

# number of simulations at each sample size
nSims <- 100

# gradient of samples sizes
res <- 50
sites_range <- seq(20,1000, length.out = res)

# number of covariates (note changing this to > 1 will require in pulling in
# some of your code from your other script)
nCov <- 2


est <- array(NA, dim = c(nSims, res, nCov))
se <- array(NA, dim = c(nSims, res, nCov))
#tru <- matrix(NA, nSims, res)
tru <- array(NA, dim = c(nSims, res, nCov))
bias <- matrix(NA, dim = c(nSims, res, nCov))

#jj <- 1

for (ii in 1:nSims){
  for (jj in 1:res){
    beta <- rnorm(nCov, 0, 1)
    #x <- rnorm(sites_range[jj], 0, 1)
    x <- create_cov_matrix(sites_range[jj],nCov)
    # probability of whatever (theta)
    #theta <- plogis(0 + beta * x)
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
      est[ii,jj,nCov] <- summary(fit)$coefficients[nCov+1,1]
      se[ii,jj,nCov] <- summary(fit)$coefficients[nCov+1,2]
      #tru[ii,jj,nCov] <- beta
      bias[ii,jj,nCov] <- est[ii,jj,nCov] - beta
    }
    #est[ii,jj,1] <- summary(glm(y ~ x, family = 'binomial'))$coefficients[2,1]
    #se[ii,jj,1] <- summary(glm(y ~ x, family = 'binomial'))$coefficients[2,2]
    #tru[ii,jj] <- beta
    #bias[ii,jj] <- est[ii,jj,1] - beta
  }
}




# Plot the variation in bias and standard errors
par(mar = c(5,5,2,2))
boxplot(bias, names = sites_range,
        ylab = expression(hat(beta)~'-'~beta), xlab = 'Simulated sites',
        cex.lab = 1.5, 
        ylim = c(-2,2))
boxplot(se[,,1], names = sites_range, 
        ylab = 'Standard error (se)', 
        xlab = 'Simulated sites',
        ylim = c(0,4))
