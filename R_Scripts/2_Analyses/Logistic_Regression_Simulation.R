#### GLM Script ####

# Date created: 7/9/2024
# Last modified: 7/9/2024
#### ISSUE WITH IT WAS that I wasn't giving a baseline probability of success, I was just adding on covariates which made it really hard to estimate. Check the email from Thomas R about this tomorrow 

# Question: how many data points do we need to estimate relationships of a binary response variable with eight covariates?
# This is a script to run a glm on different amounts of data and parameters
## First, simulate some basic data and run a logistic regression on it
## Then make this code so that you can run it as a function/loop simulations through it
## Run it on different amounts of data and different numbers of parameters 
## This would be an example of data collected on something like the probability of nest success (1/0) in an area as a response of different environmental covariates

#### Simulate Data #####
sites_range <- seq(100,600,by=50)
for(site_num in sites_range){
  n_site <- site_num
  # put the rest of your code here
}

# Establish number of sites
n_site <- 6
# Establish number of covariates
n_covs <- 2

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
# Call the function to create a matrix of observed data
data <- create_cov_matrix(n_site,n_covs)

# Simulate true effects of covariates
baseline_prob_success <- .6
betas <- rnorm(n_covs, 0, 1) # Should I be drawing from a beta distribution here???????????????????????????????????????????
# Simulate a baseline probability of occupancy 

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
# Create probability of presence 
probs <- create_prob_success(n_site,betas,data)

# Simulate a binary response variable
y_obs <- rbinom(n_site, 1, probs)


#### Run a GLM ####
data_df <- data.frame(data)
data_df$y <- y_obs

# Create the formula dynamically
covariate_names <- paste0("X", 1:n_covs)
formula <- as.formula(paste("y_obs ~", paste(covariate_names, collapse = " + ")))
colnames(data_df)[1:n_covs] <- covariate_names
fit<-glm(formula, data=data_df, family=binomial(link="logit"))
#plogis(10.68)

diffs <- rep(NA,length(betas))
b <- 1
# for (b in 1:length(betas)){
#   model_param <- fit$coefficients[b+1]
#   diff[b] <- abs(fit$coefficients[b+1]-betas[b])
# }



# Start simple: just run a glm on the data as a regular datafrmae
data2<- data.frame(data)
data2$y <- y_obs
fit<-glm(y ~ X1 + X2, data=data2, family=binomial(link="logit"))
# These might be super high or super low due to the small amounts of data

###### ARCHIVE #####
#probs[s] <- betas[1]*data[s,1] + betas[2]*data[s,2]
#vector <- betas*data_rep
#prob_site <- sum(vector)
#print(paste0("Probability is",prob_site))
# prob_site <- 1/(1+exp(-linear_predictor))
# print(prob_site)
# # Simulate data/independent variables (assume centered and scaled)
# # Covariate 1: tree richness
# cov1 <- rnorm(n_site,0,1)
# # Covariate 2: canopy height
# cov2 <- rnorm(n_site, 0, 1)
# # Covariate 3: canopy cover
# cov3 <- rnorm(n_site, 0, 1)
# # Covariate 4: subcanopy height
# cov4 <- rnorm(n_site, 0, 1)
# # Covariate 5: subcanopy density
# cov5 <- rnorm(n_site, 0, 1)
# # Covariate 6: subcanopy complexity
# cov6 <- rnorm(n_site, 0, 1)
# # Covariate 7: veg density
# cov7 <- rnorm(n_site, 0, 1)
# # Covariate 8: shrub community (leaving this out for simplicity)
# Making this a function: using a for loop for i in n_covs, simulate rnorm() and assign it to a different matrix within a covariates matrix
# create_cov_matrix <- function(n_site,n_covs){
#   cov_matrix <- matrix(nrow = n_site, ncol = n_covs)
#   for(c in 1:n_covs){
#     cov_matrix[,c] <- rnorm(n_site,0,1)
#   }
#   return(cov_matrix)
# }
# Test: create_cov_matrix(120,5) works well 
