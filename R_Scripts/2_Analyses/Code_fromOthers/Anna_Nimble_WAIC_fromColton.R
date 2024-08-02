# Nimble WAIC Naive
library(nimble)
full_dat <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_SCALED_7-30.csv")

detections <- full_dat[,2:7]

# Have to specify the z data for a true presence if there was a 1 at any time 
z_dat <- rowSums(detections, na.rm = T)
z_dat[z_dat > 1] <- 1
z_dat[z_dat == 0] <- NA

# get the length of non-nas in the detection data
get_na <- function(x){(max(which(!is.na(x))))}
# Apply this to our data
miss <- apply(detections, 1, get_na)

code <- nimbleCode({
  # Priors
  a0 ~ dlogis(0, 1)
  b0 ~ dlogis(0, 1)
  
  # Likelihood
  # Loop through each sampling unit
  for(i in 1:n_unit){
    # Impute missing habitat data
    
    Z[i] ~ dbern(psi[i])  # Presence or absence drawn from a bernoulli distribution with occupancy probability psi  
    logit(psi[i]) <- a0 
    
    for(j in 1:miss[i]){
      det_data[i,j] ~  dbern(Z[i]*theta[i,j])  # Create detection data drawn from a bernoulli distribution incorporating the true presence (Z) with probability of detection theta
      logit(theta[i,j]) <- b0
      
    }
  }
  
  # Derived parameters
})

# Create a list of data for JAGS
naive_data <- list(
  # True presence at sites with confirmed presences
  Z = z_dat,
  # Detection/nondetection data
  det_data = detections,
  n_unit = nrow(detections),
  miss = miss
)

# Set initial values 
inits_func_naive <- function(){
  list(
    a0 = rnorm(1, 0, 1),
    b0 = rnorm(1, 0, 1)
  )
}

# Run the model
# I normally do this differently but the base way to do it will work for this
mcmcout <- nimbleMCMC(code = code,
                      constants = naive_data,
                      inits = inits_func_naive(),
                      WAIC = T,
                      summary = T,
                      monitors = c(), # Place what you want to monitor in her (aka params)
                      niter = 10000,
                      nburnin = 4000,
                      thin = 1) # Dan always says to set thin equal to 1, change if need be

# Access WAIC
mcmcout$WAIC

# Summary
mcmcout$summary$all.chains

# Actual samples
mcmcout$samples

