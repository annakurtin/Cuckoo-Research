####  JAGS code to calculate WAIC


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
init_func_naive <- function(){
  list(
    a0 = rnorm(1, 0, 1),
    b0 = rnorm(1, 0, 1)
  )
}

cat("
  model{
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
    
  }
  ", file = "C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Occupancy_Submodel/Model_Structure_Files/JAGS_HabOccModNAIVE.txt")


fit_naive <- jags(data = naive_data, 
                  model.file = "C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Occupancy_Submodel/Model_Structure_Files/JAGS_HabOccModNAIVE.txt",
                  parameters.to.save = c("a0", "b0"),
                  n.iter = 10000, 
                  n.burnin = 4000, 
                  n.chains = 3,
                  n.thin = 2,
                  inits = init_func_naive)