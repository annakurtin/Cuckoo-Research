# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# occupancy model simulation
# date created: 7/9/2024
# last modified: 7/9/2024
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## this is a script to simulate data and fit an occupancy model a specified number of times to evaluate model performance
## if the model is consistently way off, it indicates an issue with the model
## if the model is a bit wrong every time, it indicates the model is working alright and is just bouncing around due to noise in the dataset
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This is the script with the starting conditions:
## The number of sites that we have for our study 
## Six parameters in total: psi, p, a1, a2, b1, and b2
## Two covariates on detection, two covariates on occupancy 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(jagsUI)

nSims <- 10
nSites <- 105

# number of covariates for detection and occupancy
nCX <- 5 # Occupancy 
nCW <- 5 # Detection 

# MCMC parameters
ni <- 5000
nt <- 5
nb <- 2500
nc <- 3


# number of covariates to monitor
nCM <- 2

# number of visits to each site
nVis <- 3

# mean occupancy and detection
mu_psi <- 0.6
mu_p <- 0.2

# arrays to store simulated values
tru.alpha <- matrix(NA, nSims, nCX) # alpha for occupancy covariates
tru.beta <- matrix(NA, nSims, nCW) # beta for detection covariates

# arrays to store estimates
## Could make this more flexible? ##
est.mu <- array(NA, dim = c(nSims, 2, 5)) # mean occupancy and detection estimates (median, lower CI, upper CI, n_eff, and f)
est.alpha <- array(NA, dim = c(nSims, nCM, 5)) # estimates for occupancy covariates monitored (median, lower CI, upper CI, n_eff, and f)
est.beta <- array(NA, dim = c(nSims, nCM, 5)) # estimates for detection covariates (median, lower CI, upper CI, n_eff, and f)


sink("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Full_Occupancy_Model/Simulations/m1.jags")
cat("
      model {

      # priors for intercepts
      for (j in 1:2){
        mu[j] ~ dlogis(0,1)
      }
      
      # priors for covariate effects (nCM: number of covariates to monitor)
      for (k in 1:nCM){
        alpha[k] ~ dnorm(0, 0.1)
        beta[k] ~ dnorm(0, 0.1)
      }
      
      
      for (i in 1:nSites){
        logit(psi[i]) = mu[1] + x[i,] %*% alpha[1:nCM]
        logit(p[i]) = mu[2] + w[i,] %*% beta[1:nCM]
        
        z[i] ~ dbern(psi[i])      
        for (j in 1:nVis){
          y[i,j] ~ dbern(z[i] * p[i])
        }
      }
      
    }
    ",fill = TRUE)
sink()



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run the simulation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (ii in 1:nSims){  # use 'ii' here so it doesn't get mixed up with other loops, could use 'koala'
  
  print(ii)
  print(Sys.time())
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # simulate some z-scored occupancy covariates
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- matrix(NA, nSites, nCX)
  w <- matrix(NA, nSites, nCW)
  
  for (j in 1:nCX){
    x[,j] <- rnorm(nSites, 0, 1)
  }
  for (k in 1:nCW){
    w[,k] <- rnorm(nSites, 0, 1)
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # occupancy (alpha) and detection (beta) parameters
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  alpha <- c(qlogis(mu_psi), rnorm(nCX, 0, 1)) 
  #alpha/occupancy covariates, including baseline probability of occupancy converted to log odds with qlogis() and the log odds effect of the occupancy covariates
  beta <- c(qlogis(mu_p), rnorm(nCX, 0, 1))
  #beta/detection covariates, including baseline probability of detection converted to log odds with qlogis() and the log odds effect of the detection covariates
  
  # store 'truth'
  tru.alpha[ii,] <- alpha[2:(nCX+1)] # True effects of just the covariates, not including baseline probability of occupancy/alpha/a0
  tru.beta[ii,] <- beta[2:(nCW+1)] # True effects of just the covariates, not including baseline probability of presence/beta/b0
  
  # Establish a vector to store presence/absence and detection/nondetection data
  z <- rep(NA, nSites)
  y <- matrix(NA, nSites, nVis)
  
  # Use matrix multiplication to store psi for each site
  psi <- plogis(alpha[1] + x %*% alpha[2:(nCX+1)])
  p <- plogis(beta[1] + w %*% beta[2:(nCW+1)])

  # Generate presence/absence data based on psi
  z <- rbinom(nSites, 1, psi)  
  # Generate detection/nondetection data based on p
  for (j in 1:nVis){
    y[,j] <- rbinom(nSites, z, p)
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # known (1) presence and unknown (NA)
  z.dat <- rowSums(y, na.rm = T)
  z.dat[z.dat > 1] <- 1
  z.dat[z.dat == 0] <- NA
  # z.dat
  
  jags.data <- list(y = y, z = z.dat, x = x[,1:nCM], w = w[,1:nCM],
                    nSites = nSites, nVis = nVis, nCM = nCM)
  ## This is only taking the first two columns of x and w, could change this if we want to monitor more covariates
  
  # Initial values
  inits <- function(){list()}  
  
  # Parameters monitored
  parameters <- c('beta','alpha','mu')
  
  # Call the model on the data

  Sys.time()
  m1 <- jags(data = jags.data, 
             inits = inits, 
             parameters.to.save = parameters, 
             model.file = "C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Full_Occupancy_Model/Simulations/m1.jags",
             n.chains = nc,
             n.thin = nt,
             n.iter = ni,
             n.burnin = nb,
             parallel = T)
  #print(m1)
  Sys.time()

  # take the median, 95% confidence interval, effective sample size, and f statistic for this simulation
  est.mu[ii,1,] <- c(m1$q50$mu[1],m1$q2.5$mu[1],m1$q97.5$mu[1],m1$n.eff$mu[1],m1$f$mu[1])
  est.mu[ii,2,] <- c(m1$q50$mu[2],m1$q2.5$mu[2],m1$q97.5$mu[2],m1$n.eff$mu[2],m1$f$mu[2])  
  
  # take median, 95% CI, neff, and f for the alpha parameter and assign it to the iith row
  est.alpha[ii,,1] <- m1$q50$alpha
  est.alpha[ii,,2] <- m1$q2.5$alpha
  est.alpha[ii,,3] <- m1$q97.5$alpha
  est.alpha[ii,,4] <- m1$n.eff$alpha
  est.alpha[ii,,5] <- m1$f$alpha

  # take median, 95% CI, neff, and f for the beta parameter and assign it to the iith row
  est.beta[ii,,1] <- m1$q50$beta
  est.beta[ii,,2] <- m1$q2.5$beta
  est.beta[ii,,3] <- m1$q97.5$beta
  est.beta[ii,,4] <- m1$n.eff$beta
  est.beta[ii,,5] <- m1$f$beta
  
}

# plot the estimated value of alpha_1 against the tru value of alpha_1 across 20 simulations
plot(est.alpha[,1,1] ~ tru.alpha[,1]); abline(0,1, lty = 2)
mean(est.alpha[,1,1] - tru.alpha[,1]) 
title(main = "Residuals for alpha 1, 6 params")

vioplot::vioplot(plogis(est.mu[,1,1]), plogis(est.mu[,2,1]), drawRect = F)
points(x = 1, y = mu_p, col = "red", pch = 19)
points(x = 2, y = mu_psi, col = "red", pch = 19)
title(main = "Latent State Estimations, 6 params")


# Add on a plot - look at the differences between the estimate and the true mean across each simulation
# Create dataframe to store data in
diffs <- data.frame(matrix(nrow = 10, ncol = 5))
# Name the columns for the difference you took between actual and estimated
colnames(diffs) <- c("sim_num","a1","a2","b1","b2")
# Populate this data frame by calculating the values from each 
for(sim in 1:nSims){
  diffs[sim,"sim_num"] <- sim
  # Fill in the difference in median for alpha covariate 1 and 2
  diffs[sim,c("a1","a2")] <- c((est.alpha[sim,1,1]-tru.alpha[sim,1]), (est.alpha[sim,2,1]-tru.alpha[sim,2])) 
  # Fill in the difference in median for the beta covariate 1 and 2
  diffs[sim,c("b1","b2")] <- c((est.beta[sim,1,1]-tru.beta[sim,1]),(est.beta[sim,2,1]-tru.beta[sim,2]))
}

# pivot this dataset longer (or maybe not?)
diffs_long <- diffs %>% group_by(sim_num) %>% pivot_longer(cols = c(2:5), names_to = "param",values_to = "estimate")
ggplot() +
  geom_point(data = diffs_long, aes(x=sim_num, y=estimate, col = param), size = 5) +
  geom_hline(yintercept=0) +
  labs(title = "Difference in log odds estimate per simulation, 6 params") +
  theme_minimal()


# Put diffs in plogis format
diffsp <- data.frame(matrix(nrow = 10, ncol = 5))
# Name the columns for the difference you took between actual and estimated
colnames(diffsp) <- c("sim_num","a1","a2","b1","b2")
# Populate this data frame by calculating the values from each 
for(sim in 1:nSims){
  diffsp[sim,"sim_num"] <- sim
  # Fill in the difference in median for alpha covariate 1 and 2
  diffsp[sim,c("a1","a2")] <- c(plogis((est.alpha[sim,1,1])-plogis(tru.alpha[sim,1])), plogis((est.alpha[sim,2,1])-plogis(tru.alpha[sim,2]))) 
  # Fill in the difference in median for the beta covariate 1 and 2
  diffsp[sim,c("b1","b2")] <- c(plogis((est.beta[sim,1,1])-plogis(tru.beta[sim,1])),plogis((est.beta[sim,2,1])-plogis(tru.beta[sim,2])))
}
diffsp_long <- diffsp %>% group_by(sim_num) %>% pivot_longer(cols = c(2:5), names_to = "param",values_to = "estimate")
ggplot() +
  geom_point(data = diffsp_long, aes(x=sim_num, y=estimate, col = param), size = 5) +
  geom_hline(yintercept=0) +
  labs(title = "Difference in probability estimate per simulation, 6 parameters") +
  theme_minimal()
# Why are these all positive????????????

# take the difference in the estimate of the median and take the mean
mean(est.alpha[,1,1] - tru.alpha[,1])
# should be ~0 across large numbers of simulations if model works (0 doesn't mean it does, but <= 0 indicates issues)

# Print out the data for how many presences/absences
table(y)
table(z)