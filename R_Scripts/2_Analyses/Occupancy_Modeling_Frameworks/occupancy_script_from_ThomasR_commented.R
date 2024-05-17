######### Occupancy Script from Thomas Reicke ####################

# This was a script he created for me to show how to deal with NAs in JAGS 
# dealing with NAs in this way is unique to Bayesian modeling 
# Also has useful code for using in a power analysis

# Created 5/17/2023

# Last modified 5/17/2023

#############################################################

# sampler settings
nc <- 4 # number of chains
nt <- 1 # number of thinning
ni <- 5000 # number of iterations
nb <- 1000 # number burnin

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0) generate underlying data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nP <- 100  # number of points
nV <- 3    # number of repeated visits
nC <- 2    # number of covariates

# create latent state (z) and detection (y) data storage
z <- NULL        # point-level latent state
y <- array(NA, dim = c(nP, nV))   # point-level observation data

# create covariates
# initialize empty array for x (covariates)
x <- array(NA, dim = c(nP,nC))
# Fill the array with randomly generated data
for (j in 1:(nC)){
  x[,j] <- rnorm(nP, 0, 1)
}


# covariate effects (fixed to be near zero so we can model without covariates for now)
beta <- rnorm(nC, 0, 2)

psi0 <- qlogis(0.2) # probability of presence
p0 <- qlogis(0.8) # probability of detection

# Calculate probability of presence from the beta values
psi <- NULL
for (i in 1:nP){
  psi[i] = plogis(psi0 + beta[1]* x[i,1] + beta[2] * x[i,2])
}
# plot how probability of occupancy relates to the two covariates to see what the relationship is
plot(psi ~ x[,1])
plot(psi ~ x[,2])

# get your true presence/non presence values from psi
z <- rbinom(nP, 1, psi)

# total number of detections across number of visits (nV)
y <- matrix(NA, nP, nV)
# fill in the detection matrix dependent on whether the animal was there (z) and whether it was detected (p0)
for (i in 1:nP){
  for (j in 1:nV){
    y[i,j] <- rbinom(1, z[i], plogis(p0))
  }
}

# Create some holes in the samples to simulate NA data
# select randomly which data will be NA
mess.y <- sample(1:nP, 20)
mess.x <- sample(1:nP, 5)
# Change this data to NA
y[mess.y, 3] <- NA
x[mess.x, ] <- c(NA,NA)

# you can also deal with NAs by only iterating through the values of the data that have values and not NAs
# pull out how many values without NAs each has
get.na <- function(x){(max(which(!is.na(x))))}
# Apply this to our data
miss <- apply(y, 1, get.na)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1a) JAGS model no augmentation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sink("m1a.jags")
cat("
      model {

      for (j in 1:(nC+1)){
        beta[j] ~ dlogis(0,1)
      }
      p ~ dbeta(1,1)
      for (i in 1:nP){
    
      # Method 1: specify a distribution to draw from when there are NAs in the data 
        x[i,1] ~ dnorm(0,1)
        x[i,2] ~ dnorm(0,1)
      
        logit(psi[i]) = beta[1] + beta[2:(nC+1)] %*% x[i,]
            z[i] ~ dbern(psi[i])      
      
          for (j in 1:miss[i]){ # Method 2:iterate through only the values that don't have NAs
            y[i,j] ~ dbern(z[i] * p)
          }
          
        }
      }
      
      ",fill = TRUE)
sink()

# You also need to give the model the z data, which gives a 1 if there was the species detected there at all and an NA if there isnt' a presence across any of the survey periods
# known (1) presence and unknown (NA)
z.dat <- rowSums(y, na.rm = T)
z.dat[z.dat > 1] <- 1
z.dat[z.dat == 0] <- NA
z.dat
jags.data <- list(y = y, nP = nP, nV = nV, z = z.dat, x = x, miss = miss, nC = nC)

# Initial values
inits <- function(){list()}  

# Parameters monitored
parameters <- c('beta','p','x')

library(jagsUI)
Sys.time()
m1a <- jags(jags.data, inits, parameters, "m1a.jags", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
            parallel = T)
Sys.time()
print(m1a)











