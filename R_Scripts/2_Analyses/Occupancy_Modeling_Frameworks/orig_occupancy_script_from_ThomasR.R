
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One final (?) simulation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 0) generate underlying data
#
# 1) models with no augmentation
#     1a) JAGS model no augmentation
#     1b) Stan model no augmentation
#     1c) model comparison
#
# 2) incorporate data augmentation
#     2a) JAGS model with augmentation
#     2b) Stan model with augmentation
#     2c) model comparison with augmentation
#
# 3) incorporate covariates
#     3a) JAGS model with augmentation and covariates
#     3b) Stan model with augmentation and covariates
#     3c) model comparison with augmentation and covariates
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# sampler settings
nc <- 4
nt <- 1
ni <- 5000
nb <- 1000

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
x <- array(NA, dim = c(nP,nC))
for (j in 1:(nC)){
  x[,j] <- rnorm(nP, 0, 1)
}


# covariate effects (fixed to be near zero so we can model without covariates for now)
beta <- rnorm(nC, 0, 2)

psi0 <- qlogis(0.2)
p0 <- qlogis(0.8)

psi <- NULL
for (i in 1:nP){
  psi[i] = plogis(psi0 + beta[1]* x[i,1] + beta[2] * x[i,2])
}

plot(psi ~ x[,1])
plot(psi ~ x[,2])

z <- rbinom(nP, 1, psi)

# total number of detections across number of visits (nV)
y <- matrix(NA, nP, nV)

for (i in 1:nP){
  for (j in 1:nV){
    y[i,j] <- rbinom(1, z[i], plogis(p0))
  }
}

mess.y <- sample(1:nP, 20)
mess.x <- sample(1:nP, 5)

y[mess.y, 3] <- NA
x[mess.x, ] <- c(NA,NA)

get.na <- function(x){(max(which(!is.na(x))))}
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
      
        x[i,1] ~ dnorm(0,1)
        x[i,2] ~ dnorm(0,1)
      
        logit(psi[i]) = beta[1] + beta[2:(nC+1)] %*% x[i,]
            z[i] ~ dbern(psi[i])      
      
          for (j in 1:miss[i]){
            y[i,j] ~ dbern(z[i] * p)
          }
          
        }
      }
      
      ",fill = TRUE)
sink()

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











