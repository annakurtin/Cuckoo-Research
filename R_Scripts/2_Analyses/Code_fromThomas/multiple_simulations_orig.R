# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# occupancy model simulation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nSims <- 10
nSites <- 100

# number of covariates for detection and occupancy
nCX <- 5
nCW <- 5


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
mu_p <- 0.3

# arrays to store simulated values
tru.alpha <- matrix(NA, nSims, nCX)
tru.beta <- matrix(NA, nSims, nCW)

# arrays to store estimates
est.mu <- array(NA, dim = c(nSims, 2, 5))
est.alpha <- array(NA, dim = c(nSims, nCM, 5))
est.beta <- array(NA, dim = c(nSims, nCM, 5))


sink("m1.jags")
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
  beta <- c(qlogis(mu_p), rnorm(nCX, 0, 1))
  
  # store 'truth'
  tru.alpha[ii,] <- alpha[2:(nCX+1)]
  tru.beta[ii,] <- beta[2:(nCW+1)]
  
  z <- rep(NA, nSites)
  y <- matrix(NA, nSites, nVis)
  
  psi <- plogis(alpha[1] + x %*% alpha[2:(nCX+1)])
  p <- plogis(beta[1] + w %*% beta[2:(nCW+1)])

  z <- rbinom(nSites, 1, psi)  
  
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
  
  # Initial values
  inits <- function(){list()}  
  
  # Parameters monitored
  parameters <- c('beta','alpha','mu')
  
  library(jagsUI)
  Sys.time()
  m1 <- jags(jags.data, inits, parameters, "m1.jags", 
              n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
              parallel = T)
  Sys.time()

  
  est.mu[ii,1,] <- c(m1$q50$mu[1],m1$q2.5$mu[1],m1$q97.5$mu[1],m1$n.eff$mu[1],m1$f$mu[1])
  est.mu[ii,2,] <- c(m1$q50$mu[2],m1$q2.5$mu[2],m1$q97.5$mu[2],m1$n.eff$mu[2],m1$f$mu[2])  
  
  est.alpha[ii,,1] <- m1$q50$alpha
  est.alpha[ii,,2] <- m1$q2.5$alpha
  est.alpha[ii,,3] <- m1$q97.5$alpha
  est.alpha[ii,,4] <- m1$n.eff$alpha
  est.alpha[ii,,5] <- m1$f$alpha
  
  est.beta[ii,,1] <- m1$q50$beta
  est.beta[ii,,2] <- m1$q2.5$beta
  est.beta[ii,,3] <- m1$q97.5$beta
  est.beta[ii,,4] <- m1$n.eff$beta
  est.beta[ii,,5] <- m1$f$beta
  
  
}

# plot the estimated value of alpha_1 against the tru value of alpha_1 across 20 simulations
plot(est.alpha[,1,1] ~ tru.alpha[,1]); abline(0,1, lty = 2)
mean(est.alpha[,1,1] - tru.alpha[,1]) # should be ~0 across large numbers of simulations if model works (0 doesn't mean it does, but <= 0 indicates issues)

vioplot::vioplot(plogis(est.mu[,1,1]), plogis(est.mu[,2,1]), drawRect = F)

table(y)
table(z)
