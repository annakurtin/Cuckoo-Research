# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# simulate data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(R2jags)

# n: total number of sites
n <- 200

# h: number of discrete habitat types (of course this is silly, but...)
h <- 3

# habitat type
x <- sample(1:h, n, replace = T)
table(x)

# psi: occupancy probability
psi <- rbeta(h, 3, 3)
psi

# z: latent state
z <- rbinom(n, 1, psi[x])
table(x, z)


# p: detection
# J: number of secondary occasions
# y: 1/0 data
p <- 0.5
J <- 3
y <- matrix(0, n, J)

# fill data
for (j in 1:J){
  y[,j] <- rbinom(n, z, p)
}

# makes sense
table(z, rowSums(y))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# JAGS model, single intercept (i.e., psi(.), p(.))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sink("m0.jags")
cat("
      model {


      psi ~ dbeta(5,5)
      p ~ dbeta(5,5)
    
      for (i in 1:n){
        z[i] ~ dbern(psi)
        
        for (j in 1:J){
          y[i,j] ~ dbern(z[i] * p)
        }
        
      }
      
      }
      
      ",fill = TRUE)
sink()

## Why is beta used here? I thought that beta distributions could only take on values between 0 and 1? What do the moments used here mean?????????????????????????????????

# known (1) presence and unknown (NA)
z.dat <- rowSums(y, na.rm = T)
z.dat[z.dat > 1] <- 1
z.dat[z.dat == 0] <- NA
z.dat
jags.data <- list(y = y, z = z.dat, x = x, n = n, J = J)

# Initial values
inits <- function(){list()}  

# Parameters monitored
parameters <- c('p','psi')

ni <- 1000
nb <- 500
nc <- 3
nt <- 1


Sys.time()
m0 <- jags(jags.data, inits, parameters, "m0.jags", 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
Sys.time()



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# JAGS model, habitat-type specific intercept (i.e., psi(x), p(.))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sink("m1.jags")
cat("
      model {
    
      # Priors
      for (k in 1:h){
        psi[k] ~ dbeta(5,5) # Draw a random value for psi for each habitat type
      }
      p ~ dbeta(5,5)
      
      for (i in 1:n){
        z[i] ~ dbern(psi[x[i]])
        
        for (j in 1:J){
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
jags.data <- list(y = y, z = z.dat, x = x, n = n, J = J, h = h)

# Initial values
inits <- function(){list()}  

# Parameters monitored
parameters <- c('p','psi')

ni <- 1000
nb <- 500
nc <- 3
nt <- 1


Sys.time()
# m1 <- R2jags::jags(jags.data, inits, parameters, "m1.jags", 
#                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
m1 <- jagsUI::jags(jags.data, inits, parameters, "m1.jags", 
           n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
Sys.time()



str(m0)
str(m1)

m0$model
m1$model

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sample WAIC
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
samples.m0 <- jags.samples(m0$model, 
                           c("WAIC","deviance"), 
                           type = "mean", 
                           n.iter = 5000,
                           n.burnin = 1000,
                           n.thin = 1)
samples.m0$p_waic <- samples.m0$WAIC
samples.m0$waic <- samples.m0$deviance + samples.m0$p_waic
tmp <- sapply(samples.m0, sum)
waic.m0 <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)

# compute wAIC for model with covariate
samples.m1 <- jags.samples(m1$model, 
                           c("WAIC","deviance"), 
                           type = "mean", 
                           n.iter = 5000,
                           n.burnin = 1000,
                           n.thin = 1)
samples.m1$p_waic <- samples.m1$WAIC
samples.m1$waic <- samples.m1$deviance + samples.m1$p_waic
tmp <- sapply(samples.m1, sum)
waic.m1 <- round(c(waic = tmp[["waic"]], p_waic = tmp[["p_waic"]]),1)

waic.m0
waic.m1





