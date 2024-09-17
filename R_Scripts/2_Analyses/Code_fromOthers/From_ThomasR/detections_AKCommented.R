# This is code from my conversation with Thomas R 9/13
## Additional context in OneNote

# Talking about how to parameterize the linear model for the second part of my habitat chapter
## Simulate data for the binomial parameterization of the model
# number of sites
n <- 100
## Simulate a random covariate
x <- rnorm(n, 0, 1)
# Calculate probability of use at a site based on our covariate 
psi <- plogis(0 + 1 * x)
plot(psi ~ x)
# latent state for whether or not the site was used
z <- rbinom(n, 1, psi)
plot(z ~ x)

# Numbe of surveys (/number of days)
s <- 30
# number of times that the birds were seen on surveys (response variable)
y <- matrix(0, n, s)
# probability of detection 
p <- 0.1
for (j in 1:s){
  y[,j] <- rbinom(n, z, p)
}
# Total number of times the birds were seen 
rowSums(y)

# Showing the effect of the dispersion parameter in a gamma distribution
## Low dispersion
hist(rgamma(100000, 0.5, 0.5), breaks = 100)
## High dispersion 
hist(rgamma(100000, 50, 50), breaks =100)

## Creating a jags model for different ways of parameterizing it 
## Basically we need a new way to parameterize the model so that 1/100 days with calling doesn't look the same as 1/10 days with calling (much lower quality of data and less information in this data)
sink("detections.jags")
cat("

    model {

    for (i in 1:n){
      # Method 1: binomial approach
      # In this approach, you are essentially just taking the detection submodel of an occupancy model but probability of detection (parameter you're interested in) is instad interpreted as proportion of days with calls
      # Can use both just sites with BBCU and sites with zeros but you might have some issues with model fit
      # logit(mu[i]) = beta0 + beta1 * x[i]
      # y[i] ~ dbin(mu[i], s)
      
      
      # Method 2: gamma-poisson (this is equivalent to a negative binomial)
      # In this approach, the parameter of interest is the count of days with calls but we're also feeding it the number of days that we recorded 
      # log(gamma[i]) = beta0 + beta1 * x[i]
      # h[i] ~ dgamma(theta, theta)
      # y[i] ~ dpois(gamma[i] * h[i])
      
      
      
      # Method 3: zero-inflated gamma-poisson (this is equivalent to a ZI negative binomial)
      # Here we're adding a level of the model where we accoutn for whether the cuckoo was present at the site or not
      # a1 affects presence at a site, b1 affects strength of use at a site. I would be testing the same covariates in each side of the model here which Thomas said would be fine but the effects of each on each response variable might get a bit convoluted and hard to interpret
      logit(mu[i]) = a0 + a1 * x[i]
      log(gamma[i]) = beta0 + beta1 * x[i]
      z[i] ~ dbern(mu[i])
      h[i] ~ dgamma(theta, theta)
      y[i] ~ dpois(gamma[i] * h[i] * z[i])
      
    }

    theta ~ dgamma(1,1)
    beta0 ~ dnorm(0, 0.1)
    beta1 ~ dnorm(0, 0.1)
    a0 ~ dnorm(0, 0.1)
    a1 ~ dnorm(0, 0.1)

    }
    ",fill = TRUE)
sink()


#############################################################################################
# Bundle data
#############################################################################################

# we've now added x (latitude of each population) as data
z.dat <- rowSums(y)
z.dat[z.dat == 0] <- NA
z.dat[z.dat > 1] <- 1
jags.data <- list(y = rowSums(y), s = s, n = n, x = x, z = z.dat)


inits <- function(){list()}  

# Parameters monitored
parameters <- c('theta','beta0','beta1','a0','a1')

nc <- 4
nt <- 25
ni <- 50000
nb <- 25000


# Call JAGS from R 
# 7s for 50k iterations with an Intel i9-10900 10-core processor with some other computing going on as well...
library(jagsUI)
Sys.time()
m <- jags(jags.data, inits, parameters, "detections.jags", parallel = T, 
           n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)

Sys.time()
print(m, digits = 3)

plot(rowSums(y) ~ x)
px <- seq(-3,3,length.out = 100)
py <- exp(m$q50$beta0 + m$q50$beta1 * px)
points(py ~ px, type = 'l')
