### Bug Report for JAGS Linear Model ####

library(jagsUI)
library(rjags)

n_site <- 20

# simulated observed response data - number of days at a site with calling
days <- round(runif(n_site, 0,30),0)
# Scaled covariate for explanatory variable
cov <- round(rnorm(n_site, 0,1), 2)


# Create a list of data for JAGS
jags_data <- list(
  days = days,
  cov1 = cov,
  n_unit = n_site
)

hist(jags_data$days)
hist(jags_data$cov1)

# Set initial values 
init_func <- function(){
  list(
    a0 = rnorm(1, 0, 1),
    a1 = rnorm(1, 0, 1)
  )
}

# Create model
cat("
  model{
        # Priors
    a0 ~ dnorm(0, 1)
    a1 ~ dnorm(0, 1)
    sigma ~ dunif(0,10)
    tau <- 1/ (sigma*sigma)
  
    # Likelihood
    # Loop through each sampling unit
    for(i in 1:n_unit){
      # Impute missing habitat data
      cov1[i] ~ dnorm(0, 1)
  
      days[i] ~ dpois(lambda[i])  # Presence or absence drawn from a bernoulli distribution with occupancy probability psi  
      log(lambda[i]) <- a0 + a1*cov1[i] + eps[i]
      eps[i] ~ dnorm(0, tau)
    }
    
    # Derived parameters
    
  }
  ", file = "./LM1_Replicate.txt")

# Fit the model
fit_lm1 <- jags(data = jags_data, 
                model.file = "./LM1_Replicate.txt",
                parameters.to.save = c("a0", "a1"),
                n.iter = 10000, 
                n.burnin = 4000, 
                n.chains = 3,
                n.thin = 2,
                inits = init_func)

print(fit_lm1)
tracedens_jags(fit_lm1)
