
n <- 100
x <- rnorm(n, 0, 1)
psi <- plogis(0 + 1 * x)
plot(psi ~ x)
z <- rbinom(n, 1, psi)
plot(z ~ x)


s <- 30
y <- matrix(0, n, s)
p <- 0.1
for (j in 1:s){
  y[,j] <- rbinom(n, z, p)
}

rowSums(y)

hist(rgamma(100000, 0.5, 0.5), breaks = 100)
hist(rgamma(100000, 50, 50), breaks =100)
sink("detections.jags")
cat("

    model {

    for (i in 1:n){
      # binomial approach
      # logit(mu[i]) = beta0 + beta1 * x[i]
      # y[i] ~ dbin(mu[i], s)
      
      # gamma-poisson (this is equivalent to a negative binomial)
      # log(gamma[i]) = beta0 + beta1 * x[i]
      # h[i] ~ dgamma(theta, theta)
      # y[i] ~ dpois(gamma[i] * h[i])
      
      
      
      # zero-inflated gamma-poisson (this is equivalent to a ZI negative binomial)
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
