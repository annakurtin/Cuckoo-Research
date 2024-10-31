# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load packages
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(latex2exp)
library(jagsUI)
library(MuMIn)
library(loo)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sample size (n)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
n <- 100
par(mfrow = c(1,1), mar = c(5.1,5.1,2.1,2.1))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# covariate
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x <- rnorm(n, 0, 1)
hist(x, main = NULL, xlab = "x")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# simulate some data (y)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
y <- rnorm(n, x, 1)
plot(y ~ x, pch = 21, bg = 'dodgerblue', las = 1, cex.lab = 2)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# JAGS model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sink("./R_Scripts/2_Analyses/Code_fromOthers/From_ThomasR/lm.jags")
cat("
      model {

      alpha ~ dnorm(0, 1)
      beta ~ dnorm(0, 0.1)
      sigma ~ dgamma(1,1)
      tau = 1/(sigma * sigma)
      
      for (i in 1:n){
      
        # model
        y[i] ~ dnorm(alpha + beta * x[i], tau)
        
        # calculate log-likelihood
        logL[i] <- log(dnorm(y[i], beta * x[i], tau))
        
      }

      
      }
      ",fill = TRUE)
sink()

jags.data <- list(y = y, x = x, n = n)
inits <- function(){list()}  
parameters <- c('sigma','beta','alpha','logL')

nc <- 4
nt <- 10
ni <- 25000
nb <- 10000

library(jagsUI)
Sys.time()
m <- jags(jags.data, inits, parameters, "./R_Scripts/2_Analyses/Code_fromOthers/From_ThomasR/lm.jags", 
          n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
          parallel = T)
Sys.time()

###
### predict credible intervals
###
res <- 100
px <- seq(-3,3, length.out = res)
n.iter <- (ni - nb) * nc/nt
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# not incorporating intercept uncertainty
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mu <- m$q50$alpha + m$q50$beta * px # average
uc <- m$q50$alpha + m$q97.5$beta * px # upper credible interval
lc <- m$q50$alpha + m$q2.5$beta * px # lower credible interval

plot(mu ~ px, lty = 1, lwd = 4, type = 'l',
     ylab = 'E(y)', xlab = 'x', cex.lab = 2)
lines(uc ~ px, lty = 2) # note that they cross the median prediction?
lines(lc ~ px, lty = 2) # same
points(y ~ x)
# Since we have the z standardized covariates, the upper credible interval will go through 0 and then the slope will increase more quickly
# When the covariate is negative, the estimated effect of beta will be negative as well, whereas the lower credible interval will do the opposite

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# incorporating intercept uncertainty
# here we save 95%, 80% CIs and median
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Ey <- matrix(NA, n.iter, res) # matrix for the subset of samples we took (100)
qy <- matrix(NA, 5, res) # matrix for quantiles
for (j in 1:res){
  Ey[,j] <- m$sims.list$alpha + m$sims.list$beta * px[j] # multiple this by the simulated data
  qy[,j] <- quantile(Ey[,j], c(0.025, 0.1, 0.5, 0.9, 0.975))
}

plot(qy[3,] ~ px, lty = 1, lwd = 4, type = 'l',# plot median
     ylab = 'E(y)', xlab = 'x', cex.lab = 2)
lines(qy[1,] ~ px, lty = 2) # note that it doesn't cross
lines(qy[5,] ~ px, lty = 2) # same
points(y ~ x)

###
### check for sampling correlation (it's fine...)
### 
plot(m$sims.list$alpha ~ m$sims.list$beta)









# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Same code, but x is no longer z-standardized
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# sample size (n)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
n <- 100
par(mfrow = c(1,1), mar = c(5.1,5.1,2.1,2.1))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# covariate
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x <- rnorm(n, 5, 1)
hist(x, main = NULL, xlab = "x")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# simulate some data (y)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
y <- rnorm(n, x, 1)
plot(y ~ x, pch = 21, bg = 'dodgerblue', las = 1, cex.lab = 2)

# intercept will now be predicted value of y at x = 0 (less than range of data)
# expect sampling correlation

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# JAGS model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sink("./R_Scripts/2_Analyses/Code_fromOthers/From_ThomasR/lm2.jags")
cat("
      model {

      alpha ~ dnorm(0, 0.1)
      beta ~ dnorm(0, 0.1)
      sigma ~ dgamma(1,1)
      tau = 1/(sigma * sigma)
      
      for (i in 1:n){
      
        # model
        y[i] ~ dnorm(alpha + beta * x[i], tau)
        
        # calculate log-likelihood
        logL[i] <- log(dnorm(y[i], beta * x[i], tau))
        
      }

      
      }
      ",fill = TRUE)
sink()

jags.data <- list(y = y, x = x, n = n)
inits <- function(){list()}  
parameters <- c('sigma','beta','alpha','logL')

nc <- 4
nt <- 10
ni <- 25000
nb <- 10000

library(jagsUI)
Sys.time()
m <- jags(jags.data, inits, parameters, "./R_Scripts/2_Analyses/Code_fromOthers/From_ThomasR/lm2.jags", 
          n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
          parallel = T)
Sys.time()

###
### predict credible intervals
###
res <- 100
px <- seq(0,8, length.out = res)
n.iter <- (ni - nb) * nc/nt
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# not incorporating intercept uncertainty
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mu <- m$q50$alpha + m$q50$beta * px 
uc <- m$q50$alpha + m$q97.5$beta * px
lc <- m$q50$alpha + m$q2.5$beta * px

plot(mu ~ px, lty = 1, lwd = 4, type = 'l',
     ylab = 'E(y)', xlab = 'x', cex.lab = 2)
lines(uc ~ px, lty = 2) # note that they cross the median prediction?
lines(lc ~ px, lty = 2) # same
points(y ~ x)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# incorporating intercept uncertainty
# here we save 95%, 80% CIs and median
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Ey <- matrix(NA, n.iter, res)
qy <- matrix(NA, 5, res)
for (j in 1:res){
  Ey[,j] <- m$sims.list$alpha + m$sims.list$beta * px[j]
  qy[,j] <- quantile(Ey[,j], c(0.025, 0.1, 0.5, 0.9, 0.975))
}

plot(qy[3,] ~ px, lty = 1, lwd = 4, type = 'l',
     ylab = 'E(y)', xlab = 'x', cex.lab = 2)
lines(qy[1,] ~ px, lty = 2) # note that it doesn't cross
lines(qy[5,] ~ px, lty = 2) # same
points(y ~ x)

###
### check for sampling correlation (it's fine...)
### 
plot(m$sims.list$alpha ~ m$sims.list$beta)








