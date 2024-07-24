# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This is code recieved from Thomas R on 7/12, which I modified to more closely match the parameters I'm dealing with. I also made the plots more readable.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# packages
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(unmarked)
library(beepr)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# number of simulations at each sample size
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nSims <- 50

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# gradient of samples sizes (i.e., how many steps to take from min to max)
# note you should set this up so that it iterates in even numbers, but
# the code should work even with some rounding...
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
res <- 23
min.sites <- 60
max.sites <- 300
sites_range <- round(seq(min.sites, max.sites, length.out = res), 0)
# sites_range <- round(sites_range)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# number of covariates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nCov <- 5

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# number of visits
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nVis <- 6

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# detection probability
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p <- 0.2



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# arrays and matrices to store outpu
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
est <- array(NA, dim = c(nSims, res, nCov))
se <- array(NA, dim = c(nSims, res, nCov))
bias <- array(NA, dim = c(nSims, res, nCov))
tru <- array(NA, dim = c(nSims, res, nCov))
lci <- array(NA, dim = c(nSims, res, nCov))
uci <- array(NA, dim = c(nSims, res, nCov))

for (ii in 1:nSims){
  for (jj in 1:res){
    
    # random covariate effects
    beta <- rnorm(nCov, 0, 1)

    
    # each site has a random z-scaled covariate value
    x <- matrix(NA, sites_range[jj], nCov)    
    for (k in 1:nCov){
      x[,k] <- rnorm(sites_range[jj], 0, 1)
    }

    
    # probability of occupancy (theta)
    theta <- plogis(0 + x %*% beta)
    z <- rbinom(sites_range[jj], 1, theta)
    
    # observation data
    y <- matrix(NA, sites_range[jj], nVis)
    for (k in 1:nVis){
      y[,k] <- rbinom(sites_range[jj], z, p)
    }
    
    
    x <- data.frame(x); names(x) <- paste0('x',1:nCov)
    
    dat <- unmarkedFrameOccu(y = y, siteCovs = x)
    #summary(dat)
    
    tmp <- as.formula(paste('~ 1',' ~', paste0('x',1:nCov, collapse = ' + ')))
    mod <- occu(formula = tmp,
                data = dat)

    est[ii,jj,] <- summary(mod)$state$Estimate[2:(nCov+1)]                 # mean
    se[ii,jj,] <- summary(mod)$state$SE[2:(nCov+1)]                        # SE
    lci[ii,jj,] <- confint(type = 'state', level = 0.95, mod)[1:nCov,1]    # lower CI
    uci[ii,jj,] <- confint(type = 'state', level = 0.95, mod)[1:nCov,2]    # upper CI 
    tru[ii,jj,] <- beta                                                    # data generating value
    bias[ii,jj,] <- est[ii,jj,] - beta
    
  }
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here is a plot of bias for covariate 1...
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
par(mar = c(5.1,5.1,2,2))
boxplot(bias[,,1], names = sites_range,
        ylab = expression(hat(beta)~'-'~beta), xlab = 'Simulated sites',
        cex.lab = 1.5,
        ylim = c(-10,10))  # I added in constraints on the y axis to better see those closer to zero

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here is a plot of SE for covariate 1...
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
boxplot(se[,,1], names = sites_range, 
        ylab = 'Standard error (se)',
        ylim = c(0,15)) # I added in constraints on the y axis to better see those closer to zero


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# estimate vs. truth at upper end of simulation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot(est[,res,1] ~ tru[,res,1]); abline(0,1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Here is a quick and dirty power analysis... first we'll state our goals...
# 1) I want the true beta value to be within 0.25, (and the CIs to cover the true beta?)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
power <- rep(NA, res)
for (k in 1:res){
  # power[k] <- length(which(abs(bias[,k,1]) < 1 & lci[,k,1] < tru[,k,1] & uci[,k,1] > tru[,k,1]))
  power[k] <- length(which(abs(bias[,k,1]) < 0.25))
}
power <- power/nSims
plot(power ~ sites_range)
# if we upped the number of simulations to 1000s this would converge into a very smooth line...

beep(sound = 3)