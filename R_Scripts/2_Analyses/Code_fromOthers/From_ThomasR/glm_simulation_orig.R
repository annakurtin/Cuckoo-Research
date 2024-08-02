

# number of simulations at each sample size
nSims <- 100

# gradient of samples sizes
res <- 50
sites_range <- seq(20,1000, length.out = res)

# number of covariates (note changing this to > 1 will require in pulling in
# some of your code from your other script)
nCov <- 1



est <- array(NA, dim = c(nSims, res, nCov))
se <- array(NA, dim = c(nSims, res, nCov))
tru <- matrix(NA, nSims, res)
bias <- matrix(NA, nSims, res)

for (ii in 1:nSims){
  for (jj in 1:res){
    beta <- rnorm(1, 0, 1)
    x <- rnorm(sites_range[jj], 0, 1)
    # probability of whatever (theta)
    theta <- plogis(0 + beta * x)
    y <- rbinom(sites_range[jj], 1, theta)
    
    est[ii,jj,1] <- summary(glm(y ~ x, family = 'binomial'))$coefficients[2,1]
    se[ii,jj,1] <- summary(glm(y ~ x, family = 'binomial'))$coefficients[2,2]
    tru[ii,jj] <- beta
    bias[ii,jj] <- est[ii,jj,1] - beta
  }
}





par(mar = c(5,5,2,2))
boxplot(bias, names = sites_range,
        ylab = expression(hat(beta)~'-'~beta), xlab = 'Simulated sites',
        cex.lab = 1.5)
boxplot(se[,,1], names = sites_range, ylab = 'Standard error (se)')
