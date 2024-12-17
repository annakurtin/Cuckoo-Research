# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hi Anna,
#
# Maybe we can chat quickly to make sure we're on the same page?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#library(tidyverse)
library(reshape2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# simulate two posterior distributions (an intercept, beta0, and a slope, beta2)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
n <- 1000
beta0 <- rnorm(n, -1, 0.5)
beta2 <- rnorm(n, -0.5, 0.15)
hist(beta2)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# if I just want the 'beta coefficient'
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mean(beta2)
median(beta2)
quantile(beta2, c(0.025,0.5,0.975)) # 95% CrI's and median
# f-value
ifelse(mean(beta2) < 0, length(which(beta2 < 0)), length(which(beta2 > 0)))/n

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# simulate a covariate scale to use for prediction (res is short for resolution)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
res <- 100
x <- seq(-3,3, length.out = res)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculate expected value with full uncertainty as well as 85% and 95% CrIs
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ev <- matrix(NA, n, res)
qe <- matrix(NA, res, 5)
for (j in 1:res){
  ev[,j] <- plogis(beta0 + beta2 * x[j])                      # calculate expected values
  qe[j,] <- quantile(ev[,j], c(0.025,0.075,0.5,0.925,0.975))  # quantiles
}


# melt expected values to make a smooth scatter plot
me <- melt(ev)
names(me) <- c('i','x','e')

par(mar = c(5.1,5.1,2.1,2.1))
smoothScatter(me$e ~ x[me$x],
              nrpoints = 0,
              #ylab = TeX("Occupancy probability ($\\psi$))"),
              ylab = "Occupancy probability",
              xlab = 'Covariate value')
lines(qe[,3] ~ x, lwd = 3, col = 'white')
# 95% credible intervals
lines(qe[,1] ~ x, lwd = 2, col = 'white', lty = 2)
lines(qe[,5] ~ x, lwd = 2, col = 'white', lty = 2)

# 85% credible intervals
lines(qe[,2] ~ x, lwd = 2, col = 'white', lty = 3)
lines(qe[,4] ~ x, lwd = 2, col = 'white', lty = 3)

