

# n: number of iterations
# p: posterior distribution
n <- 10000
p <- rnorm(n, 0.1, 0.25)
length(which(p > 0))/n
hist(p, breaks = 1000)
q <- quantile(p, c(0.025,0.05,0.1,0.25,0.333,0.5,0.66,0.75,0.9,0.95,0.975))

pdf("E:/O_Final/Reviews/Ecology_letters/CrIs.pdf",
    height = 5, width = 5)
par(mfrow = c(1,1), mar = c(5,5,2,2))
vioplot::vioplot(p, drawRect = F, wex = 0.5, names = '')
arrows(1, q[1], 1, q[11], length = 0, lwd = 1, col = 'white') # 95%
arrows(1, q[2], 1, q[10], length = 0, lwd = 3, col = 'white') # 90%
arrows(1, q[3], 1, q[9], length = 0, lwd = 5, col = 'white')  # 80%
arrows(1, q[4], 1, q[8], length = 0, lwd = 7, col = 'white')  # 50%
arrows(1, q[5], 1, q[7], length = 0, lwd = 9, col = 'white')  # 33%
points(1, q[6], pch = 21, bg = 'white', cex = 3)
mtext(side = 2, expression(theta), cex = 2, line = 3)
dev.off()




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# coin-flipping example
# flip 100 coins with a probability 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# prior is beta(1,1)
n <- 1000 # number of coins
nF <- 100  # number of flips
y <- rbinom(n, nF, 0.5)
hist(y)

# f-value
nS <- 1000
f <- NULL
for (i in 1:n){
  p <- rbeta(nS, y[i]+1, (nF)-y[i]+1) # posterior is beta(heads + 1, tails + 1)
  if (mean(p) > 0.5){
    f[i] <- length(which(p > 0.5))/nS
  }
  if (mean(p) <= 0.5){
    f[i] <- length(which(p < 0.5))/nS
  }
}

y[1000]
f[1000]
hist(p, breaks = 100)

hist(y)
hist(f)

length(which(f > 0.7))/n
length(which(f > 0.8))/n
length(which(f > 0.85))/n
length(which(f > 0.9))/n
length(which(f > 0.95))/n








# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# a known fate example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
nSims <- 100
f <- NULL
for (ii in 1:nSims){
  

  nT <- 25
  x <- rnorm(nT, 0, 1)
  beta <- 0
  nR <- 50
  
  phi <- plogis(0 + beta * x)
  y <- rbinom(nT, nR, phi)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Specify model in BUGS language
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sink("known_fate.jags")
  cat("
    model {

    mu ~ dlogis(0,1)
    beta ~ dnorm(0, 0.01)
    for (t in 1:nT){
      logit(phi[t]) = mu + beta * x[t]
      y[t] ~ dbin(phi[t], 50)
    }


    }
    ",fill = TRUE)
  sink()
  
  
  jags.data <- list(y = y, nT = nT, x = x)
  
  # Initial values
  inits <- function(){list()}  
  
  # Parameters monitored
  parameters <- c('phi','beta')
  
  nc <- 4
  nt <- 1
  ni <- 5000
  nb <- 2500
  
  
  library(jagsUI)
  Sys.time()
  m <- jags(jags.data, inits, parameters, "known_fate.jags", parallel = T, 
            n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
  Sys.time()
  # print(m)
  print(ii)
  print(Sys.time())
  f[ii] <- m$f$beta
}

length(which(f > 0.66))








