# Visualizing posterior distributions - code from Thomas 10/31/2024

# Do not run - this takes a while
# Not sure where this wrote the jags or blavaan file to???

n <- 200
x <- rnorm(n, 0,1)
y <- rnorm(n,x,1)
library(blavaan)
dat <- data.frame(y,x)
m <- bsem(y ~ x,target = 'jags', data = dat)
summary(m)
str(m)
dim(m$sims.list)
n.iter <- 
res <- 100
px <- seq(NA, n.iter, res)
