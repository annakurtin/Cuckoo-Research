##### Demonstration of Gamma-Poisson and Poisson Distribution ####
# From discussion 9/20

# Initilize number of datapoints/samples/sites
n <- 100
# simulate covariate
x <- rnorm(n, 0, 1)
# Simulate response drawn from poisson distrib with effect of x
y <- rpois(n, exp(1 + 1 * x))
# Visualize this
plot(y ~ x)

# Here we see that the variance is much greater than the mean 
mean(y)
var(y)


# When we run a GLM on it with a poisson family, it works fine 
summary(glm(y ~ x, family = 'poisson'))


# Draw 10000 points from a poisson distribution with an intensity (mean and variance) around 1
n <- 100000
a <- rpois(n, 1)
table(a)
hist(a)

# Draw 10000 points from a poisson distribution with an intensity (mean and variance) drawn from a gamma distribution centered around 5)
theta <- 1
h <- rgamma(n, theta, theta)
hist(h)
b <- rpois(n, 0.5 * h)

table(b)
hist(b)




hist(rgamma(n, 1,1), breaks = 1000)

hist(rgamma(n, 10,10), breaks = 1000)
