# Replicating thomas' code

# Take 100 sites
res <- 100
# Simulate the parameters for a0
intercept <- rnorm(res, -1, 0.5)
# Simulate the parameters for a1 
increase <- rnorm(res, 0.5, 0.5)


# Correct way to interpret these
effect <- plogis(intercept+increase)

# Plot the effects separatey 
boxplot(intercept, increase)
# Plot the effects together - add the parameters effect onto the intercept
boxplot(intercept+increase)
