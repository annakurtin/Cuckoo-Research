## This is a script from Thomas to show the difference between ways of parameterizing quadratic effects of models

# Examples: Nonstandarzied covariates

# continuous covariate
x <- seq(1,100, length.out = 100)
# linear
beta <- c(2, -0.05)
y <- plogis(beta[1] + beta[2] * x)
plot(y ~ x, ylab = 'p', xlab = 'Date')

# x^2
beta <- c(2, -0.1, -0.0025)
y <- plogis(beta[1] + beta[3] * x * x)
plot(y ~ x, ylab = 'p', xlab = 'Date')

# quadratic
beta <- c(-4, 0.45, -0.005)
y <- plogis(beta[1] + beta[2] * x + beta[3] * x * x)
plot(y ~ x, ylab = 'p', xlab = 'Date')




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# z-standardized covarates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
x <- seq(-2,2, length.out = 100)


# linear
beta <- c(0, -0.75)
y <- plogis(beta[1] + beta[2] * x)
plot(y ~ x, ylab = 'p', xlab = 'Date')

# x^2
beta <- c(0, 0.25)
y <- plogis(beta[1] + beta[2] * x * x)
plot(y ~ x, ylab = 'p', xlab = 'Date')

# quadratic
beta <- c(0, 0.75, -0.5)
y <- plogis(beta[1] + beta[2] * x + beta[3] * x * x)
plot(y ~ x, ylab = 'p', xlab = 'Date', col = "blue")


# Main takeaway: When you have z-standardized data, it changes what coefficients affect which parts of the distribution. 
## Without z-standardized: B0 is intercept on left hand side of graph, B1 is effect on left hand side of graph before inflection, B2 is effect on right hand side of the graph before inflection 
## With z-standardized: B0 is the intercept in the middle of the graph (on the inflection point), B1 is moving that point left and right and affecting the shape of the inflection point, and B2 controls the slop on the left and right hand sides/curves of the graph. 

## When you parameterize a model with just B*x2, you just get the regular quadratic shape. When you do Bx + Bx^2, you get a rise/plateau with a drop off after that.


# Playing around with quadratic
# Change b0 to 1, the intercept for the graph increases and the shape plateaus more
beta <- c(1, 0.75, -0.5)
b0 <- beta[1]
b1 <- beta[2]
b2 <- beta[3]
y <- plogis(b0 + b1 * x + b2 * x * x)
plot(y ~ x, ylab = 'p', xlab = 'Date')

# Change b1 to 2, now the x position of the intercept is shifted to the right (can flip this around with making it -2)
beta <- c(0, 2, -0.5)
b0 <- beta[1]
b1 <- beta[2]
b2 <- beta[3]
y <- plogis(b0 + b1 * x + b2 * x * x)
plot(y ~ x, ylab = 'p', xlab = 'Date')

# Change b1 to 2 and b0 to 1, this shifts it to the right and even farther up and gives it an even bigger curve
beta <- c(1, 2, -0.5)
b0 <- beta[1]
b1 <- beta[2]
b2 <- beta[3]
y <- plogis(b0 + b1 * x + b2 * x * x)
plot(y ~ x, ylab = 'p', xlab = 'Date')

# change b2 to -1 - makes the slope steeper
beta <- c(1, 0.75, -1)
b0 <- beta[1]
b1 <- beta[2]
b2 <- beta[3]
y <- plogis(b0 + b1 * x + b2 * x * x)
plot(y ~ x, ylab = 'p', xlab = 'Date')


# change b2 to 0,5 - flips the direction of the graph 
## Why does the intercept not stay at 1?
beta <- c(1, 0.75, 0.5)
b0 <- beta[1]
b1 <- beta[2]
b2 <- beta[3]
y <- plogis(b0 + b1 * x + b2 * x * x)
plot(y ~ x, ylab = 'p', xlab = 'Date')
