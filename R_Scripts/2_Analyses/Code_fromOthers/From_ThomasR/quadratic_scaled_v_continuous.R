
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
plot(y ~ x, ylab = 'p', xlab = 'Date')

