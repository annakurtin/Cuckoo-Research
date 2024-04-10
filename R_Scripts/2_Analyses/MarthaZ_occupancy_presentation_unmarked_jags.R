
#############################################
#OCCUPANCY MODELING IN R
#############################################

# other resources
# Occupancy example: https://oliviergimenez.github.io/occupancy-workshop/

##############################################

library(AHMbook)
library(unmarked)
library(tidyverse)
library(jagsUI)

#These notes are based off of Martha's presentation in 2023, which is based off of (This page on her webiste)[https://marthazillig.github.io/2019-12-05-Introduction-to-Occupancy-Models/]

#Simulate some occupancy data and some covariates 

M <- 100 #number of sites 
J <- 3 #number of visits 
C <- matrix(NA, nrow = M, ncol = J)

#fill in this data with 0 and 1 with a detection probability of 30%
# visit 1
C[,1] <- rbinom(M, 1, 0.3)
# visit 2
C[,2] <- rbinom(M, 1, 0.3)
# visit 3
C[,3] <- rbinom(M, 1, 0.3)
#There are no NAs. These would be removed in a usual analysis (downside to unmarked)
# Baysean methods such as JAGS can handle this

# Simulate some wind
# note that it is important to scale your variables
wind <- array(runif(M * J, -1,1), dim = c(M, J)) #making a fake windspeed covariate

# let's make a discrete covariate for habitat quality 
hab <- c(rep("Good", 33), rep("Med", 33), rep("Poor", 34)) #three habitat types - first 33 are good, next 33 are medium, and last 34 are poor

time <- runif(M,0,4)

temp <- runif(M, 0,4) #fake temperature 



#Analysis Using Unmarked Package 

#Data needs to be in an unmarkedFrame 

# using unmarkedFrameOccu for an occupancy model 
umf <- unmarkedFrameOccu(y = C, siteCovs = data.frame(hab = hab, temp = temp), obsCovs = list(wind = wind)) 
# input the dataframe, the site covariates (things that affect presence), and the observation covariates (things that effect detection probability)
# automatically converted characters to factors

summary(umf)

#now you can model the data
#Syntax of unmarked:
#occu(~obsCov ~siteCov, data) use a ~1 for intercept only 

m1 <- occu(~1 ~1, data = umf) #no covariates for detection, no covariates for occupancy

m1
# important to check model fit
## what is the mean detection?
# outputs are untransformed mean estimate for occupancy at a given site
# you always want your detection probability to be better than 15%, if it's not more than that you probably can't trust the model

#so let's transform them 
backTransform(m1, "det") #can only use when there are no covariates 
# this shows 37%

backTransform(m1, "state")
#.981- this means 98.1% probability of occupancy at each site

m2 <- occu(~1~hab, data = umf) # no covariates on detection, habitat covariate on state part
#remember the first level of habitat, "good" habitat (factor = 1) is the intercept, and the intercept for habMed and habPoor is the estimate plus the intercept 

m3 <- occu(~1~hab + temp, data = umf)
m3

m4 <- occu(~wind ~temp, data = umf)
# wind covariate for detection part of model, temperature in the state part of the model 
m4

# She's talking about these outputs like they're a linear regression, but wouldn't this be a logistic regression?
# go through a put your data into these basic models 





#JAGS 

# JAGS needs your data in a list and only takes numeric data
# JAGS doesn't do anything for you

#Simulate some occupancy data and some covariates 

M <- 100 #number of sites 
J <- 3 #number of visits 
C <- matrix(NA, nrow = M, ncol = J)

#fill in this data with 0 and 1 with a detection probability of 30%
# visit 1
C[,1] <- rbinom(M, 1, 0.3)
# visit 2
C[,2] <- rbinom(M, 1, 0.3)
# visit 3
C[,3] <- rbinom(M, 1, 0.3)
#There are no NAs. These would be removed in a usual analysis (downside to unmarked)
# Baysean methods such as JAGS can handle this

# Simulate some wind
# note that it is important to scale your variables
wind <- array(runif(M * J, -1,1), dim = c(M, J)) #making a fake windspeed covariate

# let's make a discrete covariate for habitat quality 
hab <- c(rep("Good", 33), rep("Med", 33), rep("Poor", 34)) #three habitat types - first 33 are good, next 33 are medium, and last 34 are poor

time <- runif(M,0,4)

temp <- runif(M, 0,4) #fake temperature 

## Step 1: change habitat quality into numeric
hab2 <- as.factor(hab)#JAGS will not do this for you. JAGS only takes numeric data

#STEP 2: Compile the data into a list 
data <- list(C=C, wind = wind, hab = as.numeric(hab2), M = nrow(C), J=ncol(C))
## M and J is number of sites and numer of visits, respecitvely (you have to explicitly specify this)
# Check out your data: Make sure the different vectors within the matrix are all the same length! 
data

#Write the model
## write out the mathematical formula
# cat concatenates the model into a text files
# note: since this is baysean, you have to specify your priors
cat("
    model{
    
     for(k in 1:3){ #set a prior for every level of this factor variable (habitat)
      beta1[k] ~ dunif(-5,5)
     }
    
    for (i in 1:M){   #loop over sites 
      z[i] ~ dbern(psi[i]) #state model(occupancy)
      logit(psi[i]) <- beta0 + beta1[hab[i]]
      
      for (j in 1:J) {  #loop over visits 
        C[i,j] ~ dbin(p[i,j], z[i]) #detection model
        logit(p[i,j]) <- alpha0 + beta2*wind[i,j]
      }
    }
      
  #priors (I picked non-informative priors)
    alpha0 ~ dlogis(0,1)
    beta0 ~ dnorm(0,1)
    beta2 ~ dnorm(0,1)
    
   
    
    }
    ", file = "\\scripts\\test_model.txt")

params <- c("alpha0", "beta0", "beta1", "beta2") #parameter for the model to monitor 
## you can also name them wind and habitat but this is the syntax that most people use when talking about JAGS

# Bayesian models need a place to start for latent variables 
zst <- apply(C, 1, max) #starting values for our latent z variable 
# take the C data, take the max value and start at place 1
inits <- function(){list(z = zst, beta1 = rnorm(3))} #starting values 


# do your JAGS call
# you can use jagsUI, where the call is just jags, if you use RJags you use jagsr or something like that
# give JAGS the data, the model files, the initial values, the parameters to save, the number of chains you want to run, an adaption period (optional), a thinning period (optional), a burnin period (optional - the first x number that you specify it will throw out to find the parameter space), and number of iterations
fit1_jags <- jags(data = data, model.file = "test_model.txt", inits = inits, parameters.to.save = params, n.chains = 3, n.iter = 5000, n.burnin = 1000)

fit1_jags

# bayesian - does your mean for posterior distribution overlap zero?
# for model convergence we want all values less than 1.1
# n effect is the number of draws that R used to determine the value

#another way to check model fit: plot the fitted model
plot(fit1_jags)
# we're looking for fuzzy caterpillars 

exp(fit1_jags$mean$alpha0) #important to check that detection probability is greater than 15% 
