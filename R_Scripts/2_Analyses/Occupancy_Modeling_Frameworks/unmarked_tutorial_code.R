## Run Unmarked Model on Simulated Project Data ####

# Script created 3/21/2024
# Script last edited 3/21/2024

## Unmarked Resources ####

# https://doi90.github.io/lodestar/fitting-occupancy-models-with-unmarked.html

## Setup ####
library(unmarked)

#### Simulate data ####

colnames <- c("point_id","pres_visit1","pres_visit2","pres_visit3","pres_populus","percent_veg_50","background_db","date1","date2","date3")

# generate a list of point IDs
point_id <- c(1:85)
# Simulate 3 sampling periods of detection data based on prelim detections from habitat points
# 85 total habitat points, 18 with detections, 21% naive occupancy 
pres_visit1 <-rbinom(n = 85,size = 1, prob = .21)
pres_visit2 <-rbinom(n = 85,size = 1, prob = .21)
pres_visit3 <-rbinom(n = 85,size = 1, prob = .21)

# Site-level covariates
# Discrete: simulate whether cottonwood was present or not at the sites (estimate a 30% probability that cottonwood present)
populus <- rbinom(n = 85, size = 1, prob = .35)
# Continuous: Simulate GEDI percent of returns coming from 50% height
percent_veg_50 <- round(runif(n = 85, min = 10, max = 80),2)

# Sonic masking: could do this as a site level or a survey level covariate
# just starting with site level
background_db <- round(runif(n = 85, min = -70, max = -17),2)

# Survey-level covariates
# date of first survey ranging from June 1 (J.D. 152) to Aug 1 (J.D. 213)
julian_date1 <- round(runif(n=85, min = 152, max = 213),0)
# date of second survey: two days apart
julian_date2 <- julian_date1 + 2
# date of last survey: two days apart
julian_date3 <- julian_date2 + 2

# Combine into dataframe
# Combine lists into a data frame
df <- data.frame(
  point_id = point_id,
  pres_visit1 = pres_visit1,
  pres_visit2 = pres_visit2,
  pres_visit3 = pres_visit3,
  pres_populus = populus,
  percent_veg_50 = percent_veg_50,
  background_db = background_db,
  date1 = julian_date1,
  date2 = julian_date2,
  date3 = julian_date3
)

# Create an unmarked dataframe
y <- df[,2:4]
siteCovs <- df[,5:7]
obsCovs <- list(date = df[,8:10])

umf <- unmarkedFrameOccu(y = y, siteCovs = siteCovs, obsCovs = obsCovs)
# Look at the dataframe to make sure it's correct
summary(umf)

# standardize the data
umf@siteCovs$pres_populus <- scale(umf@siteCovs$pres_populus)
umf@siteCovs$percent_veg_50 <- scale(umf@siteCovs$percent_veg_50)
umf@siteCovs$background_db <- scale(umf@siteCovs$background_db)

umf@obsCovs$date <- scale(umf@obsCovs$date)

#### Fit the models ####
# Naive model
fit_occu_naive <- occu(formula = ~ 1 # detection formula
                 ~ 1, # occupancy formula
                 data = umf)
fit_occu_naive
# back transform these
backTransform(fit_occu_naive, type = 'state')

# Model with covriates
fit_occu_full <- occu(formula = ~ date + background_db ~ pres_populus + percent_veg_50, data = umf)
#### Left off here ####