###### Evaluate Colinearity LiDAR Derivatives #####

# This is a script to read in the cleaned lidar derivatives of canopy height and canopy cover and evaluate them for colinearity 

# date create 7/15/20234
# last modified 7/15/2024

##### packages and functions #####
packages <- c("tidyverse","janitor","ggplot2")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_SiteColumn_fromPointID.R")
source("./R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")
load_packages(packages)

##### data ######
landscape <- read.csv("./Data/Vegetation_Data/Outputs/AllPoints_LandscapeScale_LiDARMetrics_7-15-24.csv")
corearea <- read.csv("./Data/Vegetation_Data/Outputs/AllPoints_CoreAreaScale_LiDARMetrics_7-15-24.csv")
all <- left_join(landscape, corearea, by = "alt_point_id")


# Testing for colinearity 
# Landscape
ggplot(data = landscape)+
  geom_point(aes(x=percent_canopy, y = percent_subcan))
## Pearson's Correlation Test
cor.test(landscape$percent_canopy, landscape$percent_subcan)
## Fit a linear model 
landscape_lm <- lm(percent_canopy ~ percent_subcan, data = landscape)
summary(landscape_lm)
# Explains a small portion but is statistically significant 
plot(landscape$percent_canopy, landscape$percent_subcan, type = "p")
abline(landscape_lm, col = "red")


# Core area
ggplot(data = corearea)+
  geom_point(aes(x=percent_canopy, y = percent_subcan))
cor.test(corearea$percent_canopy, corearea$percent_subcan)
core_lm <- lm(percent_canopy ~ percent_subcan, data = corearea)
summary(core_lm)
plot(corearea$percent_canopy, corearea$percent_subcan, type = "p")
abline(core_lm, col = "red")


pairs(~percent_canopy+percent_subcan, data = corearea, main = "Scatterplot Matrix")
cor.prob <- function(X, dfr = nrow(X) - 2) {
  R <- cor(X, use="complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R
}
# Testing multiple covariates
cor.prob(as.matrix(all[,c("percent_canopy.x", "percent_canopy.y", "percent_subcan.x", "percent_canopy.y")]))
# How to interpret this???