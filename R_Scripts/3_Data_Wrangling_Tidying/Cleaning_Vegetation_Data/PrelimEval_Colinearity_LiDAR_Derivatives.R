###### Evaluate Colinearity LiDAR Derivatives #####

# This is a script to read in the cleaned lidar derivatives of canopy height and canopy cover and evaluate them for colinearity 

# date create 7/15/20234
# last modified 7/18/2024

##### packages and functions #####
packages <- c("tidyverse","janitor","ggplot2","corrgram")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")
source("./R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")
load_packages(packages)

##### data ######
all <- read.csv("./Data/Vegetation_Data/Outputs/AllPoints_AllScales_LiDARMetrics_7-23-24.csv")

### Look at distributions ####
boxplot(all$percent_canopy_core,all$percent_subcan_core, all$percent_canopy_landsc, all$percent_subcan_landsc,
        names = c("% Canopy Core","% SubCan Core","% Canopy Landscape", "% Subcan Landscape"))
boxplot(all$canopy_avgheight_core, all$subcan_avgheight_core, all$subcan_stdev_core,
        names = c("Core Canopy Height", "Core Subcan Height", "Core Subcan St Dev"))

#### test for colinearity between covariates #####
# Visualize it
pairs(~percent_canopy_landsc+percent_subcan_landsc+percent_canopy_core+percent_subcan_core+canopy_avgheight_core+subcan_avgheight_core+subcan_stdev_core, data = all, main = "Scatterplot Matrix")
# Function for calculating correlation
cor.prob <- function(X, dfr = nrow(X) - 2) {
  R <- cor(X, use="complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R_fin <- round(R,4)
  return(R_fin)
}
# Testing multiple covariates
cor.prob(as.matrix(all[,c("percent_canopy_landsc", "percent_subcan_landsc", "percent_canopy_core", "percent_subcan_core","canopy_avgheight_core","subcan_avgheight_core","subcan_stdev_core", "all_stdev_core")]))

# Looking at univariate relationships
# with a threshold of .8 (what Andy said), these are all good
# with a threshold of .5 (what Mark said), there are three to worry about
## percent canopy core and canopy average height core .77
plot(all$percent_canopy_core~all$canopy_avgheight_core)
abline(lm(all$percent_canopy_core~all$canopy_avgheight_core))
## subcanopy standard deviation and percent canopy landscape .57
plot(all$percent_canopy_core~all$subcan_stdev_core)
abline(lm(all$percent_canopy_core~all$subcan_stdev_core))
## percent canopy core and subcanopy average height core .54
plot(all$percent_canopy_core~all$subcan_avgheight_core)
abline(lm(all$percent_canopy_core~all$subcan_avgheight_core))
## percent canopy landscape and percent canopy core
plot(all$percent_canopy_core~all$percent_canopy_landsc)
abline(lm(all$percent_canopy_core~all$percent_canopy_landsc))

# other things to look at
# Generally high canopycover = low subcanopy cover
# there are points with lots of subcanopy and little canopy
# there are point with even mixtures of both
plot(all$percent_canopy_landsc~all$percent_subcan_landsc) # not much correlation, like .22 here
plot(all$percent_subcan_landsc~all$percent_subcan_core)
plot(all$subcan_avgheight_core~all$subcan_stdev_core)

#This makes a scatterplot matrix with the upper panel defined as the Pearsons correlation coefficient expressed in Pie graph form (where r = 1.0), red = negative correlation and blue equals a positive correlation. Similarly, the bottom panel displays the strength of the correlation in shaded color.
jpeg("./Deliverables/Veg_Data_Covariance/veg_covariance.jpg", width=800, height=800)
corrgram(all[2:8], order=TRUE, lower.panel=panel.pie,
         upper.panel=panel.pts, text.panel=panel.txt,
         main="Correlations in the Veg Data")
dev.off()
#### Archive ####
# # Plot the ones close to 1 or -1 (looking at everything more extreme than .5)
# plot(all$percent_canopy_core~all$percent_subcan_landsc)

# # Testing for colinearity 
# # Landscape
# ggplot(data = landsc)+
#   geom_point(aes(x=percent_canopy, y = percent_subcan))
# ## Pearson's Correlation Test
# cor.test(landsc$percent_canopy, landsc$percent_subcan)
# ## Fit a linear model 
# landscape_lm <- lm(percent_canopy ~ percent_subcan, data = landsc)
# summary(landscape_lm)
# # Explains a small portion but is statistically significant 
# plot(landsc$percent_canopy, landsc$percent_subcan, type = "p")
# abline(landscape_lm, col = "red")
# 
# 
# # Core area
# ggplot(data = core)+
#   geom_point(aes(x=percent_canopy, y = percent_subcan))
# cor.test(core$percent_canopy, core$percent_subcan)
# core_lm <- lm(percent_canopy ~ percent_subcan, data = core)
# summary(core_lm)
# plot(core$percent_canopy, core$percent_subcan, type = "p")
# abline(core_lm, col = "red")
# 
# 
# pairs(~percent_canopy+percent_subcan, data = core, main = "Scatterplot Matrix")
# cor.prob <- function(X, dfr = nrow(X) - 2) {
#   R <- cor(X, use="complete.obs")
#   above <- row(R) < col(R)
#   r2 <- R[above]^2
#   Fstat <- r2 * dfr / (1 - r2)
#   R[above] <- 1 - pf(Fstat, 1, dfr)
#   R
# }
# # Testing multiple covariates
# cor.prob(as.matrix(all[,c("percent_canopy.x", "percent_canopy.y", "percent_subcan.x", "percent_canopy.y")]))
# # How to interpret  this???
# 
# 
# cor.test(all$percent_canopy.x, all$percent_canopy.y, all$percent_subcan.x)
# # Cor test can only do two at a time
# 

#landsc <- read.csv("./Data/Vegetation_Data/Outputs/AllPoints_LandscapeScale_LiDARMetrics_7-15-24.csv")
#core <- read.csv("./Data/Vegetation_Data/Outputs/AllPoints_CoreAreaScale_LiDARMetrics_7-15-24.csv")
#all <- left_join(landsc, core, by = "alt_point_id")