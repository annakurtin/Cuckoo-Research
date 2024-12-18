##### Create Visualizations for Habitat Chapter Modelling Results #####
library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggdist)
library(jagsUI)
library(jagshelper)
source("./R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")
# To save plots, run this after the line you generated??
# Save a specific plot object as a JPEG
#ggsave("my_plot.jpeg", plot = my_plot, width = 8, height = 6, dpi = 300)
# Gives an error that plot not found :(


# Next up: read in the code to predict effects, add them together in a multifigure using patch work

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in models #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Detection model
habdet <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Detection_Submodel/Models_Ran_Outputs/Actual_Data/DetSubModel_Final4.Rdata")

# Landscape Scale Model 
fit_B <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Occupancy_Models/Models_Ran_Outputs/StateSubModel_BFinal3.Rdata")

# Core Scale Model
fit_C <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Occupancy_Models/Models_Ran_Outputs/StateSubModel_CFinal3.Rdata")

# Point Scale Model
fit_A <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Occupancy_Models/Models_Ran_Outputs/StateSubModel_AFinal3.Rdata")

# Linear model
fit_gpois <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Poisson_Model/Models_Ran_Outputs/LMod15_GamPois_14Day.Rdata")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make Density Plots #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Detection Model Density Plots
chains <- jags_df(habdet)
# Select he chains for our covariates of interest
chains_viol <- chains %>% select(b1,b2,b3,b4,b4Q, b5)
# Rename them to be more interpretable
colnames(chains_viol) <- c("Vegetation Density","Background Noise","Survey Effort", "Julian Date","Julian Date²","ARU Type")
# Pivot this longer so that we can visualize it
chains_viol_long <- chains_viol %>% pivot_longer(cols = c("Vegetation Density","Background Noise","Survey Effort", "Julian Date","Julian Date²","ARU Type"),names_to = "parameter", values_to = "values")
# Specify the desired order of the y-axis values
chains_viol_long$parameter <- factor(chains_viol_long$parameter, 
                                     levels = c("Julian Date",
                                                "Julian Date²",
                                                "Background Noise",
                                                "Survey Effort",
                                                "Vegetation Density",
                                                "ARU Type"))

f_stat <- data.frame(
  parameter = c("Vegetation Density","Background Noise","Survey Effort", "Julian Date","Julian Date²","ARU Type"),
  median_value = c(round(habdet$f$b1,2), 
                   round(habdet$f$b2,2), 
                   round(habdet$f$b3,2), 
                   round(habdet$f$b4,2), 
                   round(habdet$f$b4Q,2),
                   round(habdet$f$b5,2))
)

# Create the slab interval plot
dense_detocc <- ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval(height = 1.5) +
  # Establish colors
  scale_fill_manual(values = c("Vegetation Density"=d_palette[8], 
                               "Background Noise" = d_palette[4],
                               "Survey Effort"=d_palette[6], 
                               "Julian Date" = d_palette[1], 
                               "Julian Date²" = d_palette[3],
                               "ARU Type" = d_palette[10])) +
  # Remove background color from plots
  theme_minimal() +
  # Adjust axis titles
  labs(y = NULL, x = "Posterior Estimate") +
  # Adjust axis labels
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), hjust = 1,
        axis.title.x = element_text(size = 13)) +
  # Adjust x axis
  scale_x_continuous(limits = c(-3.5,3), breaks = seq(-3,2, by = 1)) +
  # Add a line for 0 to show overlap of posterior
  geom_vline(xintercept = 0, color = "gray12", linetype = "dashed") +
  # Add median values as text labels
  geom_label(data = f_stat, 
             aes(x = 2.75, y = parameter, label = median_value),
             size = 6, color = "black", 
             hjust = .6, label.size = 0) +
  annotate("label", x = 2.65, y = 7, label = "F Statistic:", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  # Turn off the legend
  guides(fill = FALSE)
#ggsave("my_plot.jpeg", plot = my_plot, width = 8, height = 6, dpi = 300)



# Landscape Model Density Plots ####
# Plot posterior distribution
chains_bf2 <- jags_df(fit_B)
# Select the chains for our covariates of interest
chains_viol <- chains_bf2 %>% select(a1,a2)
# Rename them to be more interpretable
colnames(chains_viol) <- c("% Canopy Cover","% SES Cover")
# Pivot this longer so that we can visualize it
chains_viol_long <- chains_viol %>% pivot_longer(cols = c("% Canopy Cover","% SES Cover"),names_to = "parameter", values_to = "values")
# Specify the desired order of the y-axis values
chains_viol_long$parameter <- factor(chains_viol_long$parameter, 
                                     levels = c("% SES Cover","% Canopy Cover"))

f_stat <- data.frame(
  parameter = c("% Canopy Cover",
                "% SES Cover"),
  median_value = c(round(fit_B$f$a1,2),
                   round(fit_B$f$a2,2))
)

# Create the slab interval plot
dense_lanscocc <- ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval(height = 1.5) +
  # Establish colors
  scale_fill_manual(values = c("% Canopy Cover"=l_palette[7], 
                               "% SES Cover" = l_palette[5])) +
  # Remove background color from plots
  theme_minimal() +
  # Adjust axis titles
  labs(y = NULL, x = "Posterior Estimate") +
  # Adjust axis labels
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), hjust = 1,
        axis.title.x = element_text(size = 13)) +
  # Adjust x axis
  scale_x_continuous(limits = c(-3.5,3), breaks = seq(-3,2, by = 1)) +
  # Add a line for 0 to show overlap of posterior
  geom_vline(xintercept = 0, color = "gray12", linetype = "dashed") +
  # Add median values as text labels
  geom_label(data = f_stat, 
             aes(x = 2.75, y = parameter, label = median_value),
             size = 6, color = "black", 
             hjust = .6, label.size = 0) +
  annotate("label", x = 2.65, y = 2.5, label = "F Statistic:", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  # Turn off the legend
  guides(fill = FALSE)


# Core Model Density Plots ####
# Plot posterior distribution
chains_cf2 <- jags_df(fit_C)
# Select the chains for our covariates of interest
chains_viol <- chains_cf2 %>% select(a1,a2, a3, a4)
# Rename them to be more interpretable
colnames(chains_viol) <- c("% SES Cover","Canopy Height", "SES Height", "Vegetation Complexity")
# Pivot this longer so that we can visualize it
chains_viol_long <- chains_viol %>% pivot_longer(cols = c("% SES Cover","Canopy Height", "SES Height", "Vegetation Complexity"),names_to = "parameter", values_to = "values")
# Specify the desired order of the y-axis values
chains_viol_long$parameter <- factor(chains_viol_long$parameter, 
                                     levels = c("% SES Cover",
                                                "Canopy Height", 
                                                "SES Height", 
                                                "Vegetation Complexity"))

f_stat <- data.frame(
  parameter = c("% SES Cover",
                "Canopy Height", 
                "SES Height", 
                "Vegetation Complexity"),
  median_value = c(round(fit_C$f$a1,2),
                   round(fit_C$f$a2,2),
                   round(fit_C$f$a3,2), 
                   round(fit_C$f$a4,2))
)

# Create the slab interval plot
dense_coreocc <- ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval(height = 1.5) +
  # Establish colors
  scale_fill_manual(values = c("% SES Cover"= c_palette[1], 
                               "Canopy Height" = c_palette[3], 
                               "SES Height" = c_palette[4], 
                               "Vegetation Complexity" = c_palette[6])) +
  # Remove background color from plots
  theme_minimal() +
  # Adjust axis titles
  labs(y = NULL, x = "Posterior Estimate") +
  # Adjust axis labels
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), hjust = 1,
        axis.title.x = element_text(size = 13)) +
  # Adjust x axis
  scale_x_continuous(limits = c(-3.5,3), breaks = seq(-3,2, by = 1)) +
  # Add a line for 0 to show overlap of posterior
  geom_vline(xintercept = 0, color = "gray12", linetype = "dashed") +
  # Add median values as text labels
  geom_label(data = f_stat, 
             aes(x = 2.75, y = parameter, label = median_value),
             size = 6, color = "black", 
             hjust = .6, label.size = 0) +
  annotate("label", x = 2.75, y = 4.75, label = "F Statistic:", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  # Turn off the legend
  guides(fill = FALSE)



# Point Scale Density Plots #####
# Plot posterior distribution
chains_af1 <- jags_df(fit_A)
# Select the chains for our covariates of interest
chains_viol <- chains_af1 %>% select(a1,a2,a3,a4)
# Rename them to be more interpretable
colnames(chains_viol) <- c("Deciduous Tree Richness","Conifer Tree Richness","Floodplain Shrub","Broadleaf Shrub")
# Pivot this longer so that we can visualize it
chains_viol_long <- chains_viol %>% pivot_longer(cols = c("Deciduous Tree Richness","Conifer Tree Richness","Floodplain Shrub","Broadleaf Shrub"),names_to = "parameter", values_to = "values")
# Specify the desired order of the y-axis values
chains_viol_long$parameter <- factor(chains_viol_long$parameter, 
                                     levels = c("Floodplain Shrub",
                                                "Broadleaf Shrub",
                                                "Conifer Tree Richness",
                                                "Deciduous Tree Richness"))

f_stat <- data.frame(
  parameter = c(
    "Deciduous Tree Richness", 
    "Conifer Tree Richness", 
    "Floodplain Shrub", 
    "Broadleaf Shrub"),
  median_value = c(
    round(fit_A$f$a1,2),
    round(fit_A$f$a2,2),
    round(fit_A$f$a3,2),
    round(fit_A$f$a4,2))
)

# Create the slab interval plot
dense_pointocc <- ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval(height = 1.5) +
  # Establish colors
  scale_fill_manual(values = c("Deciduous Tree Richness"=p_palette[9], 
                               "Conifer Tree Richness" = p_palette[7],
                               "Floodplain Shrub"=p_palette[3], 
                               "Broadleaf Shrub" = p_palette[5])) +
  # Remove background color from plots
  theme_minimal() +
  # Adjust axis titles
  labs(y = NULL, x = "Posterior Estimate") +
  # Adjust axis labels
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), hjust = 1,
        axis.title.x = element_text(size = 13)) +
  # Adjust x axis
  scale_x_continuous(limits = c(-3.5,3), breaks = seq(-3,2, by = 1)) +
  # Add a line for 0 to show overlap of posterior
  geom_vline(xintercept = 0, color = "gray12", linetype = "dashed") +
  # Add median values as text labels
  geom_label(data = f_stat, 
             aes(x = 2.75, y = parameter, label = median_value),
             size = 6, color = "black", 
             hjust = .6, label.size = 0) +
  annotate("label", x = 2.75, y = 4.75, label = "F Statistic:", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  # Turn off the legend
  guides(fill = FALSE)



## Linear Model 
# Plot posterior distribution
chains_gpois <- jags_df(fit_gpois)
# Select the chains for our covariates of interest
chains_viol <- chains_gpois %>% select(beta1,beta2,beta3,beta4, beta5, beta6, beta7, beta8)
# Rename them to be more interpretable
colnames(chains_viol) <- c("% Canopy Landscape",
                           "% SES Landscape",
                           "Vegetation Complexity Core",
                           "% SES Core",
                           "Conifer Tree Richness Point",
                           "Recording Days",
                           "Background Noise",
                           "Vegetation Density")
# Pivot this longer so that we can visualize it
chains_viol_long <- chains_viol %>% pivot_longer(cols = c("% Canopy Landscape",
                                                          "% SES Landscape",
                                                          "Vegetation Complexity Core",
                                                          "% SES Core",
                                                          "Conifer Tree Richness Point",
                                                          "Recording Days",
                                                          "Background Noise",
                                                          "Vegetation Density"),names_to = "parameter", values_to = "values")
# Specify the desired order of the y-axis values
chains_viol_long$parameter <- factor(chains_viol_long$parameter, 
                                     levels = c("% Canopy Landscape",
                                                "% SES Landscape",
                                                "Vegetation Complexity Core",
                                                "% SES Core",
                                                "Conifer Tree Richness Point",
                                                "Recording Days",
                                                "Background Noise",
                                                "Vegetation Density"))

f_stat <- data.frame(
  parameter = c(
    "% Canopy Landscape",
    "% SES Landscape",
    "Vegetation Complexity Core",
    "% SES Core",
    "Conifer Tree Richness Point",
    "Recording Days",
    "Background Noise",
    "Vegetation Density"),
  median_value = c(
    round(fit_gpois$f$beta1,2),
    round(fit_gpois$f$beta2,2),
    round(fit_gpois$f$beta3,2),
    round(fit_gpois$f$beta4,2),
    round(fit_gpois$f$beta5,2),
    round(fit_gpois$f$beta6,2),
    round(fit_gpois$f$beta7,2),
    round(fit_gpois$f$beta8,2))
)

# Create the slab interval plot
dense_lm <- ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval(height = 1.5) +
  # Establish colors
  scale_fill_manual(values = c("% Canopy Landscape"=l_palette[7], 
                               "% SES Landscape" = l_palette[5],
                               "Vegetation Complexity Core"=c_palette[6], 
                               "% SES Core" = c_palette[1],
                               "Conifer Tree Richness Point" = p_palette[7],
                               "Recording Days" = d_palette[6],
                               "Background Noise" = d_palette[4],
                               "Vegetation Density" = d_palette[8])) +
  # Remove background color from plots
  theme_minimal() +
  # Adjust axis titles
  labs(y = NULL, x = "Posterior Estimate") +
  # Adjust axis labels
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), hjust = 1,
        axis.title.x = element_text(size = 13)) +
  # Adjust x axis
  scale_x_continuous(limits = c(-3.5,3), breaks = seq(-3,2, by = 1)) +
  # Add a line for 0 to show overlap of posterior
  geom_vline(xintercept = 0, color = "gray12", linetype = "dashed") +
  # Add median values as text labels
  geom_label(data = f_stat, 
             aes(x = 2.75, y = parameter, label = median_value),
             size = 6, color = "black", 
             hjust = .6, label.size = 0) +
  annotate("label", x = 2.5, y = 8.75, label = "F Statistic:", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  # Turn off the legend
  guides(fill = FALSE)



#### GRave YArD #####
# Landscape plot
# Add a line for 0 to show overlap of posterior
#geom_vline(xintercept = 0) +
# # Remove background color from plots
# theme_minimal() +
# # Turn off the legend
# guides(fill = "none") +
# # Add median values as text labels
# geom_text(data = f_stat, aes(x = 3.5, y = parameter, label = median_value), color = "black", hjust = .6)
### Point
# Add a line for 0 to show overlap of posterior
# geom_vline(xintercept = 0) +
#   # Remove background color from plots
#   theme_minimal() +
#   # Turn off the legend
#   guides(fill = "none") +
#   # Add median values as text labels
#   geom_text(data = f_stat, aes(x = 3.5, y = parameter, label = median_value), color = "black", hjust = .6) 