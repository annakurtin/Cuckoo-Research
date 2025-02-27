##### Create Visualizations for Habitat Chapter Modelling Results #####
library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggdist)
library(jagsUI)
library(jagshelper)
source("./R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")
# Making combined density plots for each spatial scale for the thesis defense 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in models #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Detection model
#habdet <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Detection_Submodel/Models_Ran_Outputs/Actual_Data/DetSubModel_Final4.Rdata")

# Landscape Scale Model 
fit_B <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Occupancy_Models/Models_Ran_Outputs/StateSubModel_BFinal3.Rdata")

# Core Scale Model
fit_C <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Occupancy_Models/Models_Ran_Outputs/StateSubModel_CFinal3.Rdata")

# Point Scale Model
fit_A <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Occupancy_Models/Models_Ran_Outputs/StateSubModel_AFinal3.Rdata")

# Linear model
fit_gpois <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Habitat_Chapter/Poisson_Model/Models_Ran_Outputs/LMod15_GamPois_14Day.Rdata")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine chains and prep for visualization #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Landscape Model
chains_bf2 <- jags_df(fit_B)
# Select the chains for our covariates of interest
chains_viol_bf1 <- chains_bf2 %>% select(a1,a2)
# Rename them to be more interpretable
colnames(chains_viol_bf1) <- c("% Canopy Cover","% Open Shrub (Landscape)")
# Core Model
chains_cf2 <- jags_df(fit_C)
# Select the chains for our covariates of interest
chains_viol_cf2 <- chains_cf2 %>% select(a1,a2, a3, a4)
# Rename them to be more interpretable
colnames(chains_viol_cf2) <- c("% Open Shrub (Core Area)","Canopy Height", "Open Shrub Height", "Vegetation Complexity")
# Point Model
chains_af1 <- jags_df(fit_A)
# Select the chains for our covariates of interest
chains_viol_af1 <- chains_af1 %>% select(a1,a2,a3,a4)
# Rename them to be more interpretable
colnames(chains_viol_af1) <- c("Deciduous Tree Richness","Conifer Tree Richness","Floodplain Shrub","Broadleaf Shrub")

# Combine all of the chains_viol data - should be able to cbind them all together 
all_chains_viol <- cbind(chains_viol_bf1,chains_viol_cf2,chains_viol_af1)


# Pivot this longer so that we can visualize it
chains_viol_long <- all_chains_viol %>% pivot_longer(cols = c("% Canopy Cover",
                                                          "% Open Shrub (Landscape)", 
                                                          "% Open Shrub (Core Area)",
                                                          "Canopy Height", 
                                                          "Open Shrub Height", 
                                                          "Vegetation Complexity",
                                                          "Deciduous Tree Richness",
                                                          "Conifer Tree Richness",
                                                          "Floodplain Shrub",
                                                          "Broadleaf Shrub"),names_to = "parameter", values_to = "values")
# Specify the desired order of the y-axis values
chains_viol_long$parameter <- factor(chains_viol_long$parameter, 
                                     levels = c("% Open Shrub (Landscape)",
                                                "% Canopy Cover",
                                                "% Open Shrub (Core Area)",
                                                "Canopy Height", 
                                                "Open Shrub Height", 
                                                "Vegetation Complexity",
                                                "Floodplain Shrub",
                                                "Broadleaf Shrub",
                                                "Conifer Tree Richness",
                                                "Deciduous Tree Richness"))

# Extract median F statistics
f_stat <- data.frame(
  parameter = c("% Canopy Cover",
                "% Open Shrub (Landscape)", 
                "% Open Shrub (Core Area)",
                "Canopy Height", 
                "Open Shrub Height", 
                "Vegetation Complexity",
                "Deciduous Tree Richness",
                "Conifer Tree Richness",
                "Floodplain Shrub",
                "Broadleaf Shrub"),
  median_value = c(round(fit_B$f$a1,2),
                   round(fit_B$f$a2,2),
                   round(fit_C$f$a1,2),
                   round(fit_C$f$a2,2),
                   round(fit_C$f$a3,2), 
                   round(fit_C$f$a4,2),
                   round(fit_A$f$a1,2),
                   round(fit_A$f$a2,2),
                   round(fit_A$f$a3,2),
                   round(fit_A$f$a4,2))                   
  )



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make Density Plot #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create the slab interval plot
dense_occ <- ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval(height = 1.5) +
  # Establish colors
  scale_fill_manual(values = c("% Canopy Cover"=l_palette[7], 
                               "% Open Shrub (Landscape)" = l_palette[5],
                                "% Open Shrub (Core Area)"= c_palette[1], 
                                "Canopy Height" = c_palette[3], 
                                "Open Shrub Height" = c_palette[4], 
                                "Vegetation Complexity" = c_palette[6],
                               "Deciduous Tree Richness"=p_palette[9], 
                               "Conifer Tree Richness" = p_palette[7],
                               "Floodplain Shrub"=p_palette[3], 
                               "Broadleaf Shrub" = p_palette[5])) +
  # Remove background color from plots
  theme_minimal() +
  # Adjust axis titles
  labs(y = NULL, x = "Posterior Estimate") +
  # Adjust axis labels
  theme(axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15), hjust = 1,
        axis.title.x = element_text(size = 15)) +
  # Adjust x axis
  scale_x_continuous(limits = c(-3.5,3), breaks = seq(-3,2, by = 1)) +
  # Add a line for 0 to show overlap of posterior
  geom_vline(xintercept = 0, color = "gray12", linetype = "dashed") +
  # Add median values as text labels
  geom_label(data = f_stat, 
             aes(x = 2.75, y = parameter, label = median_value),
             size = 6, color = "black", 
             hjust = .6, label.size = 0) +
  annotate("label", x = 2.65, y = 10.85, label = "F Statistic:", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  # Turn off the legend
  guides(fill = FALSE)

# Save this
dense_occ
ggsave("./Deliverables/HabChap_ModelVisualizations/DensityPlot_AllOccMods.jpg", width=8, height=6)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Linear Model #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

chains_gpois <- jags_df(fit_gpois)
# Select the chains for our covariates of interest
chains_viol <- chains_gpois %>% select(beta1,beta2,beta3,beta4, beta5, beta6, beta7, beta8)
# Rename them to be more interpretable
colnames(chains_viol) <- c("% Canopy Cover",
                           "% Open Shrub (Landscape)",
                           "Vegetation Complexity",
                           "% Open Shrub (Core Area)",
                           "Conifer Tree Richness",
                           "Recording Days",
                           "Background Noise",
                           "Vegetation Density")
# Pivot this longer so that we can visualize it
chains_viol_long <- chains_viol %>% pivot_longer(cols = c("% Canopy Cover",
                                                          "% Open Shrub (Landscape)",
                                                          "Vegetation Complexity",
                                                          "% Open Shrub (Core Area)",
                                                          "Conifer Tree Richness",
                                                          "Recording Days",
                                                          "Background Noise",
                                                          "Vegetation Density"),names_to = "parameter", values_to = "values")
# Specify the desired order of the y-axis values
chains_viol_long$parameter <- factor(chains_viol_long$parameter, 
                                     levels = c("% Canopy Cover",
                                                "% Open Shrub (Landscape)",
                                                "Vegetation Complexity",
                                                "% Open Shrub (Core Area)",
                                                "Conifer Tree Richness",
                                                "Recording Days",
                                                "Background Noise",
                                                "Vegetation Density"))

f_stat <- data.frame(
  parameter = c(
    "% Canopy Cover",
    "% Open Shrub (Landscape)",
    "Vegetation Complexity",
    "% Open Shrub (Core Area)",
    "Conifer Tree Richness",
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
  scale_fill_manual(values = c("% Canopy Cover"=l_palette[7], 
                               "% Open Shrub (Landscape)" = l_palette[5],
                               "Vegetation Complexity"=c_palette[6], 
                               "% Open Shrub (Core Area)" = c_palette[1],
                               "Conifer Tree Richness" = p_palette[7],
                               "Recording Days" = d_palette[6],
                               "Background Noise" = d_palette[4],
                               "Vegetation Density" = d_palette[8])) + 
  # Remove background color from plots
  theme_minimal() +
  # Adjust axis titles
  labs(y = NULL, x = "Posterior Estimate") +
  # Adjust axis labels
  theme(axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20), hjust = 1,
        axis.title.x = element_text(size = 20)) +
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
  guides(fill = "none")


# Save this
dense_lm
ggsave("./Deliverables/HabChap_ModelVisualizations/DensityPlot_LinMod_forTalk.jpg", width=8, height=6)
