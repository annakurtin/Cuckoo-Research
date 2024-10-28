##### Create Visualizations for Habitat Chapter Modelling Results #####
library(ggplot2)
library(patchwork)
library(ggdist)
library(jagsUI)

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




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make Density Plots #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Detection Model Density Plots
# Base palette around this color:
d_col <- "#FE6100"
d_palette <- c("#863300","#B44500","#EA5900","#FE6100","#FF8B43","#FFAF7D","#FFCBAB") 
chains <- jags_df(habdet)
# Select he chains for our covariates of interest
chains_viol <- chains %>% select(b1,b2,b3,b4,b4Q, b5)
# Rename them to be more interpretable
colnames(chains_viol) <- c("veg density","background noise","effort", "date","quadratic date","aru type")
# Pivot this longer so that we can visualize it
chains_viol_long <- chains_viol %>% pivot_longer(cols = c("veg density", "background noise", "effort", "date","quadratic date", "aru type"),names_to = "parameter", values_to = "values")

f_stat <- data.frame(
  parameter = c("veg density",
                "background noise",
                "effort","date", 
                "quadratic date",
                "aru type"),
  median_value = c(paste0("F: ", round(habdet$f$b1,2)), 
                   paste0("F: ",round(habdet$f$b2,2)), 
                   paste0("F: ",round(habdet$f$b3,2)), 
                   paste0("F: ",round(habdet$f$b4,2)), 
                   paste0("F: ",round(habdet$f$b4Q,2)),
                   paste0("F: ",round(habdet$f$b5,2)))
)

# Create the slab interval plot
ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval() +
  # Establish colors
  scale_fill_manual(values = c("veg density"=d_palette[6], 
                               "background noise" = d_palette[2],
                               "effort"=d_palette[4], 
                               "date" = d_palette[3], 
                               "quadratic date" = d_palette[5],
                               "aru type" = d_palette[1])) +
  # Add a line for 0 to show overlap of posterior
  geom_vline(xintercept = 0) +
  # Remove background color from plots
  theme_minimal() +
  # Turn off the legend
  guides(fill = FALSE)+
  # Add median values as text labels
  geom_text(data = f_stat, aes(x = 3.5, y = parameter, label = median_value), color = "black", hjust = .6) 



# Landscape Model Density Plots ####
# Use palette based around this color: 
lcol <- "#882255"
l_palette <- c("#751D49","#882255","#A32966","#CD3782","#D9659F","#E391BA","#EFBFD7")
# Plot posterior distribution
chains_bf2 <- jags_df(fit_B)
# Select the chains for our covariates of interest
chains_viol <- chains_bf2 %>% select(a1,a2)
# Rename them to be more interpretable
colnames(chains_viol) <- c("% Canopy Cover","% Shrub/Early Successional Cover")
# Pivot this longer so that we can visualize it
chains_viol_long <- chains_viol %>% pivot_longer(cols = c("% Canopy Cover","% Shrub/Early Successional Cover"),names_to = "parameter", values_to = "values")

f_stat <- data.frame(
  parameter = c("% Canopy Cover",
                "% Shrub/Early Successional Cover"),
  median_value = c(paste0("F: ",round(fit_B$f$a1,2)),
                   paste0("F: ",round(fit_B$f$a2,2)))
)

# Create the slab interval plot
ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval() +
  # Establish colors
  scale_fill_manual(values = c("% Canopy Cover"=l_palette[3], 
                               "% Shrub/Early Successional Cover" = l_palette[6])) +
  # Add a line for 0 to show overlap of posterior
  geom_vline(xintercept = 0) +
  # Remove background color from plots
  theme_minimal() +
  # Turn off the legend
  guides(fill = "none") +
  # Add median values as text labels
  geom_text(data = f_stat, aes(x = 3.5, y = parameter, label = median_value), color = "black", hjust = .6)



# Core Model Density Plots ####
# Base around this color:
ccol <- "#88CCEE"
c_palette <- c("#115475","#1A83B8","#24A1E0","#44AFE4","#88CCEE","#B9E1F5","#CCE9F8")
# Plot posterior distribution
chains_cf2 <- jags_df(fit_C)
# Select the chains for our covariates of interest
chains_viol <- chains_cf2 %>% select(a1,a2, a3, a4)
# Rename them to be more interpretable
colnames(chains_viol) <- c("% Shrub/Early Successional Cover","Canopy Height", "Shrub/Early Successional Height", "Veg SD Resid")
# Pivot this longer so that we can visualize it
chains_viol_long <- chains_viol %>% pivot_longer(cols = c("% Shrub/Early Successional Cover","Canopy Height", "Shrub/Early Successional Height", "Veg SD Resid"),names_to = "parameter", values_to = "values")

f_stat <- data.frame(
  parameter = c("% Shrub/Early Successional Cover",
                "Canopy Height", 
                "Shrub/Early Successional Height", 
                "Veg SD Resid"),
  median_value = c(paste0("F: ",round(fit_C$f$a1,2)),
                   paste0("F: ",round(fit_C$f$a2,2)),
                   paste0("F: ",round(fit_C$f$a3,2)), 
                   paste0("F: ",round(fit_C$f$a4,2)))
)

# Create the slab interval plot
ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval() +
  # Establish colors
  scale_fill_manual(values = c("% Shrub/Early Successional Cover"= c_palette[1], 
                               "Canopy Height" = c_palette[2], 
                               "Shrub/Early Successional Height" = c_palette[4], 
                               "Veg SD Resid" = c_palette[5])) +
  # Add a line for 0 to show overlap of posterior
  geom_vline(xintercept = 0) +
  # Remove background color from plots
  theme_minimal() +
  # Turn off the legend
  guides(fill = "none") +
  # Add median values as text labels
  geom_text(data = f_stat, aes(x = 3.5, y = parameter, label = median_value), color = "black", hjust = .6)



# Point Scale Density Plots #####
# Base around this color: 
pcol <- "#117733"
p_palette <- c("#0C5424","#117733","#169A42","#1DC956","#36E26F","#8DEFAE","#C0F6D2")
# Plot posterior distribution
chains_af1 <- jags_df(fit_A)
# Select the chains for our covariates of interest
chains_viol <- chains_af1 %>% select(a1,a2,a3,a4)
# Rename them to be more interpretable
colnames(chains_viol) <- c("Decid Spp Rich","Conif Spp Rich","Floodplain Shrub","Broadleaf Shrub")
# Pivot this longer so that we can visualize it
chains_viol_long <- chains_viol %>% pivot_longer(cols = c("Decid Spp Rich","Conif Spp Rich","Floodplain Shrub","Broadleaf Shrub"),names_to = "parameter", values_to = "values")

f_stat <- data.frame(
  parameter = c(
    "Decid Spp Rich", 
    "Conif Spp Rich", 
    "Floodplain Shrub", 
    "Broadleaf Shrub"),
  median_value = c(
    paste0("F:",round(fit_A$f$a1,2)),
    paste0("F:",round(fit_A$f$a2,2)),
    paste0("F:",round(fit_A$f$a3,2)),
    paste0("F:",round(fit_A$f$a4,2)))
)

# Create the slab interval plot
ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval() +
  # Establish colors
  scale_fill_manual(values = c("Decid Spp Rich"=p_palette[4], 
                               "Conif Spp Rich" = p_palette[3],
                               "Floodplain Shrub"=p_palette[6], 
                               "Broadleaf Shrub" = p_palette[1])) +
  # Add a line for 0 to show overlap of posterior
  geom_vline(xintercept = 0) +
  # Remove background color from plots
  theme_minimal() +
  # Turn off the legend
  guides(fill = "none") +
  # Add median values as text labels
  geom_text(data = f_stat, aes(x = 3.5, y = parameter, label = median_value), color = "black", hjust = .6) 
