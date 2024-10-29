##### Create Visualizations for Habitat Chapter Modelling Results #####
library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggdist)
library(jagsUI)
library(jagshelper)

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




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make Density Plots #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Detection Model Density Plots
# Base palette around this color:
d_col <- "#FE6100"
d_palette <- c("#863300","#B44500","#EA5900","#FE6100","#FF8B43","#FFAF7D","#FFCBAB") 
dp_all <- colorRampPalette(c("#863300","#EA5900","#FFCBAB"))
d_palette <- dp_all(10)
#d_palette <- "#863300" "#9C3B00" "#B24300" "#C84C00" "#DE5400" "#EC6513" "#F17E38" "#F5985E" "#FAB184" "#FFCBAB"

chains <- jags_df(habdet)
# Select he chains for our covariates of interest
chains_viol <- chains %>% select(b1,b2,b3,b4,b4Q, b5)
# Rename them to be more interpretable
colnames(chains_viol) <- c("Veg Density","Noise","Effort", "Date","Date*Date","SM Present")
# Pivot this longer so that we can visualize it
chains_viol_long <- chains_viol %>% pivot_longer(cols = c("Veg Density","Noise","Effort", "Date","Date*Date","SM Present"),names_to = "parameter", values_to = "values")
# Specify the desired order of the y-axis values
chains_viol_long$parameter <- factor(chains_viol_long$parameter, 
                                     levels = c("Date",
                                                "Date*Date",
                                                "Noise",
                                                "Effort",
                                                "Veg Density",
                                                "SM Present"))

f_stat <- data.frame(
  parameter = c("Veg Density","Noise","Effort", "Date","Date*Date","SM Present"),
  median_value = c(round(habdet$f$b1,2), 
                   round(habdet$f$b2,2), 
                   round(habdet$f$b3,2), 
                   round(habdet$f$b4,2), 
                   round(habdet$f$b4Q,2),
                   round(habdet$f$b5,2))
)

# Create the slab interval plot
ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval(height = 1.5) +
  # Establish colors
  scale_fill_manual(values = c("Veg Density"=d_palette[8], 
                               "Noise" = d_palette[4],
                               "Effort"=d_palette[6], 
                               "Date" = d_palette[1], 
                               "Date*Date" = d_palette[3],
                               "SM Present" = d_palette[10])) +
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
# Use palette based around this color: 
lcol <- "#882255"
l_palette <- c("#751D49","#882255","#A32966","#CD3782","#D9659F","#E391BA","#EFBFD7")
lp_all <- colorRampPalette(c("#531534","#882255","#EFBFD7"))
l_palette <- lp_all(10)
#t <- "#531534" "#5E173B" "#6A1A42" "#761D49" "#822051" "#933363" "#AA5680" "#C1799D" "#D89CBA" "#EFBFD7"
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
  median_value = c(paste0("F: ",round(fit_B$f$a1,2)),
                   paste0("F: ",round(fit_B$f$a2,2)))
)

# Create the slab interval plot
ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
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
# Base around this color:
ccol <- "#88CCEE"
c_palette <- c("#115475","#1A83B8","#24A1E0","#44AFE4","#88CCEE","#B9E1F5","#CCE9F8")
cp_all <- colorRampPalette(c("#115475","#88CCEE","#CCE9F8"))
c_palette <- cp_all(10)
#c <- "#115475" "#2B6E8F" "#4589AA" "#60A3C5" "#7ABEE0" "#8FCFEF" "#9ED5F1" "#ADDCF3" "#BCE2F5" "#CCE9F8"
# Plot posterior distribution
chains_cf2 <- jags_df(fit_C)
# Select the chains for our covariates of interest
chains_viol <- chains_cf2 %>% select(a1,a2, a3, a4)
# Rename them to be more interpretable
colnames(chains_viol) <- c("% SES Cover","Canopy Height", "SES Height", "Veg Complexity")
# Pivot this longer so that we can visualize it
chains_viol_long <- chains_viol %>% pivot_longer(cols = c("% SES Cover","Canopy Height", "SES Height", "Veg Complexity"),names_to = "parameter", values_to = "values")
# Specify the desired order of the y-axis values
chains_viol_long$parameter <- factor(chains_viol_long$parameter, 
                                     levels = c("% SES Cover",
                                                "Canopy Height", 
                                                "SES Height", 
                                                "Veg Complexity"))

f_stat <- data.frame(
  parameter = c("% SES Cover",
                "Canopy Height", 
                "SES Height", 
                "Veg Complexity"),
  median_value = c(paste0("F: ",round(fit_C$f$a1,2)),
                   paste0("F: ",round(fit_C$f$a2,2)),
                   paste0("F: ",round(fit_C$f$a3,2)), 
                   paste0("F: ",round(fit_C$f$a4,2)))
)

# Create the slab interval plot
ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval(height = 1.5) +
  # Establish colors
  scale_fill_manual(values = c("% SES Cover"= c_palette[1], 
                               "Canopy Height" = c_palette[3], 
                               "SES Height" = c_palette[4], 
                               "Veg Complexity" = c_palette[6])) +
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
# Base around this color: 
pcol <- "#117733"
p_palette <- c("#0C5424","#117733","#169A42","#1DC956","#36E26F","#8DEFAE","#C0F6D2")
pp_all <- colorRampPalette(c("#0C5424","#169A42","#C0F6D2"))
p_palette <- pp_all(10)
#p <- "#0C5424" "#0E632A" "#107331" "#128238" "#14923E" "#28A452" "#4EB871" "#74CD92" "#9AE1B2" "#C0F6D2"
# Plot posterior distribution
chains_af1 <- jags_df(fit_A)
# Select the chains for our covariates of interest
chains_viol <- chains_af1 %>% select(a1,a2,a3,a4)
# Rename them to be more interpretable
colnames(chains_viol) <- c("Decid Spp Rich","Conif Spp Rich","Floodplain Shrub","Broadleaf Shrub")
# Pivot this longer so that we can visualize it
chains_viol_long <- chains_viol %>% pivot_longer(cols = c("Decid Spp Rich","Conif Spp Rich","Floodplain Shrub","Broadleaf Shrub"),names_to = "parameter", values_to = "values")
# Specify the desired order of the y-axis values
chains_viol_long$parameter <- factor(chains_viol_long$parameter, 
                                     levels = c("Floodplain Shrub",
                                                "Broadleaf Shrub",
                                                "Conif Spp Rich",
                                                "Decid Spp Rich"))

f_stat <- data.frame(
  parameter = c(
    "Decid Spp Rich", 
    "Conif Spp Rich", 
    "Floodplain Shrub", 
    "Broadleaf Shrub"),
  median_value = c(
    round(fit_A$f$a1,2),
    round(fit_A$f$a2,2),
    round(fit_A$f$a3,2),
    round(fit_A$f$a4,2))
)

# Create the slab interval plot
ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval(height = 1.5) +
  # Establish colors
  scale_fill_manual(values = c("Decid Spp Rich"=p_palette[9], 
                               "Conif Spp Rich" = p_palette[7],
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