##### Create Plots of Detection Prob and Number of Surveys for PAM Surveys ####

library(tidyverse)
library(vioplot)
library(ggplot2)
library(ggdist)
library(corrgram)
library(reshape2)
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")


# Read in models and data
fit_pb <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Methods_Chapter/Playback_Model/Models_Ran_Outputs/JAGS_PBMod2.txt")
pb_dat <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Playback_Results/2023/Outputs/2023_PBData_FormatforOccModSCALED_8-6-24.csv")

fit_pam <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Methods_Chapter/ARU_Model/Models_Ran_Outputs/MetChap_ARUMod4.Rdata")
pam_dat <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/HabChap_DetOccCovsFull_SCALED_8-12.csv")

# Detection probability for a single playback survey:
p_pb_all <- plogis(fit_pb$q50$b0 + fit_pb$q50$b1*0 + fit_pb$q50$b1Q*0 + fit_pb$q50$b3*0 + fit_pb$q50$b4*0 )
# Cumulative detection probability
# calculate cumulative detection probability
n_pb <- 3
pb_p_star <- round(1 - (1 - p_pb_all)^n_pb, 4)



#### Figure 2: Cumulative Detection Probability for ARU ####
nrow_pam <- fit_pam$mcmc.info$n.samples
max_surveys <- 10
pam_pstars <- array(NA, dim = c(nrow_pam, max_surveys))

# Set up x values with the same number of rows as your array
# x values will allow plotting of box values
x <-  cbind(rep(1, nrow_pam), rep(2, nrow_pam), rep(3, nrow_pam), 
            rep(4, nrow_pam), rep(5, nrow_pam), rep(6, nrow_pam), 
            rep(7, nrow_pam), rep(8, nrow_pam), rep(9, nrow_pam), 
            rep(10, nrow_pam))

#Set up a for-loop that will run the same amount of iterations as the samples produced in the MCMC
for (i in 1:nrow_pam) { 
  # fills in data for each row 
  for (j in 1:max_surveys){ 
    # Fills in data for each column (i.e. number of columns)
    pam_pstars[i,j] <- 1 - (1 - plogis(fit_pam$sims.list$b0[i] + fit_pam$sims.list$b1[i]*0 + fit_pam$sims.list$b1Q[i]*0 + fit_pam$sims.list$b3[i]*0 + fit_pam$sims.list$b4[i]*0 + fit_pam$sims.list$b5[i]*0 + fit_pam$sims.list$b6[i]*0))^j #Calculate estimated maximum detection probability for each survey using mean probability calculated in the MCMC 
  } 
}

pam_pstars_long <- melt(pam_pstars)
colnames(pam_pstars_long) <- c("mcmc_samp", "num_surveys", "cumulative_p")
# Convert Surveys from numeric to factor for ggplot2
pam_pstars_long$num_surveys <- as.factor(pam_pstars_long$num_surveys)


# Create the violin plot using ggplot2
aru_pnumsurveys <- ggplot(data = pam_pstars_long, aes(x = num_surveys, y = cumulative_p)) + 
  geom_violin(fill = d_palette[8]) +  # Violin plot with your custom color
  geom_hline(yintercept = pb_p_star, linetype = "dashed", size = 1.5, color = pb_palette[3]) +  # Dashed line for pstar
  labs(x = "Number of Surveys", y = "Cumulative Detection Probability") + 
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) + # Adjust text angle and position
  stat_summary(fun.y="median", geom="point", color = d_palette[1], size = 5) +
  annotate("label", x = 9.5, y = 0.1, label = "C", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  # Adjust axis labels
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))

#aru_pnumsurveys