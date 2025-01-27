##### Arrange Methods Chapter Plots ####
# Copied over from the manuscript one 1-27-25
library(ggplot2)
library(patchwork)
source("./R_Scripts/5_Visualization/Methods_Chapter/CreatePlots_PosteriorDist_DetectionMods_Talk.R")
source("./R_Scripts/5_Visualization/Methods_Chapter/CreatePlots_PBDetProb_NumSurveys_Talk.R")
#source("./R_Scripts/5_Visualization/Methods_Chapter/CreatePlots_PAMDetProb_NumSurveys_Talk.R")

# Also haven't used these in talks yet
# # Arrange ARU plots
# (dense_int_aru | dense_det_aru ) / aru_pnumsurveys
# ggsave("./Deliverables/MetChap_ModelVisualizations/Edited_forTalks/PAM_Posterior_PNumSurveys.jpg", width=12, height=8) # tried 10x6 but it didn't look as good
# 
# # Arrange PB plots
# (dense_int_pb | dense_det_pb ) / pb_pnumsurveys
# ggsave("./Deliverables/MetChap_ModelVisualizations/Edited_forTalks/PB_Posterior_PNumSurveys.jpg", width=12, height=8)




## Didn't actually change these for the talk
# (dense_int_aru | dense_det_aru ) / (dense_int_pb | dense_det_pb )
# ggsave("./Deliverables/MetChap_ModelVisualizations/Edited_forTalks/PAM_PB_Posteriors.jpg", width=12, height=8)

# aru_pnumsurveys/ pb_pnumsurveys
# ggsave("./Deliverables/MetChap_ModelVisualizations/Edited_forTalks/PAM_PB_NumSurveys.jpg", width=12, height=8)

# Changed this one for the talk
pb_pnumsurveys_empty <- ggplot() + 
  # Dashed line for pstar
  geom_hline(yintercept = aru_p_star, linetype = "dashed", linewidth = 1.5, color = d_palette[3]) +
  # Add axis labels
  labs(x = "Number of Surveys", y = "Cumulative Detection Probability") + 
  # Minimal theme
  theme_minimal() +
  # Add annotation
  annotate("label", x = 9.5, y = 0.1, label = "", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  # Scale x-axis from 1 to 10 by 1
  scale_x_continuous(limits = c(1, 10), breaks = 1:10) +
  # Adjust axis text and labels
  theme(
    axis.text.x = element_text(size = 20), 
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "gray", linewidth = 0.25)
  )
pb_pnumsurveys_empty
ggsave("./Deliverables/MetChap_ModelVisualizations/Edited_forTalks/PB_NumSurveys_BLANK.jpg", width=12, height=6)


# Copied over text to create the plot
pb_pnumsurveys <- ggplot(data = pb_pstars_long, aes(x = num_surveys, y = cumulative_p)) + 
  geom_violin(fill = pb_palette[8]) +  # Violin plot with your custom color
  geom_hline(yintercept = aru_p_star, linetype = "dashed", linewidth = 1.5, color = d_palette[3]) +  # Dashed line for pstar
  labs(x = "Number of Surveys", y = "Cumulative Detection Probability") + 
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) + # Adjust text angle and position
  stat_summary(fun.y="median", geom="point", color = pb_palette[1], size = 5) +
  annotate("label", x = 9.5, y = 0.1, label = "", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  # Adjust axis labels
  theme(axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        panel.grid.major.x = element_line(color = "gray", linewidth = 0.25))

pb_pnumsurveys
ggsave("./Deliverables/MetChap_ModelVisualizations/Edited_forTalks/PB_NumSurveys.jpg", width=12, height=6)
