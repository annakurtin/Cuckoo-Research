##### Arrange Methods Chapter Plots ####
library(ggplot2)
library(patchwork)
source("./R_Scripts/5_Visualization/Methods_Chapter/CreatePlots_PosteriorDist_DetectionMods.R")
source("./R_Scripts/5_Visualization/Methods_Chapter/CreatePlots_PBDetProb_NumSurveys.R")
source("./R_Scripts/5_Visualization/Methods_Chapter/CreatePlots_PAMDetProb_NumSurveys.R")


# Arrange ARU plots
(dense_int_aru | dense_det_aru ) / aru_pnumsurveys
ggsave("./Deliverables/MetChap_ModelVisualizations/PAM_Posterior_PNumSurveys.jpg", width=12, height=8) # tried 10x6 but it didn't look as good

# Arrange PB plots
(dense_int_pb | dense_det_pb ) / pb_pnumsurveys
ggsave("./Deliverables/MetChap_ModelVisualizations/PB_Posterior_PNumSurveys.jpg", width=12, height=8)
# Why do these look so different? Why are the playback posterior distriutions so much thickers than the PAM posterior distributions?? 




## Trying out a different format
(dense_int_aru | dense_det_aru ) / (dense_int_pb | dense_det_pb )
ggsave("./Deliverables/MetChap_ModelVisualizations/PAM_PB_Posteriors.jpg", width=12, height=8)

aru_pnumsurveys/ pb_pnumsurveys
ggsave("./Deliverables/MetChap_ModelVisualizations/PAM_PB_NumSurveys.jpg", width=12, height=8)

pb_pnumsurveys
ggsave("./Deliverables/MetChap_ModelVisualizations/PB_NumSurveys.jpg", width=12, height=6)
