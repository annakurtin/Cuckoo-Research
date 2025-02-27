## Looking at when calls were detected at sites
library(tidyverse)

# read in 2023 data
dets23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_5-27.csv")
yell150 <- dets23 %>% filter(point_id == "YELL-150", annotation == 1)
miso_202 <- dets23 %>% filter(point_id == "MISO-202", annotation == 1)
