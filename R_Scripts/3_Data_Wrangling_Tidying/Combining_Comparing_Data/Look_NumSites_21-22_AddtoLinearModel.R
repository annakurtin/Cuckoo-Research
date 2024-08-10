### Look at how many sites adding 2022 and 2021 data would give you
library(tidyverse)
source("./R_Scripts/6_Function_Scripts/Create_Site_SamplingColumn_fromPointID.R")
dat22 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/SiteLevelBBCU_2022.csv")
dat22 <- dat22 %>% create_site_col()
dat22 <- dat22 %>% group_by(site_id) %>% summarize(site_det = max(bbcu))
sum(dat22$site_det) # 12 more sites


#look at 2021
dat21 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2021_AllCollab_topclips_filteredPBYBCU_5-27.csv")
dat21 <- dat21 %>% group_by(site_id) %>% summarize(site_det = max(annotation))
sum(dat21$site_det)

# Adding these two years would add on 14 sites
# I think it might be worth it to add on the 2022 data but not the 2021 data, since there are only two sites that are added