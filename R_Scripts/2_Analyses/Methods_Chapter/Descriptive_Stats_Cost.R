#### Descriptive statistics for methods chapter

library(tidyverse)
library(patchwork)


# read in data
cost_dat <- read.csv("./Data/Cost_Analysis/Cost_Summary_Totals_v4.csv")

v3 <- cost_dat %>% filter(!item == "hours") %>% group_by(method, item) %>% summarize(total = sum(value))
v3 %>% group_by(method) %>% summarize(total_combined = sum(total))

sites_total <- 40
years <- 3
combined_sites <- sites_total * years
by_site <- v3 %>% group_by(method) %>% summarize(cost_persite = sum(total)/combined_sites)


