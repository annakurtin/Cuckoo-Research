#### Create Visualizations for Cost Analysis #####

library(tidyverse)
library(patchwork)
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")

# read in data
cost_dat <- read.csv("./Data/Cost_Analysis/Cost_Summary_Totals.csv")


# Visualization 1: bar graph with year on x axis, cost on y, bar by organization for each method ####
v1 <- cost_dat %>% group_by(org, method, year) %>% summarize(total = sum(value))
# Try this with facet_wrap
plot1 <- ggplot(data = v1) +
  geom_bar(aes(x = year, y = total, fill = org), position = "dodge", stat= "identity") +
  scale_fill_manual(values = c("agency"=palette_8[1],
                               "univ_lab" = palette_8[6]),
                    labels = c("agency" = "Wildlife Agency",
                               "univ_lab" = "University Lab"),
                    name = "Organization") +
  theme_minimal() +
  labs(y = "Total (USD)", x = "Project Year") +  
  facet_wrap(~ method, ncol = 2, labeller =  labeller(method = c("pam" = "PAM",
                                                                 "pb" = "Playback")))+
  theme(text = element_text(size = 15),
        legend.position = "bottom")
# I like this one better than the ones below
# I think I like facet wrap by columns rather than by rows

# old graphics for v1 ####
v1_pam <- cost_dat %>% filter(method == "pam") %>% group_by(org, year) %>% summarize(total = sum(value))
v1_pb <- cost_dat %>% filter(method == "pb") %>% group_by(org, year) %>% summarize(total = sum(value))
ggplot(data = v1_pam) +
  geom_bar(aes(x = year, y = total, fill = org), position = "dodge", stat= "identity") +
  scale_fill_manual(values = c("agency"=palette_8[1],
                               "univ_lab" = palette_8[6])) +
  theme_minimal() +
  labs(title = "PAM Costs")

ggplot(data = v1_pb) +
  geom_bar(aes(x = year, y = total, fill = org), position = "dodge", stat= "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("agency"=palette_8[1],
                               "univ_lab" = palette_8[6])) +
  labs(title = "Playback Costs")




# Visualization 2: each method across years regardless of organization ####
v2 <- cost_dat %>% group_by(method, year) %>% summarize(total = sum(value))
# Try this with facet_wrap
# ggplot(data = v2) +
#   geom_bar(aes(x = year, y = total), fill = cuckoo_palette[1], stat= "identity") +
#   theme_minimal() +
#   labs(y = "Total (USD)") + 
#   facet_wrap(~ method, ncol = 2)

plot2 <- ggplot(data = v2) +
  geom_bar(aes(x = year, y = total, fill = method), position = "dodge", stat= "identity") +
  scale_fill_manual(values = c("pb"=palette_8[3],
                               "pam" = palette_8[2]),
                    labels = c("pb" = "Playback",
                               "pam" = "PAM")) +
  theme_minimal() +
  labs(y = "Total (USD)", x = "Project Year") 



# Visualization 3: components of each method
v3 <- cost_dat %>% filter(!item == "hours") %>% group_by(method, item) %>% summarize(total = sum(value))
ggplot(data = v3) +
  geom_bar(aes(x = item, y = total), fill = cuckoo_palette[1], stat = "identity") + 
  labs(y = "Total (USD)") + 
  facet_wrap(~ method, nrow = 2)

# without facet wrapping
plot_items <- ggplot(data = v3) +
  geom_bar(aes(x = item, y = total, fill = method), position = "dodge", stat = "identity") + 
  scale_fill_manual(values = c("pb"=palette_8[3],
                               "pam" = palette_8[2]),
                    labels = c("pb" = "Playback",
                               "pam" = "PAM"),
                    guide = "none") +
  theme_minimal()+
  labs(y = "Total (USD)", x = NULL) +
  theme(text = element_text(size = 15))



# Visualization 4: hours by method and year
v4 <- cost_dat %>% filter(item == "hours") %>% group_by(method, year) %>% summarize(total = sum(value))
plot_hours <- ggplot(data = v4) +
  geom_bar(aes(x = year, y = total, fill = method), position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("pb"=palette_8[3],
                               "pam" = palette_8[2]),
                    labels = c("pb" = "Playback",
                               "pam" = "PAM"),
                    name = "Method") +
  theme_minimal() +
  labs(y = "Personnel Hours", x = "Project Year")  +
  theme(text = element_text(size = 15),
        legend.position = "bottom",
        legend.justification = "left")


# TO look at palettes
# "#332288" "#117733" "#44AA99" "#88CCEE" "#DDCC77" "#CC6677" "#AA4499" "#882255"


#### Export graphics you want #####

jpeg("./Deliverables/HabChap_CostVisualizations/Cost_byOrgMethod2.jpeg", width=600, height=400)
plot1
dev.off()
# Combine hours and split apart costs into one
final2 <- plot_items | plot_hours
jpeg("./Deliverables/HabChap_CostVisualizations/HrsYear_CostSupplies_byMethod.jpeg", width=600, height=400)
final2 
dev.off()
