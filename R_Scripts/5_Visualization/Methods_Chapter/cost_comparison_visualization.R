#### Create Visualizations for Cost Analysis #####

library(tidyverse)
library(patchwork)
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")

# read in data
cost_dat <- read.csv("./Data/Cost_Analysis/Cost_Summary_Totals_v4.csv")

# Visualization 1: bar graph with year on x axis, cost on y, bar by organization for each method ####
v1_pam <- cost_dat %>% filter(method == "pam") %>% filter(! item == "hours") %>% group_by(org, year) %>% summarize(total = sum(value))

v1_pb <- cost_dat %>% filter(method == "pb") %>% filter(! item == "hours")%>% group_by(org, year) %>% summarize(total = sum(value))
plot1_pam <- ggplot(data = v1_pam) +
  geom_bar(aes(x = year, y = total, fill = org), position = "dodge", stat= "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("agency"=d_palette[5],
                               "univ_lab" = d_palette[8]),
                    labels = c("agency" = "Wildlife Agency",
                               "univ_lab" = "University Lab"),
                    name = "Organization") +
  theme(axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 13), 
        axis.title.x = element_text(size = 13),
        text = element_text(size = 13),
        legend.position = "bottom")+
  scale_y_continuous(limits = c(0,20000), breaks = c(0,5000,10000,15000,20000))+
  labs(title = "PAM",y = "Total (USD $)", x = "Project Year")

plot1_pb <- ggplot(data = v1_pb) +
  geom_bar(aes(x = year, y = total, fill = org), position = "dodge", stat= "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("agency"=pb_palette[5],
                               "univ_lab" = pb_palette[8]),
                    labels = c("agency" = "Wildlife Agency",
                               "univ_lab" = "University Lab"),
                    name = "Organization") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 13), 
        axis.title.x = element_text(size = 13),
        text = element_text(size = 13),
        legend.position = "bottom")+
  scale_y_continuous(limits = c(0,20000), breaks = c(0,5000,10000,15000,20000))+
  labs(title = "Playback", x = "Project Year")

#v1 <- cost_dat %>% group_by(org, method, year) %>% summarize(total = sum(value))
# #OLD: Try this with facet_wrap
# plot1 <- ggplot(data = v1) +
#   geom_bar(aes(x = year, y = total, fill = org), position = "dodge", stat= "identity") +
#   scale_fill_manual(values = c("agency"=palette_8[3],
#                                "univ_lab" = palette_8[8]),
#                     labels = c("agency" = "Wildlife Agency",
#                                "univ_lab" = "University Lab"),
#                     name = "Organization") +
#   theme_minimal() +
#   labs(y = "Total (USD $)", x = "Project Year") +  
#   facet_wrap(~ method, ncol = 2, labeller =  labeller(method = c("pam" = "PAM",
#                                                                  "pb" = "Playback")))+
#   theme(text = element_text(size = 15),
#         legend.position = "bottom")

# Visualization 2: each method across years regardless of organization ####
v2 <- cost_dat %>% group_by(method, year) %>% summarize(total = sum(value))
# Try this with facet_wrap
costperyear_fortalk <- ggplot(data = v2) +
  geom_bar(aes(x = year, y = total,fill = method), stat= "identity") +
  facet_wrap(~ method, ncol = 2, labeller = as_labeller(c("pb" = "Playback", "pam" = "PAM"))) +
  scale_fill_manual(values = c("pb"=pb_palette[4],
                               "pam" = d_palette[4]),
                    labels = c("pb" = "Playback",
                               "pam" = "PAM"),
                    guide = "none") +
   theme_minimal() +
   labs(y = "Total (USD)", x = "Year") +
  theme(text = element_text(size = 20))

# plot2 <- ggplot(data = v2) +
#   geom_bar(aes(x = year, y = total, fill = method), position = "dodge", stat= "identity") +
#   scale_fill_manual(values = c("pb"=pb_palette[4],
#                                "pam" = d_palette[4]),
#                     labels = c("pb" = "Playback",
#                                "pam" = "PAM")) +
#   theme_minimal() +
#   labs(y = "Total (USD $)", x = "Project Year") +
#   theme(text = element_text(size = 20))



# Visualization 3: components of each method
v3 <- cost_dat %>% filter(!item == "hours") %>% group_by(method, item) %>% summarize(total = sum(value))
# ggplot(data = v3) +
#   geom_bar(aes(x = item, y = total), fill = cuckoo_palette[1], stat = "identity") + 
#   labs(y = "Total (USD $)") + 
#   facet_wrap(~ method, nrow = 2)
# Trying another visualization - cost per site
sites_total <- 40
years <- 3
combined_sites <- sites_total * years
by_site <- v3 %>% group_by(method) %>% summarize(cost_persite = sum(total)/combined_sites)

# without facet wrapping
plot_items <- ggplot(data = v3) +
  geom_bar(aes(x = item, y = total, fill = method), position = "dodge", stat = "identity") + 
  scale_fill_manual(values = c("pb"=pb_palette[4],
                               "pam" = d_palette[3]),
                    labels = c("pb" = "Playback",
                               "pam" = "PAM"),
                    guide = "none") +
  theme_minimal()+
  labs(y = "Total (USD $)", x = NULL) +
  theme(text = element_text(size = 15))
# Create a version of this for the talk
plot_items_fortalk <- ggplot(data = v3) +
  geom_bar(aes(x = method, y = total, fill = item), position = "stack", stat = "identity") + 
  # use palette_8 3,5 ,6 
  scale_fill_manual(values = c("salary"=palette_8[3],
                               "supplies" = palette_8[5],
                               "transportation" = palette_8[6]),
                    labels = c("salary" = "Salary",
                               "supplies" = "Supplies",
                               "transportation" = "Transportation")) +
  scale_x_discrete(labels = c("pam" = "PAM", "pb" = "Playback"))+
  theme_minimal()+
  labs(y = "Total (USD $)", x = NULL, fill = "Cost Type") +
  theme(text = element_text(size = 20))


# Visualization 4: hours by method and year
v4 <- cost_dat %>% filter(item == "hours") %>% group_by(method, year) %>% summarize(total = sum(value))
plot_hours <- ggplot(data = v4) +
  geom_bar(aes(x = year, y = total, fill = method), position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("pb"=pb_palette[4],
                               "pam" = d_palette[3]),
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

# Why are these not high quality?????
#### Export graphics you want #####
final1 <- plot1_pam | plot1_pb
final1
ggsave("./Deliverables/MetChap_CostVisualizations/Cost_byOrgMethod_v4_11-21.jpeg", width=800, height=400)

# Combine hours and split apart costs into one
final2 <- plot_items | plot_hours
jpeg("./Deliverables/MetChap_CostVisualizations/HrsYear_CostSupplies_byMethod_v4_11-21.jpeg", width=800, height=400)
final2 
dev.off()

# Export image for thesis talk
final3 <- plot_items_fortalk
jpeg("./Deliverables/MetChap_CostVisualizations/CostSuppliesStacked_byMethod_1-27.jpeg", width=800, height=400)
final3
dev.off()

final4 <- costperyear_fortalk
# Trying to make the graphic cleaner - this didn't seem to make a difference
# ggsave("./Deliverables/MetChap_CostVisualizations/CostCombined_byYear_1-27.jpg", width=12, height=8)
jpeg("./Deliverables/MetChap_CostVisualizations/CostCombined_byYear_1-27.jpeg", width=800, height=400)
final4
dev.off()