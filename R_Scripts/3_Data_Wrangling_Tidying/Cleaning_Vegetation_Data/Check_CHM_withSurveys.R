#### Check for Differences in CHM ###################

## Purpose: read in the cleaned survey 123 data and check

# Created 7/2/2024

# Last modified: 7/2/2024


#### Setup #################################
packages <- c("tidyverse","janitor")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_SiteColumn_fromPointID.R")
load_packages(packages)


#### Code #################################
# Read in survey data
veg <- read.csv("./Data/Vegetation_Data/Outputs/2023_VegSurvey_MainData_Cleaned6-19.csv")
mean(veg$canopy_height, na.rm = TRUE)
max(veg$canopy_height, na.rm = TRUE)

# Filter out points that are in Treasure County
treasure <- veg %>% filter(point_id %in% c("ISA-1","ISA-2","ISA-3","AME-1","AME-2","AME-3","PFA-1","PFA-2","PFA-3"))
treasure_mean <- mean(treasure$canopy_height, na.rm = TRUE)
treasure_max <- max(treasure$canopy_height, na.rm = TRUE)

# Look at distribution for Blaine county
blaine <- veg %>% filter(point_id %in% c("MISO-174","MISO-089","MISO-123","MISO-125","MISO-057","MISO-097","MISO-090","MISO-016","MISO-015","MISO-094","MISO-117","MISO-181","MISO-022","MISO-065","MISO-188","MISO-196","MISO-017","MISO-191","MISO-177","MISO-171","MISO-197","MISO-203","MISO-064","MISO-091","MISO-206","MISO-013"))
blaine_mean <- mean(blaine$canopy_height, na.rm = TRUE)
blaine_max <- max(blaine$canopy_height, na.rm = TRUE)

# Valley county
valley <- veg %>% filter(point_id %in% c("MISO-024","MISO-099","MISO-146","MISO-025","MISO-032","MISO-204","MISO-116","MISO-077","MISO-069","MISO-163","MISO-086","MISO-150","MISO-202","MISO-105","MISO-009"))
valley_mean <- mean(valley$canopy_height, na.rm = TRUE)
valley_max <- max(valley$canopy_height, na.rm = TRUE)


yrich <- veg %>% filter(point_id %in% c("ELI-1","ELI-2","ELI-3","XSS-1","XSS-2","XSS-3","XFI-1","XFI-2","XFI-3","SID-1","SID-2","SID-3"))
yrich_mean <- mean(yrich$canopy_height, na.rm = TRUE)
yrich_max <- max(yrich$canopy_height, na.rm = TRUE)


dawson <- veg %>% filter(point_id %in% c("HOL-1","HOL-2","HOL-3","YELL-100","YELL-050","STI-1","STI-2","STI-3","YELL-030","YELL-032","YELL-090","YELL-125"))
dawson_mean <- mean(dawson$canopy_height, na.rm = TRUE)
dawson_max <- max(dawson$canopy_height, na.rm = TRUE)

wheat <- veg %>% filter(point_id %in% c("MUSH-131","MUSH-023","MUSH-202","MUSH-216","MUSH-184","MUSH-060","MUSH-011","MUSH-124","MUSH-171","MUSH-183","MUSH-169"))
wheat_mean <- mean(wheat$canopy_height, na.rm = TRUE)
wheat_max <- max(wheat$canopy_height, na.rm = TRUE)

# Look at the points in Richland Yellowstone area
avg_height <- data.frame(counties = c("treasure","treasure",
                                      "blaine","blaine"), 
                         values = c(treasure_mean, treasure_max, 
                                    blaine_mean, blaine_max), 
                         data = c("mean","max",
                                  "mean","max"))


