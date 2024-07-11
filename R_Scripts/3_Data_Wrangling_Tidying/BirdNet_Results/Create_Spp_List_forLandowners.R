#### Create Species List form BirdNet Results for Landowners ####

# Created 7/11/2024
# Last modified 7/11/2024
library(tidyverse)
library(janitor)
source("./R_Scripts/6_Function_Scripts/Combine_CSV_Files_In_Directory.R")
##### Read in Data, combine CSVs#####
duncan <- combine_csv_files("./Data/ARU_BirdNet_SppList/DuncanRanch") %>% clean_names()
duncan_list <- unique(duncan$common_name)
# remove great egret, lesser goldfinch, nashville warbler, pacific wren, purple finch, great tailed grackle, blue grosbeak
duncan_rm <- c("Great Egret", "Lesser Goldfinch", "Nashville Warbler", "Pacific Wren", "Purple Finch", "Great-tailed Grackle", "Blue Grosbeak")
duncan_list <- setdiff(duncan_list, duncan_rm)
write.csv(duncan_list,"./Data/ARU_BirdNet_SppList/DuncanRanch_2023SppList.csv",row.names = FALSE)


hazel <- combine_csv_files("./Data/ARU_BirdNet_SppList/Hazelwood") %>% clean_names()
hazel_list <- hazel %>% group_by(year) %>% summarize(common_name = unique(common_name))
write.csv(hazel_list, "./Data/ARU_BirdNet_SppList/Hazelwood_2021-2023SppList.csv", row.names = FALSE)