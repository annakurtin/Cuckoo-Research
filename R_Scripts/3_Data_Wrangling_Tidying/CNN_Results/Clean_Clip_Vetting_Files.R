### Clean Clip Vetting Output ####

# Script created 3/20/2023
# Script last edited 3/20/2023

#### Setup#####
packages <- c("data.table","tidyverse","janitor")
# Read in the packages function
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
# Load packages
load_packages(packages)


#### Code#####

# To do:
# Mask out all of the dates that align with the dates of the playback
# Some of the clips that have playback audio in them (changed to 0):
# JDO-2 20230706_070000 185-190
# SNO-3/20230714_90000_1525s-1530s_cadence_coo.wav
# CUL-2/20230715_70000_1795s-1800s_cadence_coo.wav
# ELI-2/20230705_90000_370.0s-375.0s_cadence_coo.wav Could have been the playbacks but I left this in as a positive
# ELI-1/20230627_90000_1635.0s-1640.0s_cadence_coo.wav changed to 0
# JUD-3/20230706_90000_120s-125s_cadence_coo.wav changed to 0
# 83-3/20230614_70000_375s-380s_cadence_coo.wav LEFT AS 1, though this sounds like it likely was the playback audio. Playbacks started at 5:47. Mask out files within 2 hrs of start of playbacks?
# 83-3/20230621_70000_1160s-1165s_cadence_coo.wav changed to 0
# 83-2/20230621_70000_1245s-1250s_cadence_coo.wav changed to 0
# 83-1/20230614_70000_490s-495s_cadence_coo.wav changed to 0
# 82-4/20230614_90000_570s-575s_cadence_coo.wav changed to 0
# 82-4/20230622_70000_240s-245s_cadence_coo.wav LEFT AS 1, sounds like distant playback but too far away to confirm
# 82-3/20230614_90000_1780s-1785s_cadence_coo.wav changed to 0
# 82-3/20230615_70000_1710s-1715s_cadence_coo.wav LEFT AS 1, likely playback audio but too far away to say for sure
# 82-3/20230622_70000_310s-315s_cadence_coo.wav changed to 0
# 82-1/20230615_70000_1570s-1575s_cadence_coo.wav LEFT AS 1, likely playback audio but too far away to say for sure
# 82-1/20230621_70000_1165s-1170s_cadence_coo.wav LEFT AS 1, sounds like PB audio but no PB conducted at site 82 on this date (yes on 84 and 83), not close to other points (unlikely that this was a playback at another point that we hear)
# 82-1/20230622_70000_110s-115s_cadence_coo.wav LEFT AS 1, potentially a playback but audio too distant to tell
# 203-3/20230620_90000_1660s-1665s_cadence_coo.wav changed to 0
# 203-2/20230615_90000_1545s-1550s_cadence_coo.wav changed to 0
# 203-1/20230615_90000_1630s-1635s_cadence_coo.wav changed to 0
# 105-2/20230703_70000_1220s-1225s_cadence_coo.wav changed to 0
# 105-1/20230624_70000_670s-675s_cadence_coo.wav changed to 0
# 104-5/20230624_70000_55s-60s_cadence_coo.wav changed to 0
# 102-3/20230623_90000_445s-450s_cadence_coo.wav changed to 0
# As a note: when I was going through and double checking the audio, I changed files that had playback calls in them to 0. I will also go through and mask out these files later, but I want to minimize the chance that a false positive slips through.
# mask out just the file that has the playback during it or mask the whole day?

# Look at 2023 ELI-3 and 2023 PRD-3 in the File_Comments google sheets to determine if this is YBCU or BBCU

# Filter out files in the file_comments google sheet that were playback audio