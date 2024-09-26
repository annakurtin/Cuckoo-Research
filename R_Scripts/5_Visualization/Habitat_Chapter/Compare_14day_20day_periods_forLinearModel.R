### Compare time period of interest for linear model #####

# 14 day period
# selected based on which time period maximizes the number of calls across both 2023 and 2022
calls22_14 <- read.csv("./Data/Detection_History/2022_All_ARUs/Outputs/2022_Sites_DayswCalling_July1-15_9-26.csv")
calls23_14 <- read.csv("./Data/Detection_History/2023_All_ARUs/Outputs/2023_Sites_DayswCalling_July1-15_9-26.csv")
# number of 2022 sites: 35
nrow(calls22_14)
# number of 2022 sites with positive: 10
nrow(calls22_14[calls22_14$days_wcall > 0,])
# number of 2023 sites: 98
nrow(calls23_14)
# number of 2023 sites with positive: 13
nrow(calls23_14[calls23_14$days_wcall > 0,])
# total sites: 133
nrow(calls22_14)+nrow(calls23_14)
# number of positive sites: 23
nrow(calls22_14[calls22_14$days_wcall > 0,]) + nrow(calls23_14[calls23_14$days_wcall > 0,])


# 20 day period
# selected based on which time period maximizes the number of calls across both 2023 and 2022 then take the five days early to reflect the fact that the peak in calling starts earlier in 2023
calls22_20 <- read.csv("./Data/Detection_History/2022_All_ARUs/Outputs/2022_Sites_DayswCalling_Jun25-Jul15_9-26.csv")
calls23_20 <- read.csv("./Data/Detection_History/2023_All_ARUs/Outputs/2023_Sites_DayswCalling_Jun25-Jul15_9-26.csv")
# number of 2022 sites: 20
nrow(calls22_20)
# number of 2022 sites with positive: 4 
nrow(calls22_20[calls22_20$days_wcall > 0,])
# number of 2023 sites: 98
nrow(calls23_20)
# number of 2023 sites with positive: 17
nrow(calls23_20[calls23_20$days_wcall > 0,])
# total sites: 118
nrow(calls22_20)+nrow(calls23_20)
# number of positive sites: 21
nrow(calls22_20[calls22_20$days_wcall > 0,]) + nrow(calls23_20[calls23_20$days_wcall > 0,])
# less data points and less positives

# Going with the two week period from July 1 - July 14th 
# Run the model with both data to see if it makes a difference (I'm expecting it won't)