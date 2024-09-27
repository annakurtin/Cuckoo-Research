### Visualize Distribution Linear Model Covariates ###

# Read in unscaled data and visualize the distribution
dat_us <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Linear_Model/DaysCalls_14DayPer_HabCovsUNSCALED_22-23_9-26.csv")
# how many positives?
nrow(dat_us[dat_us$days_wcall > 0,])

# Percent Canopy Landscape (palette_8[8])
dat_us %>% ggplot() +
  geom_point(aes(x = pct_can_landsc, y = days_wcall), col = palette_8[8])+
  labs(x = "% Canopy at Landscape Scale", y = "Count of Days with Calls") +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 2)) +
  theme_minimal()


# Percent Open Shrub Landscape palette_8[6]
dat_us %>% ggplot() +
  geom_point(aes(x = pct_openshrub_landsc, y = days_wcall), col = palette_8[6])+
  labs(x = "% Shrub/Early Successional Cover at Landscape Scale", y = "Count of Days with Calls") +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 2)) +
  theme_minimal()


# Veg SD Residuals "Veg SD Residuals"=palette_8[7]
dat_us %>% ggplot() +
  geom_point(aes(x = veg_sd_resid, y = days_wcall), col = palette_8[7])+
  labs(x = "Veg St Dev Residuals from Canopy Height", y = "Count of Days with Calls") +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 2)) +
  theme_minimal()


# Percent Openshrub Core "Percent Shrub/Early Successional Core" = palette_5[5]
dat_us %>% ggplot() +
  geom_point(aes(x = pct_openshrub_core, y = days_wcall), col = palette_8[5])+
  labs(x = "% Shrub/Early Successional Cover at Core Scale", y = "Count of Days with Calls") +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 2)) +
  theme_minimal()


# Combined days of recording palette_5[1]
dat_us %>% ggplot() +
  geom_point(aes(x = combined_days_rec, y = days_wcall), col = palette_5[1])+
  labs(x = "Recording Days at Site", y = "Count of Days with Calls") +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10, by = 2)) +
  theme_minimal()