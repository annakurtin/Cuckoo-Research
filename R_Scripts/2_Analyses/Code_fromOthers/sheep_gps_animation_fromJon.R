# Other resources
# Making a GIF in R: https://www.nagraj.net/notes/gifs-in-r/

# libraries
library(tidyverse)
library(rnaturalearth)
library(gganimate)
library(ggmap)
library(sf)

ggmap::register_google(key = "AIzaSyCwEQ4Tc4D-WsKvqUYWl_GqU1LqY09DllY")

bhs <- readRDS("C:/Users/Jonathan Farr/Box/bhs_3stateSeasonalHMMs_OUTPUTS_Nov132023.rds") %>% 
  filter(ID == "B19" & lubridate::year(t1_) == 2022, szn == "Winter") %>% 
  st_as_sf(., coords = c("x", "y"), crs = 26911) %>% 
  st_transform(., 4326)

bhs <- bhs %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) %>% 
  mutate(states = ifelse(states.v == 1, "Relocating", 
                         ifelse(states.v == 2, "Foraging", "Resting")))

bhs$states = factor(bhs$states, levels = c("Relocating", "Foraging", "Resting"))

bb <- st_bbox(bhs)

names(bb) <- c("left","bottom","right","top") #the bounding box needs renaming
m <- ggmap::get_map(location = bb, source="google",   maptype = "hybrid", crop = FALSE)

p =
  # basemap
  ggmap(m)+
   #coord_sf(xlim = range(bhs$lon, na.rm = TRUE), 
    #       ylim = range(bhs$lat, na.rm = TRUE), 
    #      expand = TRUE)+
  
  # lines and points
  geom_path(data = bhs, 
            aes(x=lon, y=lat, group=ID, color=states), 
            alpha = 0.7, linewidth = 2)+
  geom_point(data = bhs, 
             aes(x=lon, y=lat, group=ID, fill=states),
             alpha = 0.7, shape=21, size = 4)+
  
  # formatting
  scale_color_manual(values = c("navy", "#55CFD8", "yellow"))  + 
  scale_fill_manual(values = c("navy", "#55CFD8", "yellow"))  + 
  scale_size_continuous(range = c(0.1,10))+
  labs(x=NULL, y=NULL, 
       fill = 'Behavioural State', 
       color = 'Behavioural State')+
  theme_dark()+
  theme(panel.grid = element_blank())
p


# animate
anim = p + 
  transition_reveal(along = t1_)+
  ease_aes('linear')+
  ggtitle("Date: {frame_along}")

anim2 <- animate(anim, nframes = 20, fps = 3, height = 700, width = 1000, res = 140)

anim_save("relcating_B20.gif", animation =anim2)
