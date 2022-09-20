## Cleaning and joining FluoroProbe and MOPED data, then generating a map
## 
## 8/25/22 TMF

library(tidyverse)
library(deltamapr)
library(sf)
library(ggrepel)
library(maps)

# Set working directory
setwd("./WQ-continuous")
getwd()

# Set visual theme in ggplot
theme_set(theme_bw())

# Clean workspace
rm(list=ls()) 

# Read in station GPS data
df_stations <- read_csv("continuous_WQ_stations.csv")

# Pull data to plot cities alongside data --------------------------------------
CA <- map_data("world") %>% filter(subregion=="California")
cities <- world.cities %>% filter(country.etc=="USA")
cities$long <- cities$long

# Plot map of continuous station locations -------------------------------------

plot <- ggplot(WW_Delta) + 
  geom_sf(fill = "lightblue") + 
  geom_point(data = df_stations,
             aes(x = Longitude,
                 y = Latitude,
                 fill = StationCode,
                 size = 2),
             pch = 21,
             color = "black") +
  geom_point(data = cities %>% arrange(pop) %>% tail(500),
             aes(x = long,
                 y = lat)) +
  geom_text_repel(data = cities %>% arrange(pop) %>% tail(500), 
                  aes(x = long,
                      y = lat, 
                      label = name)) +
  #scale_y_continuous() +
  ylim(37.9, 38.2) +
  xlim(-121.9, -121.3) +
  theme_bw()

plot + labs(x = "Longitude",
            y = "Latitude") +
  guides(size = "none")

ggsave(path="plots",
       filename = "map_continuous_WQ_stations.pdf", 
       device = "pdf",
       scale=1.0, 
       units="in",
       height=4,
       width=6.5, 
       dpi="print")


