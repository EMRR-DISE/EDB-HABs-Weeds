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
stations <- read_csv("continuous_WQ_stations.csv")

## Pull data to plot cities alongside data
CA <- map_data("world") %>% filter(subregion=="California")
cities <- world.cities %>% filter(country.etc=="USA")
cities$long <- cities$long

## Plot maps of FluoroProbe data
groups <- unique(df_FP.l$Group) 

for (group in groups) {
  df_temp <- df_FP.l %>%
    filter(Group == group)
  
  plot <- ggplot(WW_Delta) + 
    geom_sf(fill = "lightblue") + 
    geom_point(data = df_temp,
               aes(x = Longitude,
                   y = Latitude,
                   fill = Conc,
                   size = 1),
               pch = 21,
               color = "black") +
    scale_fill_continuous(type = "viridis", 
                          limits = c(0,80)) +
    geom_point(data = cities %>% arrange(pop) %>% tail(500),
               aes(x = long,
                   y = lat)) +
    geom_text_repel(data = cities %>% arrange(pop) %>% tail(500), 
                    aes(x = long,
                        y = lat, 
                        label = name)) +
    #scale_y_continuous() +
    ylim(38, 38.2) +
    xlim(-122.5, -122.2) +
    theme_bw()
  
  plot + labs(x = "Longitude",
              y = "Latitude",
              fill = "Fluorescence (ug/L)",
              title = paste0("San Pablo Bay Phytoplankton, August 23, 2022 - ", group)) +
    guides(size = "none")
  
  
  ggsave(path="plots",
         filename = paste0("FP.map.Aug.2022.",group,".png"), 
         device = "png",
         scale=1.0, 
         units="in",
         height=6.5,
         width=9, 
         dpi="print")
  
}

## Plot MOPED values

plot.MOPED <- ggplot(WW_Delta) + 
  geom_sf(fill = "lightblue") + 
  geom_point(data = df_FP,
             aes(x = Longitude,
                 y = Latitude,
                 fill = EXO2.Chla,
                 size = 1),
             pch = 21,
             color = "black") +
  scale_fill_continuous(type = "viridis") +
  #                      limits = c(0,80)) +
  geom_point(data = cities %>% arrange(pop) %>% tail(500),
             aes(x = long,
                 y = lat)) +
  geom_text_repel(data = cities %>% arrange(pop) %>% tail(500), 
                  aes(x = long,
                      y = lat, 
                      label = name)) +
  #scale_y_continuous() +
  ylim(38, 38.2) +
  xlim(-122.5, -122.2) +
  theme_bw()

plot.MOPED

## Make plot comparing different fluorescence values (EXO2 and FluoroProbe)
plot.fluor <- ggplot(df_FP.l, aes(x = DateTime, y = Conc, color = Group)) +
  geom_line(data = subset(df_FP.l, Group == "EXO2.Chla")) +
  geom_line(data = subset(df_FP.l, Group == "FP.Total.Chl")) +
  scale_color_brewer(palette = "Dark2")

plot.fluor +
  labs(x = "Time",
       y = "Concentration (ug/L)",
       fill = "Fluorescence (ug/L)",
       title = "Chlorophyll Fluorescence in San Pablo Bay, August 2022") +
  guides(size = "none")

ggsave(path="plots",
       filename = "FP.map.Aug.2022.png", 
       device = "png",
       scale=1.0, 
       units="in",
       height=4,
       width=6, 
       dpi="print")
