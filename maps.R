#maps for the miner/steamboat barriers

library(tidyverse)
library(sf)
library(deltamapr)

ggplot() + geom_sf(data = WW_Delta)+
  coord_sf(ylim = c(38.15, 38.4), xlim = c(-121.5, -121.8))

NDbbox = data.frame(name = "box",
                    geometry = list(matrix(data =  c(38.15,38.15, 38.4, 38.4,38.15, -121.5, -121.8, -121.5, -121.8,  -121.5), nrow = 5))) %>%
group_by(name) %>%

  mutate(feature = st_polygon(list(matrix(geometry.1, geometry.2,  nrow = 5, ncol = 2))))

NDbox = st_bbox(c(xmin = -121.8, ymin = 38.15, xmax = -121.5, ymax = 38.4), crs = st_crs(4326))

delta = st_transform(WW_Delta, crs = 4326)

 NDbarries = st_crop(delta, NDbox)

ggplot() + geom_sf(data = WW_Delta)+
  geom_sf(data = NDbarries, color = "red")
  coord_sf(ylim = c(38.15, 38.4), xlim = c(-121.5, -121.8))

st_write(NDbarries, "NDBarriers.shp")
