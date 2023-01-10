#2021 Emergency Drought Barrier
#2021 CSTARS ground truthing data
#Submersed aquatic vegetation point data
#filter and format data just forFranks Tract, Big Break, and Clifton Court

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

#To do list
#incorporate zeros representing absences into abundance calculations

# Packages--------
library(tidyverse) #suite of data science tools
library(sf) #tools for making maps
library(deltamapr) #Sam's package with shapefiles for delta waterways

# Read in the data----------------------------------------------

#read in 2021 Delta SAV data
#cstars <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/AquaticVegetationPWT/main/MasterDataSet_SAV/Data_Formatted/CSTARS_2021_formatted.csv")
cstars <- read_csv("./weeds/data_input/ucd_rake_samples/CSTARS_2021_formatted.csv") %>%
  arrange(latitude_wgs84,longitude_wgs84) %>%
  glimpse()

#read in shape files for Franks Tract, Big Break, and Clifton Court
#files from the CSTARS lab
sf_franks <- read_sf("./weeds/data_input/ucd_region_shapefiles/FranksTractarea_wgs84.shp")
sf_bbreak <- read_sf("./weeds/data_input/ucd_region_shapefiles/BigBreak_wgs84.shp")
sf_ccourt <- read_sf("./weeds/data_input/ucd_region_shapefiles/Cliftoncourtarea_wgs84.shp")

#format data set-----------

#quick look at sample depth
#hist(cstars$depth_to_sav_m)

#website with EPSG codes for CRS
#https://spatialreference.org/

#combine Stuckenia pectinata and S. filiformis
#not consistently differentiated in field
#possibly not even different species
#start by looking at all records of them in the data set
cstars_stuck <- cstars %>%
  filter(grepl("Stuckenia",species))
#59 obs

#are the two species ever recorded in same sample?
cstars_stuck_dist <- cstars_stuck %>%
  distinct(latitude_wgs84,longitude_wgs84)
#56 obs; yes, so need to combine some within sample values

cstars_format <- cstars %>%
  #change Stuckenia filiformis to S. pectinata
  mutate(species = case_when(
    #find and replace Stuckenia_filiformis
    str_detect(species,"filiformis")~"Stuckenia_pectinata"
    #for all other names, keep them as they are
    ,TRUE ~ as.character(species)
  ))

cstars_format2<-cstars_format %>%
  #combine rake proportions for the two Stuckenia spp in the few samples where they are both present
  group_by(latitude_wgs84,  longitude_wgs84, date,rake_teeth_corr,species) %>%
  summarize(rake_prop = sum(rake_prop), .groups = 'drop')

cstars_format3 <- cstars_format2 %>%
  #specify the crs which is wgs84
  st_as_sf(coords = c(x='longitude_wgs84',y='latitude_wgs84'),
           crs = 4326
           ,remove=F #retains original columns
           ) %>%   #EPSG code for WGS84
  glimpse()

#First draft of sampling maps-------------

#Note: NAD83 and WGS84 are highly similar and perhaps indistinguishable
#this explains why transformations between them appear to do nothing
#https://www.esri.com/arcgis-blog/products/arcgis-desktop/mapping/wgs84-vs-nad83/

#look at WW_Delta base map CRS
st_crs(WW_Delta)
#CRS = NAD83, which is different than our sample data points; EPSG: 4269
WW_Delta_4326 <- st_transform(WW_Delta, crs = 4326)

#create map showing all Delta SAV data points
(sav_map_all <- ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
  #plot SAV sampling points
  geom_sf(data= cstars_format3, fill= "red", color= "black", shape= 22, size= 1.5)+
    coord_sf(
      xlim =c(-121.870, -121.251),
      ylim = c(38.570, 37.801)
    )+
    theme_bw()
)

#create map showing Franks Tract SAV data points
(sav_map_ft <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data= cstars_format3, fill= "red", color= "black", shape= 22, size= 3.5)+
    #set bounding box for site
    #Box picks up a few unneeded sampling over in Taylor Slough
    coord_sf(
      xlim =c(-121.677, -121.576),
      ylim = c(38.07, 38.02)
    )+
    theme_bw()+
    ggtitle("Franks Tract")
)

#create map showing Big Break SAV data points
(sav_map_bb <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data= cstars_format3, fill= "red", color= "black", shape= 22, size= 3.5)+
    #set bounding box for site
    #No stray samples from outside sites captured in this box
    coord_sf(
      xlim =c(-121.740, -121.685),
      ylim = c(38.031, 38.005)
    )+
    theme_bw()+
    ggtitle("Big Break")
)

#create map showing Clifton Court SAV data points
#no sampling in Clifton Court
#not too surprising because access is restricted
(sav_map_cc <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data= cstars_format3, fill= "red", color= "black", shape= 22, size= 3.5)+
    coord_sf(
      xlim =c(-121.605, -121.552),
      ylim = c(37.867, 37.818)
    )+
    theme_bw()+
    ggtitle("Clifton Court")
)

#create subsets of data sets by site using site polygons---------

#Look at CRS for CSTARS shape file forS Franks Tract, Big Break, and Clifton Court
st_crs(sf_franks)
sf_franks_4326 <- st_transform(sf_franks, crs = 4326)

st_crs(sf_bbreak)
sf_bbreak_4326 <- st_transform(sf_bbreak, crs = 4326)

st_crs(sf_ccourt)
sf_ccourt_4326 <- st_transform(sf_ccourt, crs = 4326)

#plot the shape files
(map_ft <- ggplot()+
    geom_sf(data= sf_franks_4326, fill= "skyblue3", color= "black")
)

(map_bb <- ggplot()+
    geom_sf(data= sf_bbreak_4326, fill= "skyblue3", color= "black")
)

(map_cc <- ggplot()+
    geom_sf(data= sf_ccourt_4326, fill= "skyblue3", color= "black")
)

# Create a bounding box based on the Franks Tract shapefile
#will be used to crop base map in plots
bbox_fr_4326 <- st_bbox(sf_franks_4326)

# Create a bounding box based on the Big Break shapefile
#will be used to crop base map in plots
bbox_bb_4326 <- st_bbox(sf_bbreak_4326)

#Filter CSTARS data set to just those within the Franks Tract polygon
weeds_franks <- cstars_format3 %>%
  add_column(site="Franks Tract") %>%
  st_filter(sf_franks_4326)
#n=106 (not samples which are fewer)

#Filter CSTARS data set to just those within the Big Break polygon
weeds_bbreak <- cstars_format3 %>%
  add_column(site="Big Break") %>%
  st_filter(sf_bbreak_4326)
#n=61 rows (not samples which are fewer)

#Filter CSTARS data set to just those within the Clifton Court polygon
weeds_ccourt <- cstars_format3 %>%
  add_column(site="Clifton Court") %>%
  st_filter(sf_ccourt_4326)
#n=0; no sampling in clifton court

#write a file with Big Break and Franks Tract data
weeds_fb <- bind_rows(weeds_franks,weeds_bbreak)
#write_csv(weeds_fb,"./weeds/data_output/CSTARS_2021_FT&BB_Formatted.csv")
