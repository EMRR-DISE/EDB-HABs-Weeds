#2021 Emergency Drought Barrier
#SePRO data
#Submersed aquatic vegetation point data
#format data for EDI package

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

# Packages--------
library(tidyverse) #suite of data science tools

# Read in the data----------------------------------------------

franks <- read_csv("./weeds/data_output/FranksTractManagement_2014-2021_formatted.csv")

#format for EDI

#create sample IDs
sampid <- franks %>%
  distinct(station,latitude_wgs84,longitude_wgs84,date) %>%
  arrange(station,latitude_wgs84,longitude_wgs84,date) %>%
  rowid_to_column("sample_id")


franks_format <- franks %>%
  #drop unneeded columns
  select(-c(program,survey_method)) %>%
  #add sample IDs
  left_join(sampid) %>%
  #move sample ID column up
  relocate(sample_id,.after = station) %>%
  glimpse()

#export data for EDI
#write_csv(franks_format,"./weeds/data_output/FranksTractManagement_2014-2021_formatted_edi.csv")
