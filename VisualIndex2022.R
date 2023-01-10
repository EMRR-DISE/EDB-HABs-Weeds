#Add 2022 data to HAB observations
library(dplyr)
library(readr)
library(ggplot2)
library(DroughtData)
library(lubridate)
library(sf)

load("data/HABs.RData")


#take out DOP because they do things differently
HABs = filter(HABs, Source != "DOP")

#check a few plots for outliers
ggplot(HABs, aes(x = Temperature)) + geom_histogram()
summary(HABs$Temperature)
filter(HABs, Temperature <5)
#missing 120 rows, and some of those are 0s, definitely wrong.
HABs = filter(HABs, Temperature >5)

ggplot(HABs, aes(x = Secchi)) + geom_histogram()
summary(HABs$Secchi)
test =filter(HABs, Secchi <10)

group_by(HABs, Source) %>%
  summarize(secm = min(Secchi, na.rm = T), secM = max(Secchi, na.rm = T))

#Ugh, definitely some more rows where Secchi is in meters, not centemeters. But its not consistent!

HABs = mutate(HABs, Secchi = case_when(Secchi <5 ~Secchi *100,
                                       TRUE ~ Secchi))
save(HABs, file = "data/HABs.RData")

#First let's do USGS. THis is going to be a little difficult to do comparisons
#between years, since they've only got two years. Also, they don't have temperature and turbidity and stuff.
#We might watnt to ask for that.
USGS = read_csv("data/HABs/MC_ratings_2022-09-22_CS_USGS.csv")

str(USGS)
USGS = mutate(USGS, Date = mdy(`Sample start date`)) %>%
  rename(Station = `Field ID`, Latitude = `lattitude (decimal degrees)`, Longitude = `longitude (decimal degrees)`,
         Microcystis = `field MC rating`) %>%
  mutate(Source = "USGS", Year = year(Date), Month = month(Date)) %>%
  dplyr::select(Source, Station, Latitude, Longitude, Date, Microcystis, Year, Month)


#now EMP
EMP = read_csv("data/HABs/MCVisual_2022_EMP.csv") %>%
  rename(Station = `Station Name`, Date = `Sample Date`,
         Microcystis = `Microcystis aeruginosa Field Observations (Water) 925 [1]*`) %>%
  mutate(Date= mdy(Date))
#Sigh. This is less than ideal

#update station coordinates
stations = read_csv("data/AllIEP_wRegions.csv") %>%
  rename(Station = StationCode, Source = Survey)
EMPsta = filter(stations, Source == "EMP") %>%
  dplyr::select(Latitude, Longitude, Station, Source)

#add coordinates and regions
EMP2 = left_join(EMP, EMPsta) %>%
  filter(!is.na(Latitude)) %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  dplyr::select(Latitude, Longitude, Month, Year, Station, Microcystis, Source, Date)

#STN
STN = read_csv("data/HABs/MCvisual_CatchPerStation_STN.csv") %>%
  mutate(Date = mdy(SampDate), Month = month(Date), Source = "STN", Station = as.character(StationCode)) %>%
  rename(Temperature = TemperatureTop) %>%
  dplyr::select(Source, Station, Microcystis, Date, Year, Month, Temperature, Secchi)


#grab the station lat/longs
STNsta = filter(stations, Source == "TNS") %>%
  mutate(Source = "STN")

#add the coodinates
STN2 = left_join(STNsta, STN) %>%
   mutate(Year = year(Date), Month = month(Date)) %>%
  dplyr::select(Latitude, Longitude, Month, Year, Station, Microcystis, Source, Date, Temperature, Secchi)


#NCRO
library(readxl)
NCRO = read_excel("data/HABs/qry_HabObs_StationName_DeployEnd.xlsx") %>%
  mutate(Microcystis = case_when(FldObsWaterHabs == "Not Visible" ~ 1,
                                 FldObsWaterHabs == "Low" ~ 2,
                                 FldObsWaterHabs == "Medium" ~ 3),
         Microcystis = as.numeric(Microcystis), Source = "NCRO") %>%
  mutate(Date = date(DeploymentEnd), Year = year(DeploymentEnd), Month = month(DeploymentEnd)) %>%
  filter(!is.na(Microcystis)) %>%
  rename(Station = StationName)


#NCRO station coordinates
NCROstas = read_excel("data/Station_Metadata_RevNov2022_RKH.xlsx")  %>%
  rename(Latitude = `Latitude (WGS84)`, Longitude = `Longitude (WGS84)`) %>%
  select(Station, Latitude, Longitude)

NCRO2 = left_join(NCRO, NCROstas)
test = filter(NCRO2, is.na(Latitude)) %>%
  select(Station) %>%
  distinct()
#write.csv(test, "MissingStations.csv")
#Still need lat/slongs for a few NCRO stations

#FMWT
FMWT = read_csv("data/FMWT 1967-2022 Catch Matrix_updated_tidy.csv") %>%
  filter(Year == 2022)

FMWT2 = rename(FMWT, Station = StationCode, Latitude = StationLat,
               Longitude = StationLong, Temperature = WaterTemperature, Date = SampleDate) %>%
  mutate(Source = "FMWT", Month = month(Date), Station = as.character(Station)) %>%
  dplyr::select(Source, Station, Latitude, Longitude, Date, Month, Year, Microcystis, Secchi, Temperature) %>%
  distinct()

#Put them all together
HABs2022 = bind_rows(STN2, EMP2, USGS, NCRO2, filter(HABs, Source != "NCRO", Source != "DWR_NCRO"), FMWT2) %>%
  dplyr::select(Source, Station, Latitude, Longitude, Date, Microcystis, Secchi,
         Temperature, Month, Year, Chlorophyll, Chlorophyll_lab) %>%
  mutate(Source = case_when(Source == "DWR_EMP" ~ "EMP",
                            Source == "DWR_NCRO" ~ "NCRO",
                            Source == "FMWTx" ~ "FMWT",
                            TRUE ~ Source),
         Year = year(Date)) %>%
  filter(!is.na(Microcystis), !is.na(Date)) %>%
  mutate(Microcystis = round(Microcystis)) %>%
  filter(Microcystis !=0, Source != "USGS")

#did I get all the surveys added in correctly?
ggplot(HABs2022, aes(x =  Microcystis)) + geom_histogram()+
  facet_grid(Year~Source)

save(HABs2022, file = "HABsw2022.RData")

#crop for just Delta and Mash
load("data/Regions.RData")
HABs2022sf = st_as_sf(filter(HABs2022, !is.na(Latitude)), coords = c("Longitude", "Latitude"), crs = 4326)
HABssf = st_join(HABs2022sf, reg3) %>%
  filter(!is.na(Stratum), Month %in% c(6:12))

#plot of trends by year
ggplot(HABs2022sf, aes(x = Year, fill = as.factor(Microcystis))) +geom_bar(position = "fill", color = "grey")+
  scale_fill_manual(values = c("aliceblue", "tan2", "yellow", "red", "darkred"),
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  theme_bw()

#group by water year type and figure out what I'm doing for the final graphs
yrs = read_csv("data/yearassignments.csv") #%>%
  #rename(WY = Year)

#let's go by calendar year instead of water year for this one
HABs2022 = mutate(HABssf, WY = case_when(Month %in% c(10,11,12)~Year+1,
                                           TRUE ~ Year)) %>%
  left_join(yrs) %>%
  #filter(WY != 2023) %>%
  mutate(Whitepaper = factor(Whitepaper, levels = c("Critical", "Dry", "Below Normal",
                                                    "Above Normal", "Wet", "2020", "2021", "2022")))


ggplot(HABs2022, aes(x = Whitepaper, y = Microcystis, fill = Yr_type))+
  geom_boxplot()+ drt_color_pal_yrtype()

ggplot(HABs2022, aes(x = Whitepaper, fill = as.factor(Microcystis))) +
  geom_bar(position = "fill", color = "grey")+
  scale_fill_manual(values = c("aliceblue", "tan2", "yellow", "red", "darkred"),
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  theme_bw()+ xlab(NULL)



###############################################################################
#upload data from Christy that has more FMWT data added
HABs22x = read_csv("data/VisMicro_010523.csv")
HABs22x = left_join(HABs22x, yrs)

ggplot(HABs22x, aes(x = Whitepaper, fill = as.factor(Microcystis))) +
  geom_bar(position = "fill", color = "grey")+
  scale_fill_manual(values = c("aliceblue", "tan2", "yellow", "red", "darkred"),
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  theme_bw()+ xlab(NULL)

#how much does the ugsg data throw us off?

ggplot(filter(HABs22x, Source != "USGS"), aes(x = Whitepaper, fill = as.factor(Microcystis))) +
  geom_bar(position = "fill", color = "grey")+
  scale_fill_manual(values = c("aliceblue", "tan2", "yellow", "red", "darkred"),
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  theme_bw()+ xlab(NULL)
# a little bit, not much

test = filter(HABs22x, Year == 2022, Source != "USGS") %>%
  mutate(Microcystis = factor(Microcystis, levels = c("Absent", "Low", "Medium", "High", "Very High")))
test2 = filter(HABs2022, Year == 2022)  %>%
  mutate(Microcystis = factor(Microcystis, levels = c(1,2,3,4,5), labels = c("Absent", "Low", "Medium", "High", "Very High")))
HABs2022 =  mutate(HABs2022, Microcystis = factor(Microcystis, levels = c(1,2,3,4,5), labels = c("Absent", "Low", "Medium", "High", "Very High")))

#OK, let's use Kristi's 2022 data
HABs22z = filter(HABs2022, Year != 2022) %>%
  bind_rows(test)

ggplot(HABs22z, aes(x = Whitepaper, fill = as.factor(Microcystis))) +
  geom_bar(position = "fill", color = "grey")+
  scale_fill_manual(values = c("aliceblue", "tan2", "yellow", "red", "darkred"),
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  theme_bw()+ xlab(NULL)
