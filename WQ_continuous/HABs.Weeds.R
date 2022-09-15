## Graph WQ Values at Franks Tract (FRK) and NCRO sites
## March 2022

library("tidyverse");packageVersion("tidyverse")
library("lubridate");packageVersion("lubridate")
library("timetk");packageVersion("timetk")

setwd("C:/Users/tflynn/Documents/R/HABs.Weeds")
theme_set(theme_bw())

rm(list=ls()) #cleans workspace

## Import data from False River near Oakley (FAL)
FAL <- read_csv("NCRO/HYCSV_FAL_20150101_20211117.csv", 
                skip = 3,
                col_names = c("DateTime","Temp","Qual1","SpCond","Qual2","DO.Conc","Qual3","Chl.a","Qual4","Turb","Qual5","Key"))

## Get dates to match
FAL$DateTime <- parse_date_time(FAL$DateTime, 
                           c("mdy HM"))

## Remove key column
FAL$Key <- NULL

## Pivot to long format
FAL.long <- FAL %>%
  pivot_longer(cols = matches("Qual"), names_to = "Type", values_to = "Qual")

FAL.long$Type <- NULL

## Remove low-quality data
FAL.long <- FAL.long %>%
  filter(Qual <= 2) ## To keep "Fair" data change to 40

FAL.long$Qual <- NULL

## Remove duplicate rows
FAL.long <- FAL.long %>% distinct()

## Pivot again
FAL.clean <- FAL.long %>%
  pivot_longer(cols = 2:6, names_to = "Analyte", values_to = "Amount")

## Import data from Middle River near Holt (HLT)
HLT <- read_csv("NCRO/HYCSV_HLT_20150101_20211208.csv",
                skip = 3,
                col_names = c("DateTime","Temp","Qual1","SpCond","Qual2","Chl.a","Qual3","Turb","Qual4","Key"))

## Get dates to match
HLT$DateTime <- parse_date_time(HLT$DateTime, 
                                c("mdy HM"))

## Remove key column
HLT$Key <- NULL

## Pivot to long format
HLT.long <- HLT %>%
  pivot_longer(cols = matches("Qual"), names_to = "Type", values_to = "Qual")

HLT.long$Type <- NULL

## Remove low-quality data
HLT.long <- HLT.long %>%
  filter(Qual <= 2) ## To keep "Fair" data change to 40

HLT.long$Qual <- NULL

## Remove duplicate rows
HLT.long <- HLT.long %>% distinct()

## Pivot again
HLT.clean <- HLT.long %>%
  pivot_longer(cols = 2:5, names_to = "Analyte", values_to = "Amount")

## Import data from Holland Cut near Bethel Island (HOL)
HOL <- read_csv("NCRO/HYCSV_HOL_20150101_20220119.csv", 
                skip = 3,
                col_names = c("DateTime","Temp","Qual1","SpCond","Qual2","DO.Conc","Qual3","Turb","Qual4","Key"))

## Get dates to match
HOL$DateTime <- parse_date_time(HOL$DateTime, 
                                c("mdy HM"))

## Remove key column
HOL$Key <- NULL

## Pivot to long format
HOL.long <- HOL %>%
  pivot_longer(cols = matches("Qual"), names_to = "Type", values_to = "Qual")

HOL.long$Type <- NULL

## Remove low-quality data
HOL.long <- HOL.long %>%
  filter(Qual <= 2) ## To keep "Fair" data change to 40

HOL.long$Qual <- NULL

## Remove duplicate rows
HOL.long <- HOL.long %>% distinct()

## Pivot again
HOL.clean <- HOL.long %>%
  pivot_longer(cols = 2:5, names_to = "Analyte", values_to = "Amount")

## Import Data from Old River at Quimby Island (ORQ)
ORQ <- read_csv("NCRO/HYCSV_ORQ_20150101_20211208.csv", 
                skip = 3,
                col_names = c("DateTime","Temp","Qual1","SpCond","Qual2","Turb","Qual3","Key"))

## Get dates to match
ORQ$DateTime <- parse_date_time(ORQ$DateTime, 
                                c("mdy HM"))

## Remove key column
ORQ$Key <- NULL

## Pivot to long format
ORQ.long <- ORQ %>%
  pivot_longer(cols = matches("Qual"), names_to = "Type", values_to = "Qual")

ORQ.long$Type <- NULL

## Remove low-quality data
ORQ.long <- ORQ.long %>%
  filter(Qual <= 2) ## To keep "Fair" data change to 40

ORQ.long$Qual <- NULL

## Remove duplicate rows
ORQ.long <- ORQ.long %>% distinct()

## Pivot again
ORQ.clean <- ORQ.long %>%
  pivot_longer(cols = 2:4, names_to = "Analyte", values_to = "Amount")



## Import data from Old River near Franks Tract (OSJ)
OSJ <- read_csv("NCRO/HYCSV_OSJ_20150101_20220106.csv", 
                skip = 3,
                col_names = c("DateTime","Temp","Qual1","SpCond","Qual2","DO.Conc","Qual3","Chl.a","Qual4","Turb","Qual5","Key"))

## Get dates to match
OSJ$DateTime <- parse_date_time(OSJ$DateTime, 
                                c("mdy HM"))

## Remove key column
OSJ$Key <- NULL

## Pivot to long format
OSJ.long <- OSJ %>%
  pivot_longer(cols = matches("Qual"), names_to = "Type", values_to = "Qual")

OSJ.long$Type <- NULL

## Remove low-quality data
OSJ.long <- OSJ.long %>%
  filter(Qual <= 2) ## To keep "Fair" data change to 40

OSJ.long$Qual <- NULL

## Remove duplicate rows
OSJ.long <- OSJ.long %>% distinct()
  
## Pivot again
OSJ.clean <- OSJ.long %>%
  pivot_longer(cols = 2:6, names_to = "Analyte", values_to = "Amount")


## Import data from MDM
MDM <- read_csv("NCRO/MDM_2015_2022.csv",
                skip = 1,
                col_names = c("DateTime","Chl.a","Flow","SpCond","Temp","Turb"))

## Pivot 
MDM.clean <- MDM %>%
  pivot_longer(cols = 2:6, names_to = "Analyte", values_to = "Amount")


## Import EMP data from Franks Tract (FRK)
## Import QA'd data from Andrew Tran
DO <- read_csv("EMP/FRK_data/FRK_DissolvedOxygen.csv", col_select = c(-...1, -station))
pH <- read_csv("EMP/FRK_data/FRK_pH.csv", col_select = c(-...1, -station))
Temp <- read_csv("EMP/FRK_data/FRK_WaterTemperature.csv", col_select = c(-...1, -station))
SpC <- read_csv("EMP/FRK_data/FRK_SpC.csv", col_select = c(-...1, -station))
Turb <- read_csv("EMP/FRK_data/FRK_Turbidity.csv", col_select = c(-...1, -station))
Chl <- read_csv("EMP/FRK_data/FRK_Fluorescence.csv", col_select = c(-...1, -station))

## Import QA'd 2021 data from A. Tran
DO.2021 <- read_csv("EMP/2021/FRK_DissolvedOxygen.csv", col_select = c(-...1, -station))
pH.2021 <- read_csv("EMP/2021/FRK_pH.csv", col_select = c(-...1, -station))
Temp.2021 <- read_csv("EMP/2021/FRK_WaterTemperature.csv", col_select = c(-...1, -station))
SpC.2021 <- read_csv("EMP/2021/FRK_SpC.csv", col_select = c(-...1, -station))
Turb.2021 <- read_csv("EMP/2021/FRK_Turbidity.csv", col_select = c(-...1, -station))
Chl.2021 <- read_csv("EMP/2021/FRK_Fluorescence.csv", col_select = c(-...1, -station))

## Combine datasets for each analyte
DO <- dplyr::bind_rows(DO, DO.2021)
pH <- dplyr::bind_rows(pH, pH.2021)
Temp <- dplyr::bind_rows(Temp, Temp.2021)
SpC <- dplyr::bind_rows(SpC, SpC.2021)
Turb <- dplyr::bind_rows(Turb, Turb.2021)
Chl <- dplyr::bind_rows(Chl, Chl.2021)

## Remove duplicates
DO <- DO %>% distinct()
pH <- pH %>% distinct()
Temp <- Temp %>% distinct()
SpC <- SpC %>% distinct()
Turb <- Turb %>% distinct()
Chl <- Chl %>% distinct()

## Import list of dates only
dates <- as_tibble(unique(pH$time)) %>%
  rename("time" = "value")

## Remove X-flagged data
DO <- DO %>% filter(DO$qaqc_flag_id != "X")
pH <- pH %>% filter(pH$qaqc_flag_id != "X")
Temp <- Temp %>% filter(Temp$qaqc_flag_id != "X")
SpC <- SpC %>% filter(SpC$qaqc_flag_id != "X")
Turb <- Turb %>% filter(Turb$qaqc_flag_id != "X")
Chl <- Chl %>% filter(Chl$qaqc_flag_id != "X")

## Combined all data into one 
FRK <- left_join(dates, DO[2:3]) %>%
  rename("DO.Conc" = "value")

FRK <- left_join(FRK, pH[2:3]) %>%
  rename("pH" = "value")

FRK <- left_join(FRK, Temp[2:3]) %>%
  rename("Temp" = "value")

FRK <- left_join(FRK, SpC[2:3]) %>%
  rename("SpCond" = "value")

FRK <- left_join(FRK, Turb[2:3]) %>%
  rename("Turb" = "value")

FRK <- left_join(FRK, Chl[2:3]) %>%
  rename("Chl.a" = "value")

## Rename date column
FRK <- FRK %>% rename ("DateTime" = "time")

## Pivot 
FRK.clean <- FRK %>%
  pivot_longer(cols = 2:7, names_to = "Analyte", values_to = "Amount")

## Bind all tibbles together into a single data frame
## Use .id to give a new column with original site name

WQ <- bind_rows("HOL" = HOL.clean, 
                  "ORQ" = ORQ.clean,
                  "FAL" = FAL.clean,
                  "FRK" = FRK.clean,
                  "HLT" = HLT.clean,
                  "MDM" = MDM.clean,
                  "OSJ" = OSJ.clean,
                  .id = "Site")

## Add column of just the date for grouping
WQ <- WQ %>%
  mutate(Date = date(WQ$DateTime))

## Remove 2022 data
WQ <- WQ %>% filter(Date <= "2021-12-31")

## Calculate daily mean
WQ.daily <- WQ %>%
  group_by(Site, Date, Analyte) %>%
  summarise(Daily.Mean = mean(Amount, na.rm = TRUE),
            Daily.Max = max(Amount, na.rm = TRUE),
            Daily.Min = min(Amount, na.rm = TRUE)) %>%
  ungroup

## Remove infinite min values from days with no data
WQ.daily <- WQ.daily %>% filter(!is.infinite(Daily.Min))
WQ.daily <- WQ.daily %>% filter(!is.infinite(Daily.Max))

## Add column of just the year for highlighting yearly data
## Add Julian date for plotting
WQ.daily <- WQ.daily %>%
  mutate(Year = year(WQ.daily$Date)) %>%
  mutate(Julian = yday(WQ.daily$Date)) %>%
  mutate(Date = date(WQ.daily$Date)) %>%
  mutate(month2 = month(WQ.daily$Date))

## Convert month numbers into text months (e.g., Jan, Feb, Mar)
## and append as a column named month3
WQ.daily <- WQ.daily %>%
  mutate(Month = month(WQ.daily$month2, label=TRUE))

## Order month 3 in calendar order rather than (default) alphabetical
WQ.daily$Month = factor(WQ.daily$Month, levels = month.abb)
WQ.daily$month2 <- NULL

## Save RDate files
save(WQ, file = "WQ.RData")
save(WQ.daily, file = "WQ.daily.RData")

## Example plots
plot.WQ.mean <- ggplot(WQ.daily) +
  geom_line(data = subset(WQ.daily, Analyte == "pH"), 
             aes(x = Julian, y = Daily.Mean, color = as.factor(Year)), 
             size = 1) +
  scale_x_continuous(breaks = c(1,60,121,182,244,305, 366), 
                     labels = c("Jan","Mar","May","Jul","Sep","Nov","Jan")) +
  labs(x = NULL,
       y = "pH",
       fill = "Year",
       title = "Daily Mean pH (2015-2021)")

plot.WQ.mean +
  theme(panel.background = element_rect(fill = "white", linetype = 0)) + 
  theme(panel.grid.minor = element_blank()) +
  scale_color_brewer(palette = "Set2", name = "Year") 
  #facet_wrap(Year ~ ., ncol = 3, dir = "h", scales = "free_y")

ggsave(path="plots",
       filename = paste0("pH_by_year.png"), 
       device = "png",
       scale=1.0, 
       units="in",
       height=5,
       width=6.5, 
       dpi="print")

## Make tables for FRK
## Calculate daily mean
WQ.monthly <- WQ %>%
  mutate(Year = year(WQ$DateTime)) %>%
  mutate(Julian = yday(WQ$DateTime)) %>%
  mutate(Date = date(WQ$DateTime)) %>%
  mutate(month2 = month(WQ$DateTime))

## Convert month numbers into text months (e.g., Jan, Feb, Mar)
## and append as a column named month3
WQ.monthly <- WQ.monthly %>%
  mutate(Month = month(WQ.monthly$month2, label=TRUE))

WQ.monthly <- WQ.monthly %>%
  group_by(Site, Year, Month, Analyte) %>%
  summarise(Monthly.Mean = mean(Amount, na.rm = TRUE),
            Monthly.Max = max(Amount, na.rm = TRUE),
            Monthly.Min = min(Amount, na.rm = TRUE)) %>%
  ungroup

write_csv(WQ.monthly, file = "WQ.monthly.csv")
