# LEt's look at the relationship between Delta Outflow and current speed

library(cder)
library(tidyverse)
library(lubridate)
library(dataRetrieval)

flowdata = readNWISdata(sites = c("11313452","11455315", "11337190", "11312685", "11455420"),
                        service = "iv", parameterCd = "72255", startDate = "2014-01-01T00:00", endDate = "2021-12-31T00:00")
save(flowdata, file="flowdata.RData")

#attach station names and rename things
stas = data.frame(site_no = c("11313452","11455315", "11337190", "11312685", "11455420"),
                  Station = c("Old R at Franks", "Liberty Isld","Jersey Point","Middle R at Holt", "Rio Vista" ))


flowdata = rename(flowdata, Velocity = X_72255_00000) %>%
  left_join(stas)

#flowdata = cdec_query(c("SRV", "LIB", "SJC", "RYF", "C31", "SJJ", "GES", "HOL"), c(20, 21), "E", ymd("2010-01-01"), ymd("2021-12-31"))
Outflow = cdec_query(c("DTO"), c(23), "D", ymd("2010-01-01"), ymd("2021-12-31")) %>%
  mutate(Date = date(DateTime), Year = year(Date), Month = month(Date), StationID = NULL) %>%
  rename(Outflow = Value)

Flowmean = flowdata %>%
  #filter(!is.na(Velocity), Value < 100000) %>%
  mutate(Date = date(dateTime)) %>%
  group_by(Date, Station) %>%
  summarise(Mean = mean(Velocity, na.rm = T), Max = max(Velocity, na.rm = T), Min = min(Velocity, na.rm = T),
            MaxVel = max(abs(Max), abs(Min)))

Flow = left_join(Flowmean, Outflow)

ggplot(Flow, aes(x = Date, y = Mean)) + geom_point()+facet_wrap(~Station)

Flow = Flow %>%
  mutate(Month = month(Date), Season = case_when(Month %in% c(12,1,2) ~ "Winter",
                            Month %in% c(3,4,5) ~ "Spring",
                            Month %in% c(6,7,8) ~ "Summer",
                            Month %in% c(9,10,11) ~ "Fall"),
         Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter"))) %>%
  filter(Outflow < 200000)

ggplot(Flow, aes(x= Outflow, y = MaxVel))+ geom_point()+ geom_smooth() + facet_wrap(~Station, scales = "free")

ggplot(Flow, aes(x= Outflow, y = MaxVel))+ geom_point(aes(color = as.factor(Year)))+ geom_smooth() +
  facet_grid(Station~Season, scales = "free") +
  ylab("Daily Maximum current speed")+ xlab("Daily Mean Delta OutFlow")

TUCO = data.frame(Outflow = c(3000, 4000), Scenario = c("TUCO", "D1641"), Season = c("Summer", "Summer")) %>%
  mutate(Season = factor(Season, levels = c("Spring", "Summer", "Fall", "Winter")))

ggplot(Flow, aes(x= log(Outflow), y = MaxVel))+ geom_point(aes(color = as.factor(Year)))+ geom_smooth() +
  facet_grid(Station~Season, scales = "free") +
  geom_vline(data = TUCO, aes(xintercept = log(Outflow), linetype = Scenario))+
  scale_color_brewer(palette = "Set2", name = NULL)+ theme_bw()+ theme(legend.position = "bottom")+
  ylab("Daily Maximum current speed (m/sec)")+ xlab("Log Daily Mean Delta OutFlow (CFS)")

ggsave("VelocityPlot.tiff", device = "tiff", width = 7, height = 7)


summerFlow = filter(Flow, Month %in% c(6,7,8))
ggplot(summerFlow, aes(x= log(Outflow), y = MaxVel))+ geom_point(aes(color = as.factor(Year)))+
  geom_smooth() + facet_wrap(~Station) +
  ylab("Daily Maximum current speed")+ xlab("Daily Mean Flow")
ggplot(summerFlow, aes(x= Outflow, y = MaxVel))+ geom_point(aes(color = as.factor(Year)))+ geom_smooth() + facet_wrap(~StationID, scales = "free")

ggplot(summerFlow, aes(x= Outflow, y = MaxVel))+ geom_point()+#geom_point(aes(color = as.factor(Year)))+
  geom_smooth() + facet_wrap(~StationID, scales = "free") +
  xlim(0, 10000) + ylab("Daily Maximum current speed")+ xlab("Daily Mean Delta Outflow")

LIB = filter(flowdata, StationID == "LIB", SensorNumber == 21)


ggplot(LIB, aes(x = DateTime, y = Value)) + geom_point() + geom_line()
