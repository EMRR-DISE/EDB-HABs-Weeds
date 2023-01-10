#Summer fall seasonal repor tmicrocystis graphs


#Plot for summer-fall seasonal report

library(deltamapr)

load("HABsw2022.RData")
HABs2022only = filter(HABs2022, Year == 2022, Source != "DOP", Source != "USGS")
sumfall = filter(HABs2022, Month %in% c(6,7,8,9,10), !is.na(Microcystis), Source != "DOP", Source != "USGS") %>%
  mutate(Microcystis = round(Microcystis))
ggplot(sumfall, aes(x = Year, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"),
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  theme_bw()


regions = R_EDSM_Strata_19P3 %>%
  filter(Stratum != "Western Delta") %>%
  st_transform(crs = 4326)


HABssf1 = filter(sumfall, !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))

ggplot() + geom_sf(data = regions) + geom_sf(data = HABssf1)
#crop it to the area we are interested in
sfhab = st_crop(HABssf1, regions)%>%
  st_join(regions) %>%
  mutate(Mic = factor(Microcystis, levels = c(1,2,3,4,5), labels = c("Absent", "Low", "Medium", "High", "Very Hight")),
         Month2 = factor(Month, levels = c(6,7,8,9), labels = c("Jun", "Jul", "Aug", "Sep")),
         Regions = factor(Stratum, levels = c("Cache Slough/Liberty Island", "Lower Joaquin River", "Lower Sacramento River",
                                              "Sac Deep Water Shipping Channel", "Suisun Bay", "Suisun Marsh"),
                          labels = c("Cache/Liberty", "Lower SJR", "Lower Sac", "SDWSC", "Suisun Bay", "Suisun Marsh"))) %>%
  filter(!is.na(Stratum), Stratum != "Lower Joaquin River")
#filter(Year == 2021)


ggplot(filter(sfhab, Year == 2022), aes(x = Regions, fill = Mic))+geom_bar(position = "fill", color = "grey")+
  facet_wrap(~Month2, nrow = 1)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), name = "Microcystis") +
  theme(legend.position = "top")+
  ylab(NULL) + xlab(NULL)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


ggplot(sfhab, aes(x = Year, fill = Mic)) +geom_bar(position = "fill")+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"),
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")
