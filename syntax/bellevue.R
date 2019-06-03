# Bellevue EV installation and EV vs. PV 

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(pander)
library(stringr)
library(SpatialEpi)
library(maps)
library(shapefiles)
library(maptools)
library(RColorBrewer)
library(tidyverse)
library(spdep)
library(INLA)
library(rgdal)
library(pander)
library(DCluster)
library(geoR)
library(sp)
library(spatstat)
library(splancs)
library(GWmodel)
library(psych)
library (cluster)
library(reshape)
library(reshape2)
library(som)
library(GPArotation)
library(corrplot)
library(GGally)
library(faraway)
library(sjPlot)
library(lattice)
library(png)
library(grid)
library(gridExtra)
library(colorRamps)

data <- read_csv(file = "./data/raw/Bellevue_Permit_Data.csv")
# str(data)
# head(data)
# View(data)


ev <- data %>% 
  filter(`PERMIT STATUS` %in% c("Closed","Expired"),
         str_detect(SUBTYPE, "Residential|Family"),
         !is.na(`FINALED DATE`),
         str_detect(`PROJECT DESCRIPTION`, regex("ev charger|ev charging|car charger|car charging|
                    charging Station|charging stations|vehicle", ignore_case=T))) %>% 
  mutate(Class = recode(SUBTYPE, `Single Family Residential` = "Single family",
                        `Multifamily Residential` = "Multifamily",
                        `Single Family Condo Unit` = "Multifamily")) %>% 
  mutate(Class = parse_factor(Class, levels = c("Single family",
                                                  "Multifamily"))) %>% 
  mutate(CONTRACTOR = recode(CONTRACTOR, 
                                        `PUGETSOUNDSOLAR` = "PUGET SOUND SOLAR",
                                        `Puget Sound Solar` = "PUGET SOUND SOLAR",
                                        `Puget Sound Solar LLC` = "PUGET SOUND SOLAR",
                                        `ARTISAN ELECTRICAL SERVS LLC` = "ARTISAN ELECTRIC INC",
                                        `NORTHWEST ELECTRIC & SOLAR LLC` = 
                                          "NORTHWEST ELECTRIC & SOLAR",
                                        `Northwest Electric & Solar` = 
                                          "NORTHWEST ELECTRIC & SOLAR",
                                        `WEST SEATTLE ELECTRIC AND SOLA` = 
                                          "WEST SEATTLE ELECTRIC & SOLAR",
                                        `West Seattle Electric and Solar` = 
                                          "WEST SEATTLE ELECTRIC & SOLAR",
                                        `Offset Solar` = "OFFSET SOLAR LLC",
                                        `SME Inc of Seattle` = "SME INC OF SEATTLE",
                                        `sunergy systems` = "SUNERGY SYSTEMS",
                                        `SUNERGYSYSTEMS` = "SUNERGY SYSTEMS",
                                        `SUNERGY SYSTEMS INC.` = "SUNERGY SYSTEMS"),
         Lat = str_extract(`Location 1`, "\\(47.[0-9]*"),
         Lat = as.numeric(str_replace(Lat, "\\(", replacement = "")),
         Lon = as.numeric(str_extract(`Location 1`, "-122.[0-9]*")),
         `FINALED DATE` = mdy(`FINALED DATE`)) %>%
  filter(!is.na(Lon)) %>% 
  dplyr::select(`PERMIT YEAR`, Class,`PROJECT DESCRIPTION`,`FINALED DATE`,
         CONTRACTOR,`TOTAL PAID`,SUBAREA, `NEIGHBORHOOD AREA`, Lat, Lon) 

# View(ev)

totev <- data %>% 
  filter(`PERMIT STATUS` %in% c("Closed","Expired"),
         str_detect(SUBTYPE, "Residential|Family"),
         !is.na(`FINALED DATE`)) %>% 
  mutate(Class = recode(SUBTYPE, `Single Family Residential` = "Single family",
                        `Multifamily Residential` = "Multifamily",
                        `Multi-Family in Design District` = "Multifamily",
                        `Single Family Condo Unit` = "Multifamily")) %>% 
  mutate(Class = parse_factor(Class, levels = c("Single family",
                                                "Multifamily"))) %>% 
  mutate(CONTRACTOR = recode(CONTRACTOR, 
                             `PUGETSOUNDSOLAR` = "PUGET SOUND SOLAR",
                             `Puget Sound Solar` = "PUGET SOUND SOLAR",
                             `Puget Sound Solar LLC` = "PUGET SOUND SOLAR",
                             `ARTISAN ELECTRICAL SERVS LLC` = "ARTISAN ELECTRIC INC",
                             `NORTHWEST ELECTRIC & SOLAR LLC` = 
                               "NORTHWEST ELECTRIC & SOLAR",
                             `Northwest Electric & Solar` = 
                               "NORTHWEST ELECTRIC & SOLAR",
                             `WEST SEATTLE ELECTRIC AND SOLA` = 
                               "WEST SEATTLE ELECTRIC & SOLAR",
                             `West Seattle Electric and Solar` = 
                               "WEST SEATTLE ELECTRIC & SOLAR",
                             `Offset Solar` = "OFFSET SOLAR LLC",
                             `SME Inc of Seattle` = "SME INC OF SEATTLE",
                             `sunergy systems` = "SUNERGY SYSTEMS",
                             `SUNERGYSYSTEMS` = "SUNERGY SYSTEMS",
                             `SUNERGY SYSTEMS INC.` = "SUNERGY SYSTEMS"),
         Lat = str_extract(`Location 1`, "\\(47.[0-9]*"),
         Lat = as.numeric(str_replace(Lat, "\\(", replacement = "")),
         Lon = as.numeric(str_extract(`Location 1`, "-122.[0-9]*")),
         `FINALED DATE` = mdy(`FINALED DATE`)) %>%
  filter(!is.na(Lon),
         !is.na(SUBAREA)) %>% 
  dplyr::select(Class,`PROJECT DESCRIPTION`,`FINALED DATE`,
         CONTRACTOR,`TOTAL PAID`,SUBAREA, `NEIGHBORHOOD AREA`, Lat, Lon) 

# View(totev)

## plot of install trend
ev %>% 
  mutate(year = year(`FINALED DATE`)) %>% 
  group_by(Class, year) %>% 
  summarise(install = n()) %>% 
  ggplot(aes(x = year, y = install, group = Class, color = Class))+
  geom_line(size = 1)+
  xlab("Year") + ylab("Number of installation") +
  theme_bw() +
  theme(legend.position = c(0.90, 0.25),
        legend.background = element_rect(fill="transparent")) +
  scale_x_continuous(breaks = seq(2000, 2019, by = 1))


## King county shapefile 
bell <- readOGR(dsn = "./data/raw", layer= "bellevue")
# plot(bell)
# View(bell@data)
# nb.map <- poly2nb(king)
# nb2INLA("king.graph", nb.map)
summary(bell)

View(ev)
par(mfrow=c(1,1))
write_csv(ev, path = "./data/derived/bell.csv" )

## Density plot
so <- as.data.frame(ev[,c(10,9)])
# plot(so,pch = 4, cex = 0.6)
evch <- as.ppp(so, W= owin(c(-122.25, -122.07), c(47.51, 47.68)))
# plot(evch)

tot <- as.data.frame(totev[,c(9,8)])
# plot(tot,pch = 4, cex = 0.6)
total <- as.ppp(tot, W= owin(c(-122.25, -122.07), c(47.51, 47.68)))
# plot(total) 

den <- as(as(density(evch, 0.004), "SpatialGridDataFrame"), "SpatialPixelsDataFrame")
plot(den, col=blue2red(30))
title("EV charger density in Bellevue")

dento <- as(as(density(total, 0.004), "SpatialGridDataFrame"), "SpatialPixelsDataFrame")
plot(dento)

# relative density
scaled_den = (den$v - mean(den$v))/ sd(den$v)
scaled_dento = (dento$v - mean(dento$v))/ sd(dento$v)
den1 <- den
den1@data$v <- (scaled_den - scaled_dento) 
plot(den1, col=blue2red(30))
title("EV charger density in Bellevue")

## Clustering check
evn <- envelope(evch, fun= Gest, nrank= 2, nsim= 99)
plot(evn, main="", legend= F, xlab= "Distance(d)", ylab= "G(d)")

## Color set
coul = colorRampPalette(rev(brewer.pal(11, "RdBu")))(18)
uni = colorRampPalette(brewer.pal(9, "YlOrBr"))(30)
like = matlab.like2

## Density of year 2012
ev2012 <- ev %>% 
  filter(`FINALED DATE` <= as.Date("2012-12-31"))

## Density plot
so <- as.data.frame(ev2012[,c(10,9)])
# plot(so,pch = 4, cex = 0.6)
evch <- as.ppp(so, W= owin(c(-122.25, -122.07), c(47.51, 47.68)))
# plot(evch)

den <- as(as(density(evch, 0.004), "SpatialGridDataFrame"), "SpatialPixelsDataFrame")
plot(den, col=blue2red(30))
title("EV charger density in Bellevue, 2012")

# relative density
scaled_den = (den$v - mean(den$v))/ sd(den$v)
scaled_dento = (dento$v - mean(dento$v))/ sd(dento$v)
den2 <- den
den2@data$v <- (scaled_den - scaled_dento) 
plot(den2, col=blue2red(30))
title("EV charger density in Bellevue, 2012")




# to be continued.. 

g_pvtren_dv <- inst %>% 
  mutate(year = year(CompletedDate)) %>% 
  group_by(Class, year) %>% 
  summarise(install = n()) %>%
  ggplot(aes(x = year, y = install, group = Class, color = Class))+
  geom_line(size = 1)+
  xlab("Year") + ylab("Number of installation") +
  theme_bw() +
  theme(legend.position = c(0.90, 0.25),
        legend.background = element_rect(fill="transparent")) +
  scale_x_continuous(breaks = seq(2000, 2019, by = 1))



sum_pe$value[is.na(sum_pe$value)] = 0

g_evtren_dv <- sum_pe %>% 
  ggplot(aes(x = year, y = value, group = Technology, color = Technology))+
  facet_wrap( ~ Class) +
  geom_line(size = 1)+
  xlab("Year") + ylab("Number of installation") +
  theme_bw() +
  theme(legend.position = c(0.90, 0.25),
        legend.background = element_rect(fill="transparent")) +
  scale_x_continuous(breaks = seq(2000, 2019, by = 1))

top_e <- ev_int %>% 
  group_by(Installer) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate('top installer' = ifelse(n > 25, "Y", "N"))

## plot
g_evcontr_dv <- ev_int %>% 
  mutate(year = year(CompletedDate)) %>% 
  group_by(Installer, year) %>% 
  summarise(install = n()) %>% 
  arrange(desc(install)) %>% 
  left_join(top_e, by = c("Installer")) %>% 
  filter(`top installer` == "Y") %>% 
  ggplot(aes(x = year, y = install, group = Installer, 
             color = Installer))+
  geom_line(size = 1)+
  xlab("Year") + ylab("Number of installation") +
  theme_bw() +
  theme(legend.position = c(0.6, 0.7),
        legend.background = element_rect(fill="transparent")) +
  scale_x_continuous(breaks = seq(2000, 2019, by = 1))

install_e <- ev_int %>% 
  select(-ContractorCompanyName, -Description)


## epv
epv <- inst %>% 
  distinct(Latitude, Longitude,.keep_all=TRUE) %>% 
  inner_join(ev_int %>% 
               distinct(Latitude, Longitude,.keep_all=TRUE), 
             by = c("Latitude", "Longitude")) %>% 
  mutate(id = 1:170)

glimpse(epv)
# View(epv)

## plot
g_epv <- epv %>% 
  ggplot(aes(x = CompletedDate.x, y = CompletedDate.y))+
  geom_point(alpha = 0.5, size = 2) +
  geom_abline(color = "red", size = 1) +
  geom_smooth(method = "lm", se = T) +
  xlab("PV date") + ylab("EV date") +
  theme_bw() +
  theme(legend.position = c(0.90, 0.25),
        legend.background = element_rect(fill="transparent"))

## after outliers removed 
# epv %>% 
#   filter(CompletedDate.x > as.Date("2012/01/01", "%Y/%m/%d"),
#          CompletedDate.y > as.Date("2012/01/01", "%Y/%m/%d")) %>% 
#   ggplot(aes(x = CompletedDate.x, y = CompletedDate.y))+
#   geom_point(alpha = 0.5, size = 2) +
#   geom_abline(color = "red", size = 1) +
#   geom_smooth(method = "lm", se = T) +
#   xlab("PV date") + ylab("EV date") +
#   theme_bw() +
#   theme(legend.position = c(0.90, 0.25),
#         legend.background = element_rect(fill="transparent"))

# epv %>% 
#   filter(CompletedDate.y >= as.Date("2011/01/01", "%Y/%m/%d"), 
#          CompletedDate.y < as.Date("2012/01/01", "%Y/%m/%d")) %>% View()

# write_csv(ev_int, path = "./data/derived/ev_int.csv" )

# epv/ pv = 171/3042
# epv/ ev = 171/854

# epv_pv <- dim(epv)[1]/dim(install)[1]
# epv_ev <- dim(epv)[1]/dim(ev_int)[1]
# 
# hhs = 300000
# pv = 3042
# no_pv = hhs - pv
# ev = 854
# no_ev = hhs - ev
# evpv = 171
# npv_ev = ev - evpv
# nev_pv = pv - evpv
# nevpv = hhs - pv - ev + evpv
#
# (evpv/pv)/(npv_ev/no_pv)
# (evpv/ev)/(nev_pv/no_ev)

# If you have a solar PV, then 24.4 times more you will have a EV. Among PV households, 5.6% have EV, among non PV households, 0.22% have EV.
# If you have a EV, then 20.8 times more you will have a PV. Among EV households, 20% have PV, among non EV households, 0.95% have PV.

# Assumptions to satisfy 
# * housing units are the same as households i.e., one solar PV or EV installed is counted as a household.
# * how many households are in a multifamily? one solar Pv or EV installed may serve more than a household.
# * currently, there are 76 (PV) and 53 (EV) permits in multifamily. 

# sum_pe %>% 
#   group_by(Class, Technology) %>% 
#   summarise(sum = sum(value))

save(ev_int, install_e, top_e, g_evcontr_dv, g_evtren_dv,
     epv, g_epv, file = "./data/derived/ev.Rdata")
write_csv(install_e, path = "./data/derived/install_e.csv" )

load("./data/derived/ev.Rdata")
