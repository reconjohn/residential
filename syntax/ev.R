# EV installation and EV vs. PV 

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(pander)
library(stringr)

data <- read_csv(file = "./data/raw/Electrical_Permits.csv")
str(data)
head(data)
View(data)

ev <- data %>% 
  filter(StatusCurrent == "Completed",
         str_detect(Description, "EV CHARGER|EV CHARGING|CAR CHARGER|CAR CHARGING|
                    CHARGING STATION|CHARGING STATIONS|VEHICLE"),
         PermitClassMapped == "Residential",
         !is.na(CompletedDate)) %>% 
  mutate(PermitClass = parse_factor(PermitClass, 
                                    levels = c("Single Family/Duplex", "Multifamily"))) %>% 
  mutate(Class = recode(PermitClass, `Single Family/Duplex` = "Single family",
                        Multifamiliy = "Multifamily")) %>% 
  mutate(ContractorCompanyName = recode(ContractorCompanyName, 
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
                                        `SUNERGY SYSTEMS INC.` = "SUNERGY SYSTEMS")) %>% 
  select(Class,Description,CompletedDate,
         ContractorCompanyName,Latitude,Longitude) 

lv <- ev %>% 
  group_by(ContractorCompanyName) %>% 
  summarize(n_events = n()) %>% 
  arrange(desc(n_events))

ev_int <- ev %>% 
  mutate(Installer = parse_factor(ContractorCompanyName, levels = lv[[1]]))

levels(ev_int$Installer)

## plot
sum_pe <- inst %>% 
  mutate(year = year(CompletedDate)) %>% 
  group_by(Class, year) %>% 
  summarise(PV = n()) %>% 
  left_join(ev_int %>% 
              mutate(year = year(CompletedDate)) %>% 
              group_by(Class, year) %>% 
              summarise(EV = n()), by = c("Class", "year")) %>% 
  gather(Technology, value, PV, EV) %>% 
  mutate(Technology = parse_factor(Technology, levels = c("PV", "EV")))

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
