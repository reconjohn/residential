# NW seed on EV

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(pander)
library(stringr)
library('quantmod')

load("./data/derived/iv.Rdata")

ej <- read_csv(file = "./data/raw/ejoin.csv") %>% 
  filter(!is.na(geoid)) %>% 
  mutate(Class = factor(Class)) %>% 
  mutate(geoid = as.character((geoid))) %>% 
  inner_join(regr, by = "geoid") %>% 
  select(Class, CompletedDate, Latitude, Longitude, Installer, geoid, L_HOOD)
# glimpse(ej)
# View(ej)

## data cleaning for time series
spa <- ej %>% 
  filter(!is.na(geoid)) %>% 
  mutate(date = mdy(CompletedDate),
         L_HOOD = parse_factor(L_HOOD, levels = c("QUEENANNE","MAGNOLIA","NORTHEAST","NORTHWEST",
                                                  "CENTSOUTH","WESTSEATTLE"))) %>% 
  group_by(geoid, L_HOOD, date) %>% 
  summarise(count = n()) %>% 
  mutate(sum = cumsum(count))
# View(spa)

temp <- spa %>% 
  group_by(geoid) %>% 
  summarise(max = max(sum)) %>% 
  mutate(tin = ifelse(max > 10, "above", "below")) %>% 
  select(-max) %>% 
  mutate(`Over 10` = parse_factor(tin, levels = c("above", "below"))) %>% 
  select(-tin)

temp_spatiale <- spa %>% 
  left_join(temp, by= "geoid")
# View(temp_spatiale)

## plot! 
g_ts_ets <- temp_spatiale %>% 
  filter(date > as.Date("01/01/2010", "%m/%d/%Y")) %>% 
  ggplot(aes(x = date, y = sum, group = geoid, color = `Over 10`))+
  geom_line(size = 1, alpha = 0.4)+
  scale_color_manual(name ="Above 10 installations", values = c("red", "black"))+
  xlab("Year") + ylab("Cumulative number of installation") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill="transparent")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

g_nb_ets <- temp_spatiale %>% 
  group_by(L_HOOD, date) %>% 
  summarise(total = sum(count)) %>% 
  mutate(sum = cumsum(total)) %>% 
  filter(date > as.Date("01/01/2010", "%m/%d/%Y")) %>% 
  ggplot(aes(x = date, y = sum, group = L_HOOD, color = L_HOOD))+
  geom_line(size = 1, alpha = 0.7)+
  scale_color_manual(name ="No impact of NW SEED (dotted line) on EV in Seattle", 
                     values = c("red","brown","blue","gold4", 
                                "dark green", "purple"))+
  geom_vline(xintercept = as.numeric(as.Date("2011-04-01", "%Y-%m-%d")),
             col = "red", size = 2, alpha = 0.5, linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2011-10-01", "%Y-%m-%d")),
             col = "brown", size = 2, alpha = 0.5, linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2012-04-01", "%Y-%m-%d")),
             col = "blue", size = 2,alpha = 0.5, linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2013-04-01", "%Y-%m-%d")),
             col = "gold4", size = 2,alpha = 0.5, linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2013-10-01", "%Y-%m-%d")),
             col = "dark green", size = 2,alpha = 0.5, linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2014-10-01", "%Y-%m-%d")),
             col = "purple", size = 2,alpha = 0.5, linetype = 3) +
  xlab("Year") + ylab("Cumulative number of installation") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill="transparent")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

g_to_ets <- temp_spatiale %>% 
  group_by(date) %>% 
  summarise(total = sum(count)) %>% 
  mutate(sum = cumsum(total)) %>% 
  filter(date > as.Date("01/01/2010", "%m/%d/%Y")) %>% 
  ggplot(aes(x = date, y = sum))+
  geom_line(size = 1, alpha = 0.7)+
  xlab("Year") + ylab("Cumulative number of EV charger installation") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill="transparent")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

nb <- temp_spatiale %>% 
  distinct(L_HOOD, geoid) %>% 
  group_by((L_HOOD)) %>% 
  tally() 

nts <- temp_spatiale %>% 
  group_by(L_HOOD, date) %>% 
  summarise(total = sum(count)) %>% 
  mutate(sum = cumsum(total)) %>% 
  mutate(mean = case_when(
    L_HOOD == "QUEENANNE" ~ sum/ nb[[1,2]],
    L_HOOD == "MAGNOLIA" ~ sum/ nb[[2,2]],
    L_HOOD == "NORTHEAST" ~ sum/ nb[[3,2]],
    L_HOOD == "NORTHWEST" ~ sum/ nb[[4,2]],
    L_HOOD == "CENTSOUTH" ~ sum/ nb[[5,2]],
    L_HOOD == "WESTSEATTLE" ~ sum/ nb[[6,2]]
  ))

g_nbor_ets <- temp_spatiale %>% 
  filter(date > as.Date("01/01/2010", "%m/%d/%Y")) %>% 
  ggplot(aes(x = date, y = sum, group = geoid))+
  geom_line(alpha = 0.5, aes(color = "Census track", size = "Census track")) +
  geom_line(data = nts %>% 
              filter(date > as.Date("01/01/2010", "%m/%d/%Y")), 
            aes(x = date, y = mean, group = L_HOOD, color = "Mean", 
                            size = "Mean"), alpha = 0.7) +
  facet_wrap(~ L_HOOD, nrow = 2) +
  scale_color_manual(name ="EV charger installations",
                     values = c("Census track" = "black", "Mean" = "dodgerblue1")) +
  scale_size_manual(name ="EV charger installations",
                    values = c("Census track" = 0.5, "Mean" = 2)) +
  xlab("Year") + ylab("Cumulative number of installation") +
  theme_bw() +
  theme(legend.position = c(0.1, 0.8),
        legend.background = element_rect(fill="transparent")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y")


save(ej, temp_spatiale, g_ts_ets, g_nb_ets, g_to_ets, g_nbor_ets, file = "./data/derived/ets.Rdata")

load("./data/derived/ets.Rdata")
