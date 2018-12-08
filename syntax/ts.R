library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(pander)
library(stringr)
library('quantmod')

instl <- read_csv(file = "./data/derived/spatial.csv")

## data cleaning for time series
spa <- instl %>% 
  filter(!is.na(geoid)) %>% 
  mutate(date = str_replace(CompletedD, " 0:00$", "") %>% 
           mdy()) %>% 
  group_by(geoid, date) %>% 
  summarise(count = n()) %>% 
  mutate(sum = cumsum(count))


View(spa)
temp <- spa %>% 
  group_by(geoid) %>% 
  summarise(max = max(sum)) %>% 
  mutate(tin = ifelse(max > 20, "top", "below")) %>% 
  select(-max) %>% 
  mutate(`Over 20` = parse_factor(tin, levels = c("top", "below"))) %>% 
  select(-tin)

temp_spatial <- spa %>% 
  left_join(temp, by= "geoid")

## plot! 
g_ts_ts <- temp_spatial %>% 
  filter(date > as.Date("01/01/2005", "%m/%d/%Y")) %>% 
  ggplot(aes(x = date, y = sum, group = geoid, color = `Over 20`))+
  geom_line(size = 1, alpha = 0.4)+
  scale_color_manual(name ="Above 20 installations", values = c("red", "black"))+
  xlab("Year") + ylab("Cumulative number of installation") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill="transparent")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

str(temp_spatial)
t <- temp_spatial %>% 
  mutate(year = year(date)) %>% 
  group_by(geoid, year) %>% 
  summarise(sum = sum(count)) %>% 
  spread(geoid, sum)

t[is.na(t)] <- 0

ts <- t %>% 
  gather(geoid,value, -year ) %>% 
  group_by(geoid, year) %>% 
  summarise(value = value) %>% 
  mutate(sum = cumsum(value)) %>% 
  mutate(year = as.Date(str_c("01/01/", as.character(year)), format = "%m/%d/%Y"))

write_csv(ts, path = "./data/derived/ts.csv" )
save(instl, temp_spatial, ts, g_ts_ts, file = "./data/derived/ts.Rdata")

load("./data/derived/ts.Rdata")
