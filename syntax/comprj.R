library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(pander)
library(stringr)

load("./data/derived/prj1.Rdata")
instl <- read_csv(file = "./data/derived/spatial.csv")
po <- read_csv(file = "./data/derived/potential.csv")

regr <- regr %>% 
  left_join(po %>% 
              select(geoid, sol_instl), by = "geoid")

## data cleaning for time series
spa <- instl %>% 
  filter(!is.na(geoid)) %>% 
  mutate(date = str_replace(CompletedD, " 0:00$", "") %>% 
           mdy()) %>% 
  group_by(geoid, date) %>% 
  summarise(count = n()) %>% 
  mutate(sum = cumsum(count)) %>% 
  select(-count)

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
temp_spatial %>% 
  filter(date > as.Date("01/01/2005", "%m/%d/%Y")) %>% 
  ggplot(aes(x = date, y = sum, group = geoid, color = `Over 20`))+
  geom_line(size = 1, alpha = 0.4)+
  scale_color_manual(name ="Above 20 installation", values = c("red", "black"))+
  xlab("Year") + ylab("Cumulative number of installation") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill="transparent")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

save(regr, instl, temp_spatial, file = "./data/derived/prj3.Rdata")
