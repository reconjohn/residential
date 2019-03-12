# PV NW seed

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(pander)
library(stringr)
library(quantmod)
library(gridExtra)

load("./data/derived/iv.Rdata")

instl <- read_csv(file = "./data/raw/solar_aggre.csv") %>% 
  filter(!is.na(geoid)) %>% 
  mutate(Class = factor(Class)) %>% 
  mutate(geoid = as.character((geoid))) %>% 
  inner_join(regr, by = "geoid") %>% 
  select(Class, Description, CompletedDate, Latitude, Longitude, Installer, geoid, L_HOOD)
# glimpse(instl)
# View(instl)

## data cleaning for time series
spa <- instl %>% 
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
  mutate(tin = ifelse(max > 40, "above", "below")) %>% 
  select(-max) %>% 
  mutate(`Over 40` = parse_factor(tin, levels = c("above", "below"))) %>% 
  select(-tin)

temp_spatial <- spa %>% 
  left_join(temp, by= "geoid")

## plot! 
g_ts_ts <- temp_spatial %>% 
  filter(date > as.Date("01/01/2005", "%m/%d/%Y")) %>% 
  ggplot(aes(x = date, y = sum, group = geoid, color = `Over 40`))+
  geom_line(size = 1, alpha = 0.4)+
  scale_color_manual(name ="Above 40 installations", values = c("red", "black"))+
  xlab("Year") + ylab("Cumulative number of installation") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill="transparent")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")


temp_spatial %>% 
  filter(date > as.Date("01/01/2005", "%m/%d/%Y")) %>% 
  mutate(year = year(date)) %>% 
  group_by(year, geoid) %>% 
  summarise(install = sum(count)) %>% 
  ggplot(aes(x = year, y = install, group = geoid))+
  geom_line(size = 1, alpha = 0.4)+
  xlab("Year") + ylab("Cumulative number of installation") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill="transparent"))


g_nb_ts <- temp_spatial %>% 
  group_by(L_HOOD, date) %>% 
  summarise(total = sum(count)) %>% 
  mutate(sum = cumsum(total)) %>% 
  filter(date > as.Date("01/01/2005", "%m/%d/%Y")) %>% 
  ggplot(aes(x = date, y = sum, group = L_HOOD, color = L_HOOD))+
  geom_line(size = 1, alpha = 0.7)+
  scale_color_manual(name ="Impact of NW SEED (dotted line) on residential solar in Seattle", 
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
  theme(legend.position = c(0.5, 0.7),
        legend.background = element_rect(fill="transparent")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")


temp_spatial %>% 
  group_by(L_HOOD, date) %>% 
  summarise(total = sum(count)) %>% 
  mutate(sum = cumsum(total)) %>% 
  filter(date > as.Date("01/01/2005", "%m/%d/%Y")) %>% 
  mutate(year = semester(date, with_year = T)) %>% 
  group_by(year, L_HOOD) %>% 
  summarise(install = sum(total)) %>% 
  ggplot(aes(x = year, y = install, group = L_HOOD, color = L_HOOD))+
  geom_line(size = 1, alpha = 0.7)+
  scale_color_manual(name ="Impact of NW SEED (dotted line) on residential solar in Seattle", 
                     values = c("red","brown","blue","gold4", 
                                "dark green", "purple"))+
  xlab("Year") + ylab("Cumulative number of installation") +
  theme_bw() +
  theme(legend.position = c(0.5, 0.7),
        legend.background = element_rect(fill="transparent")) 


temp_spatial %>% 
  group_by(L_HOOD, date) %>% 
  summarise(total = sum(count)) %>% 
  mutate(sum = cumsum(total),
         sum_df = c(0, diff(sum))) %>% 
  filter(date > as.Date("01/01/2005", "%m/%d/%Y")) %>% 
  ggplot(aes(x = date, y = sum_df, group = L_HOOD, color = L_HOOD))+
  geom_point(size = 1, alpha = 0.7)+
  scale_color_manual(name ="Impact of NW SEED (dotted line) on residential solar in Seattle", 
                     values = c("red","brown","blue","gold4", 
                                "dark green", "purple"))+
  xlab("Year") + ylab("Cumulative number of installation") +
  ylim(0, 20) +
  theme_bw() +
  theme(legend.position = c(0.5, 0.7),
        legend.background = element_rect(fill="transparent")) 


nb <- temp_spatial %>% 
  distinct(L_HOOD, geoid) %>% 
  group_by((L_HOOD)) %>% 
  tally() 

nts <- temp_spatial %>% 
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

g_nbor_ts <- temp_spatial %>% 
  filter(date > as.Date("01/01/2005", "%m/%d/%Y")) %>% 
  ggplot(aes(x = date, y = sum, group = geoid))+
  geom_line(alpha = 0.5, aes(color = "Census track", size = "Census track")) +
  geom_line(data = nts %>% 
              filter(date > as.Date("01/01/2005", "%m/%d/%Y")),
            aes(x = date, y = mean, group = L_HOOD, color = "Mean", 
                size = "Mean"), alpha = 0.7) +
  facet_wrap(~ L_HOOD, nrow = 2) +
  scale_color_manual(name ="Solar installations",
                     values = c("Census track" = "black", "Mean" = "dodgerblue1")) +
  scale_size_manual(name ="Solar installations",
                    values = c("Census track" = 0.5, "Mean" = 2)) +
  xlab("Year") + ylab("Cumulative number of installation") +
  theme_bw() +
  theme(legend.position = c(0.1, 0.8),
        legend.background = element_rect(fill="transparent")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y")




# time impact (6 months)
# 30 vs. 29
p1 <- temp_spatial %>% 
  filter(L_HOOD %in% c("QUEENANNE","MAGNOLIA")) %>% 
  group_by(L_HOOD, date) %>% 
  summarise(total = sum(count)) %>% 
  mutate(sum = cumsum(total)) %>% 
  filter(date > as.Date("01/01/2005", "%m/%d/%Y")) %>% 
  ggplot(aes(x = date, y = sum, group = L_HOOD, color = L_HOOD))+
  geom_line(size = 1, alpha = 0.7)+
  scale_color_manual(name ="Impact of Time (6 months)\n # panels: 30 vs. 29", 
                     values = c("red","brown","blue","gold4", 
                                "dark green", "purple"))+
  xlab("Year") + ylab("") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill="transparent")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# 141 vs. 185
p2 <- temp_spatial %>% 
  filter(L_HOOD %in% c("NORTHWEST","CENTSOUTH")) %>% 
  group_by(L_HOOD, date) %>% 
  summarise(total = sum(count)) %>% 
  mutate(sum = cumsum(total)) %>% 
  filter(date > as.Date("01/01/2005", "%m/%d/%Y")) %>% 
  ggplot(aes(x = date, y = sum, group = L_HOOD, color = L_HOOD))+
  geom_line(size = 1, alpha = 0.7)+
  scale_color_manual(name ="Impact of Time (6 months)\n # panels: 141 vs. 185", 
                     values = c("gold4","dark green"))+
  xlab("Year") + ylab("Cumulative number of installation") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill="transparent")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# amount and time impact (2.5 years)
# 52 vs. 111
p3 <- temp_spatial %>% 
  filter(L_HOOD %in% c("NORTHEAST","WESTSEATTLE")) %>% 
  group_by(L_HOOD, date) %>% 
  summarise(total = sum(count)) %>% 
  mutate(sum = cumsum(total)) %>% 
  filter(date > as.Date("01/01/2005", "%m/%d/%Y")) %>% 
  ggplot(aes(x = date, y = sum, group = L_HOOD, color = L_HOOD))+
  geom_line(size = 1, alpha = 0.7)+
  scale_color_manual(name ="Impact of Time (2.5 years)\n # panels: 52 vs. 111", 
                     values = c("blue","purple"))+
  xlab("Year") + ylab("") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill="transparent")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")

grid.arrange(p1, p2, p3, nrow = 1)


## for GIS pro data 
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

# View(ts)
# glimpse(ts)

write_csv(ts, path = "./data/derived/ts.csv" )
save(instl, temp_spatial, ts, g_ts_ts, g_nb_ts, g_nbor_ts, p1, p2, p3, file = "./data/derived/ts.Rdata")

load("./data/derived/ts.Rdata")
