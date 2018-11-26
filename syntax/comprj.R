library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(pander)
library(stringr)

data <- read_csv(file = "./data/drived/county_data.csv")
View(data)
class(data)

lapply(data, class) %>% 
  pander(style = "rmarkdown", caption = "summary table")

summary(data)
data$hh_med_income[is.na(data$hh_med_income)] = 7115

wrk <- data %>% 
  filter(geoid >= 53033000100, geoid <= 53033012100) %>% 
  mutate(`Income class` = 
           parse_factor(ifelse(incomeclass %in% c("verylow", "low", "mod"), "low", incomeclass),
                        levels = c("low", "mid", "high"))) %>% 
  group_by(geoid, `Income class`, type, own) %>% 
  summarise(shh = sum(hh), "Total MWh" = sum(mwh)) %>% 
  mutate(`MWh/house hold` = `Total MWh`/shh) %>% 
  mutate(`MWh/house hold` = ifelse(is.nan(`MWh/house hold`), 0, `MWh/house hold`))
View(wrk)
str(wrk)
summary(wrk)

ggplot(wrk, aes(x = `Income class`, y = `Total MWh`, group = geoid, color = `Income class`)) + 
  facet_grid(type ~ own) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 2)) + 
  theme_bw() + theme(legend.position = c(0.7, 0.3),
                     legend.background = element_rect(color = 1))

ggplot(wrk, aes(x = `Income class`, y = `MWh/house hold`, group = geoid, color = `Income class`)) + 
  facet_grid(type ~ own) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 2)) + 
  theme_bw() + theme(legend.position = c(0.2, 0.3),
                     legend.background = element_rect(color = 1))

eff <- wrk %>% 
  filter(`Income class` == "low" & type == "mf" & own == "own") %>% 
  select(geoid, `MWh/hh_low_mf_own` = `MWh/house hold`)
eff <- eff[,3:4]

snd <- wrk %>% 
  filter(`Income class` == "low" & type == "mf" & own == "rent") %>% 
  select(geoid, `MWh_low_mf_rnt` = `Total MWh`)
snd <- snd[,3:4]

trd <- wrk %>% 
  filter(`Income class` == "high" & type == "sf" & own == "own") %>% 
  select(geoid, `MWh_high_sf_own` =`Total MWh`)
trd <- trd[,3:4]

orig <- read_csv(file = "./data/raw/seeds_ii_replica.csv")
orig <- orig %>% 
  select(geoid, pop25_some_college_plus, pop_non_us_citizen, pop_african_american, hu_med_val)

c <- read_csv(file = "./data/raw/count.csv")
c1 <- read_csv(file = "./data/raw/count1.csv")

c <- c %>% 
  left_join(c1, by = "OBJECTID") %>% 
  select(Join_Count, GEO_ID_TRT)

fin <- data %>% 
  filter(incomeclass == "low" & type == "mf" & own == "own") %>% 
  select(geoid, hu, hh_med_income:lihtc_qualified) %>% 
  inner_join(eff, by = "geoid") %>% 
  inner_join(snd, by = "geoid") %>% 
  inner_join(trd, by = "geoid") %>% 
  inner_join(orig, by = "geoid") %>% 
  mutate(edu = pop25_some_college_plus/ pop_total, citizen = pop_non_us_citizen/ pop_total, 
         black = pop_african_american/ pop_total) %>% 
  select(-pop25_some_college_plus, -pop_non_us_citizen, -pop_african_american) %>% 
  left_join(c, by = c("geoid" = "GEO_ID_TRT")) %>% 
  mutate(sol_instl = Join_Count/ hu * 1000) %>% 
  select(-Join_Count)

View(fin)
str(fin)
summary(fin)
fin <- fin[!is.na(fin$hu_med_val),] 
write_csv(fin, path = "./data/drived/potential.csv")

fhist <- fin[, c(-1,-7)]
par(mfrow=c(2,2))
for(i in 1:length(fhist)){
  hist(fhist[[i]], main= paste("Histogram of\n", names(fhist)[i]),
       xlab= names(fhist)[i], col="gold")
  abline(v = median(fhist[[i]]), col="red", lwd=4)
  text(median(fhist[[i]]), 0, round(median(fhist[[i]]),2), col = "blue")
}


spatial <- read_csv(file = "./data/raw/spatial.csv")
str(spatial)
head(data)
View(spa)

spa <- spatial %>% 
  rename(geoid = fin__geoid) %>% 
  filter(!is.na(geoid)) %>% 
  mutate(date = str_replace(CompletedD, " 0:00$", "") %>% 
           mdy()) %>% 
  group_by(geoid, date) %>% 
  summarise(count = n()) %>% 
  mutate(sum = cumsum(count))

temp <- spa %>% 
  group_by(geoid) %>% 
  summarise(max = max(sum)) %>% 
  mutate(above20 = ifelse(max > 20, "top", "nope")) %>% 
  select(-max)

temp$above20 <- parse_factor(temp$above20, levels = c("top", "nope"))

temp_spatial <- spa %>% 
  left_join(temp, by= "geoid")

write_csv(temp_spatial, path = "./data/drived/temp_spatial.csv")
  
spa %>% 
  filter(date > as.Date("01/01/2005", "%m/%d/%Y")) %>% 
  ggplot(aes(x = date, y = sum, group = geoid, color = above20))+
  geom_line(size = 1, alpha = 0.4)+
  scale_color_manual(name ="Above 20 installation", values = c("red", "black"))+
  xlab("Year") + ylab("Cumulative number of installation") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill="transparent")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")


