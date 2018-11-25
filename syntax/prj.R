library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(pander)
library(stringr)
data <- read_csv(file = "seeds_ii_replica.csv")
str(data)
head(data)
View(data)

county <- data %>% 
  filter(county_name =="King County") %>% 
  select(geoid,ends_with("hh"),ends_with("mwh"),hh_med_income,
         hh_gini_index,pop_total,hu_own,hu_rent,hu_vintage_2010toafter,lihtc_qualified,-pct_eli_hh,
         -ends_with("elep_hh"))
str(county)
head(county)
View(county)
write_csv(county, path = "county.csv")

county %>% 
  gather(class, mwh, very_low_mf_own_mwh:high_sf_rent_mwh) %>% 
  select(geoid, class, mwh) %>% 
  ggplot(aes(x = mwh)) +
  geom_histogram(binwidth = 2) +
  facet_wrap( ~ class, nrow = 5) +
  theme_minimal() + xlim(0,6000) + ylim(0,10)
  theme(strip.text.x = element_text(size = rel(0.6))) +
  ylab("Count of Incidents") + xlab("MWh")


county %>% 
  gather(class, hh, very_low_mf_own_hh:high_sf_rent_hh) %>% 
  select(geoid, class, hh) %>% 
  ggplot(aes(x = hh)) +
  geom_histogram(binwidth = 2) +
  facet_wrap( ~ class, nrow = 5) +
  theme_minimal() + xlim(0,1500) + ylim(0,50)
  theme(strip.text.x = element_text(size = rel(0.6))) +
  ylab("Count of Incidents") + xlab("Household count")


county_simple <- county %>% 
  gather(char,val, very_low_mf_own_hh:high_sf_rent_mwh) %>% 
  mutate(hu_rnt = hu_rent/ (hu_rent + hu_own),
         hu_blt2010 = hu_vintage_2010toafter/ (hu_rent + hu_own),
         incomeclass = case_when(
           substr(char, 1,4) == "very" ~ "verylow",
           substr(char, 1,3) == "low" ~ "low",
           substr(char, 1,3) == "mod" ~ "mod",
           substr(char, 1,3) == "mid" ~ "mid",
           substr(char, 1,4) == "high" ~ "high"),
         type = ifelse(str_detect(char, "mf"), "mf", "sf"),
         own = ifelse(str_detect(char, "own"), "own", "rent"),
         sep = ifelse(str_detect(char, "hh"), "hh", "mwh")) %>% 
  spread(sep, val) 

county_data <- county_simple %>% 
  filter(!is.na(hh)) %>% 
  select(geoid,incomeclass,type,own,hh) %>% 
  inner_join(county_simple %>% 
               filter(!is.na(mwh)), 
             by = c("geoid","incomeclass","type","own")) %>% 
  rename(hh = hh.x) %>% 
  select(geoid,incomeclass,type,own,hh,mwh,hh_med_income,
         hh_gini_index,hu_rnt,hu_blt2010,pop_total,lihtc_qualified)

write_csv(county_data, path = "county_data.csv")


data <- read_csv(file = "county_data.csv")
View(data)
class(data)
lapply(data, class) %>% 
  pander(style = "rmarkdown", caption = "summary table")

summary(data)
data$hh_med_income[is.na(data$hh_med_income)] = 12300
write_csv(data, path = "data.csv")

wrk <- data %>% 
  mutate(`Income class` = 
           parse_factor(ifelse(incomeclass %in% c("verylow", "low", "mod"), "low", "high"),
                        levels = NULL)) %>% 
  group_by(geoid, `Income class`, type, own) %>% 
  summarise(shh = sum(hh), "Total MWh" = sum(mwh)) %>% 
  mutate(`MWh/house hold` = `Total MWh`/shh)
View(wrk)
str(wrk)

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
View(eff)

snd <- wrk %>% 
  filter(`Income class` == "low" & type == "mf" & own == "rent") %>% 
  select(geoid, `MWh_low_mf_rnt` = `Total MWh`)
snd <- snd[,3:4]

trd <- wrk %>% 
  filter(`Income class` == "high" & type == "sf" & own == "own") %>% 
  select(geoid, `MWh_high_sf_own` =`Total MWh`)
trd <- trd[,3:4]

fin <- data %>% 
  filter(incomeclass == "low" & type == "mf" & own == "own") %>% 
  select(geoid, hh_med_income:lihtc_qualified) %>% 
  inner_join(eff, by = "geoid") %>% 
  inner_join(snd, by = "geoid") %>% 
  inner_join(trd, by = "geoid")
View(fin)
str(fin)
summary(fin)
fin <- fin[!is.na(fin$`MWh/hh_low_mf_own`),]
write_csv(fin, path = "fin.csv")

fhist <- fin[, c(-1,-7)]
par(mfrow=c(2,4))
for(i in 1:length(fhist)){
  hist(fhist[[i]], main= paste("Histogram of\n", names(fhist)[i]),
       xlab= names(fhist)[i], col="gold")
  abline(v = median(fhist[[i]]), col="red", lwd=4)
  text(median(fhist[[i]]), 0, round(median(fhist[[i]]),2), col = "blue")
}


