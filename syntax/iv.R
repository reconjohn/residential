# Social characteristics and solar potential

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(pander)
library(stringr)

data <- read_csv(file = "./data/raw/seeds_ii_replica.csv")
data1 <- read_csv(file = "./data/raw/ACS_15_5YR_B25003.csv")[-1,] %>% 
  rename(geoid = GEO.id2, hu = HD01_VD01, hu_own = HD01_VD02 ) %>% 
  select(geoid, hu, hu_own) %>% 
  mutate_all(funs(as.numeric))
data2 <- read_csv(file = "./data/raw/neighbor.csv") %>% 
  select(geoid, L_HOOD)
data3 <- read_csv(file = "./data/raw/ACS_15_5YR_S1101.csv")[-1,] %>% 
  rename(geoid = GEO.id2, single_unit = HC01_EST_VC27) %>% 
  mutate_all(funs(as.numeric)) %>% 
  mutate(single_unit = single_unit/ 100) %>% 
  select(geoid, single_unit)

# str(data)
# head(data)
# View(data)

## county
county <- data %>% 
  filter(county_name =="King County") %>% 
  select(geoid,ends_with("hh"),ends_with("mwh"),hh_med_income,
         hh_gini_index,pop_total,pop_non_us_citizen,pop_african_american,pop_caucasian,
         pop25_some_college_plus,
         hu_monthly_owner_costs_greaterthan_1000dlrs,
         hu_monthly_owner_costs_lessthan_1000dlrs,hu_med_val, hu_no_mortgage,
         hu_vintage_2010toafter, hu_vintage_2000to2009, hu_vintage_1980to1999,
         hu_vintage_1960to1970, hu_vintage_1940to1959, hu_vintage_1939toearlier,lihtc_qualified,
         -pct_eli_hh,-ends_with("elep_hh"))

county$hh_med_income[is.na(county$hh_med_income)] = mean(county$hh_med_income, na.rm= T)
county$hu_med_val[is.na(county$hu_med_val)] = mean(county$hu_med_val, na.rm= T)

## Seattle 
seattle <- county %>% 
  filter(geoid >= 53033000100, geoid <= 53033026500) %>% 
  left_join(data1, by = "geoid") %>% 
  left_join(data3, by = "geoid")
# summary(seattle)

seattle_simple <- seattle %>% 
  gather(char,val, very_low_mf_own_hh:high_sf_rent_mwh) %>% 
  mutate(hu_own = hu_own/ hu,
         hu_blt1979 = (hu_vintage_1960to1970+hu_vintage_1940to1959+hu_vintage_1939toearlier)/ 
           (hu_vintage_2010toafter+ hu_vintage_2000to2009+ hu_vintage_1980to1999+
            hu_vintage_1960to1970+ hu_vintage_1940to1959+ hu_vintage_1939toearlier),
         hu_no_mor = hu_no_mortgage/ hu,
         hu_med_val = hu_med_val/max(hu_med_val),
         hu_ex_1000 = hu_monthly_owner_costs_greaterthan_1000dlrs/ hu,
         edu = pop25_some_college_plus/ pop_total,
         wh_race = pop_caucasian/ pop_total,
         af_race = pop_african_american/ pop_total,
         non_us = pop_non_us_citizen/ pop_total,
         hh_med_income = hh_med_income/ max(hh_med_income),
         lihtc = factor(lihtc_qualified), 
         incomeclass = case_when(
           substr(char, 1,4) == "very" ~ "verylow",
           substr(char, 1,3) == "low" ~ "low",
           substr(char, 1,3) == "mod" ~ "mod",
           substr(char, 1,3) == "mid" ~ "mid",
           substr(char, 1,4) == "high" ~ "high"),
         type = ifelse(str_detect(char, "mf"), "mf", "sf"),
         own = ifelse(str_detect(char, "own"), "own", "rent"),
         sep = ifelse(str_detect(char, "hh"), "hh", "mwh")) %>% 
  spread(sep, val) %>% 
  select(geoid, incomeclass, type, own, hh, hu, mwh, 
         hu_own, single_unit, hu_blt1979, hu_no_mor,hu_med_val, hu_ex_1000, 
         edu, wh_race, af_race, non_us, hh_med_income, hh_gini_index,lihtc) %>% 
  mutate_at(vars(type,own), funs(factor)) 

seattle_simple <- seattle_simple %>% 
  filter(!is.na(hh)) %>% 
  select(geoid, incomeclass, type, own, hh) %>% 
  inner_join(seattle_simple %>% 
               filter(!is.na(mwh)) %>% 
               select(-hh), by = c("geoid", "incomeclass", "type", "own")) %>% 
  inner_join(data2, by = "geoid") 

high_income <- seattle_simple %>% 
  group_by(geoid, incomeclass) %>% 
  summarise(tot = sum(hh), hu = mean(hu)) %>% 
  filter(incomeclass == "high") %>% 
  mutate(high_income = tot/ hu) %>% 
  select(geoid, high_income)

seattle_simple <- seattle_simple %>% 
  left_join(high_income, by = "geoid")

# View(seattle_simple)
# str(seattle_simple)
# summary(seattle_simple)

## summary 
wrk <- seattle_simple %>% 
  mutate(`Income class` = 
           parse_factor(ifelse(incomeclass %in% c("verylow", "low", "mod"), "low", incomeclass),
                        levels = c("low", "mid", "high"))) %>% 
  group_by(geoid, `Income class`, type, own) %>% 
  summarise(shh = sum(hh), "Total MWh" = sum(mwh)) %>% 
  mutate(`MWh/house hold` = `Total MWh`/shh) %>% 
  mutate(`MWh/house hold` = ifelse(is.nan(`MWh/house hold`), 0, `MWh/house hold`))

pot <- wrk %>% 
  left_join(wrk %>% 
              group_by(geoid) %>% 
              summarise(hh_tot = sum(shh)), by = "geoid") %>% 
  mutate(hh_prp = shh/ hh_tot)

## ggplot 
g_mwh_iv <- ggplot(pot, aes(x = `Income class`, y = `Total MWh`, group = geoid, color = `Income class`)) + 
  facet_grid(type ~ own) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 2)) + 
  theme_bw() + theme(legend.position = c(0.7, 0.3),
                     legend.background = element_rect(color = 1))

## ggplot 
g_mwhu_iv <- ggplot(pot, aes(x = `Income class`, y = `MWh/house hold`, group = geoid, color = `Income class`)) + 
  facet_grid(type ~ own) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.5, height = 2)) + 
  theme_bw() + theme(legend.position = c(0.2, 0.3),
                     legend.background = element_rect(color = 1))

## ggplot 
g_hh_iv <- ggplot(pot, aes(x = `Income class`, y = hh_prp, group = geoid, color = `Income class`)) + 
  facet_grid(type ~ own) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.5)) + 
  theme_bw() + ylab("Household proportion") +
  theme(legend.position = c(0.8, 0.3),
                     legend.background = element_rect(color = 1))

rich <- pot %>% 
  filter(`Income class` == "high" & type == "sf" & own == "own") %>% 
  select(geoid, `hh_high_sf_own` = hh_prp)
rich <- rich[,3:4]

poor <- pot %>% 
  filter(`Income class` == "low" & type == "mf" & own == "rent") %>% 
  select(geoid, hh_low_mf_rent = hh_prp)
poor <- poor[,3:4]

hu_mwh <- pot %>% 
  group_by(geoid) %>% 
  summarise(hu_mwh = sum(`MWh/house hold`))

regr <- seattle_simple %>% 
  filter(incomeclass == "low", type == "mf", own == "rent") %>% 
  inner_join(rich, by = "geoid") %>% 
  inner_join(poor, by = "geoid") %>% 
  inner_join(hu_mwh, by = "geoid") %>% 
  select(-incomeclass, -type, -own, -hh, -mwh) %>% 
  mutate(geoid = as.character(geoid))


# Scaling 

stad <- function(var){
  (var - mean(var))/sd(var)
}

temp <- regr[c(-1,-2,-15,-16,-18:-20)] 
regr_scale <- sapply(temp, stad) %>% as.data.frame()
regr_scale$geoid <- regr$geoid
regr_scale$hu <- regr$hu

# View(regr_scale)
# summary(regr_scale)
# str(regr_scale)
# class(regr_scale)

## Plot
# fhist <- regrs[, c(-1, -11)]
# par(mfrow=c(3,4))
# g_tab_ts <- for(i in 1:length(fhist)){
#   hist(fhist[[i]], main= paste("Histogram of\n", names(fhist)[i]),
#        xlab= names(fhist)[i], col="gold")
#   abline(v = median(fhist[[i]]), col="red", lwd=4)
#   text(median(fhist[[i]]), 0, round(median(fhist[[i]]),2), col = "blue")
# }

# source("./syntax/dv_table.R")

# ## for testing 
# m <- regr[, -11]
# m %>% 
#   gather(class, value, hu_own:hu_mwh) %>% 
#   ggplot(aes(x = value)) +
#   geom_histogram(aes(y = ..density..), binwidth = 0.02) +
#   facet_wrap( ~ class) +
#   scale_x_log10()
# 
# seattle %>% 
#   gather(class, hh, very_low_mf_own_hh:high_sf_rent_hh) %>% 
#   select(geoid, class, hh) %>% 
#   ggplot(aes(x = hh)) +
#   geom_histogram(binwidth = 50) +
#   facet_wrap( ~ class, nrow = 5) +
#   theme_minimal() + xlim(-100,1500)
# 
# seattle %>% 
#   gather(class, mwh, very_low_mf_own_mwh:high_sf_rent_mwh) %>% 
#   select(geoid, class, mwh) %>% 
#   mutate(class = parse_factor(class, levels = names(seattle))) %>% 
#   mutate(cl = ifelse(str_detect(class, "verylow|low|mod"), "low",
#                                  ifelse(str_detect(class, "mid"), "mid", 
#                                         ifelse(str_detect(class, "high"), "high", NA)))) %>%
#   mutate(cl = parse_factor(cl, levels = c("low", "mid", "high"))) %>% 
#   ggplot(aes(x = class, y = mwh, color = class)) +
#   geom_boxplot() +
#   facet_wrap( ~ cl) +
#   scale_y_log10() +
#   theme_minimal() +
#   theme(axis.text.x = element_blank(),
#       axis.ticks.x = element_blank())

save(county, seattle, seattle_simple, pot, regr, regr_scale, g_hh_iv, g_mwhu_iv, g_mwh_iv, file = "./data/derived/iv.Rdata")

load("./data/derived/iv.Rdata")


  
