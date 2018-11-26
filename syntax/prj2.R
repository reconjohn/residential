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

elec <- data %>% 
  filter(StatusCurrent == "Completed",
         str_detect(Description, "Solar|solar|SOLAR"),
         !is.na(CompletedDate)) %>% 
  mutate(PermitClass = parse_factor(PermitClass, levels = NULL)) %>% 
  mutate(Class = recode(PermitClass, `Single Family/Duplex` = "Single family",
                        Multifamiliy = "multifamily")) %>% 
  select(Class,Description,CompletedDate,
         ContractorCompanyName,Latitude,Longitude)

### plot
elec %>% 
  mutate(year = year(CompletedDate)) %>% 
  group_by(Class, year) %>% 
  summarise(install = n()) %>% 
  ggplot(aes(x = year, y = install, group = Class, color = Class))+
  geom_line()+
  xlab("Year") + ylab("Number of installation") +
  theme_bw() +
  theme(legend.position = c(0.90, 0.25),
        legend.background = element_rect(fill="transparent")) +
  scale_x_continuous(breaks = seq(2000, 2019, by = 1))
  

tt <-elec %>% 
  group_by(ContractorCompanyName) %>% 
  summarize(n_events = n()) %>% 
  arrange(desc(n_events))

elec <- elec %>% 
  mutate(Installer = parse_factor(ContractorCompanyName, 
                                  levels = tt$ContractorCompanyName))

top <- elec %>% 
  group_by(Installer) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  mutate('top installer' = ifelse(n > 34, "Y", "N"))

### plot
elec %>% 
  mutate(year = year(CompletedDate)) %>% 
  group_by(Installer, year) %>% 
  summarise(install = n()) %>% 
  arrange(desc(install)) %>% 
  left_join(top, by = c("Installer")) %>% 
  filter(`top installer` == "Y") %>% 
  ggplot(aes(x = year, y = install, group = Installer, 
             color = Installer))+
  geom_line(size = 2)+
  xlab("Year") + ylab("Number of installation") +
  theme_bw() +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill="transparent")) +
  scale_x_continuous(breaks = seq(2000, 2019, by = 1))

install <- elec %>% 
  select(-ContractorCompanyName, -Description)

save(elec, install, top, file = "./data/drived/install.Rdata")
