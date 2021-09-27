#### Vergleich Neuinfektionen und Impffortschritt in den Schweizer Kantonen #### 

#prep
rm(list=ls(all=TRUE)) # Alles bisherige im Arbeitssprecher loeschen
options(scipen=999)
library(tidyverse)
library(ggrepel)
library(jsonlite)

#### Create standard plot: new covid infections over past 14 days per 100k vs. share fully vaccinated people ####

### read-in

bag_data <- fromJSON('https://www.covid19.admin.ch/api/data/context')

bag_cases <- subset(read_csv(bag_data$sources$individual$csv$daily$cases), select = c("geoRegion", "datum", "entries", "sumTotal", "pop")) %>%
  filter(datum <= as.Date("2021-09-22")) #filter data that arrived after publication
names(bag_cases) <- c('geoRegion', 'datum', 'Fälle', 'Fälle kumuliert', 'pop')

bag_hosps <- subset(read_csv(bag_data$sources$individual$csv$daily$hosp), select = c("geoRegion", "datum", "entries", "sumTotal", "pop")) %>%
  filter(datum <= as.Date("2021-09-22"))
names(bag_hosps) <- c('geoRegion', 'datum', 'Fälle', 'Fälle kumuliert', 'pop')

bag_testPcrAntigen <- read_csv(bag_data$sources$individual$csv$daily$testPcrAntigen) %>% 
  select("geoRegion", "datum", "entries", "entries_pos", "entries_neg", "nachweismethode", "pos_anteil") %>%
  filter(datum <= as.Date("2021-09-22"))

bag_tests <- subset(read_csv(bag_data$sources$individual$csv$daily$test), select = c("geoRegion", "datum", "entries", "pos_anteil", "sumTotal", "pop")) %>%
  filter(datum <= as.Date("2021-09-22"))
names(bag_tests) <- c('geoRegion', 'datum', 'Tests', 'pos_anteil', 'Tests kumuliert', "pop")

ch_vacc_del <- read_csv(bag_data$sources$individual$csv$vaccDosesDelivered) %>% 
  filter(type == "COVID19VaccDosesDelivered") %>%
  filter(date <= as.Date("2021-09-22")) %>%
  select(geoRegion, date,pop, sumTotal) %>%
  drop_na()

ch_vacc_adm <- read_csv(bag_data$sources$individual$csv$vaccDosesAdministered) %>% 
  select(geoRegion, date, sumTotal, per100PersonsTotal) %>%
  filter(date <= as.Date("2021-09-22")) %>%
  drop_na()

ch_vacc_full <-read_csv(bag_data$sources$individual$csv$vaccPersons) %>%
  filter(type == "COVID19FullyVaccPersons") %>%
  filter(date <= as.Date("2021-09-22")) %>%
  select(geoRegion, date, sumTotal) %>%
  drop_na()

#data prep
bag_kanton_choro <- bag_cases %>%
  filter(!is.na(datum), datum >= last(datum)-13, geoRegion != "CHFL", geoRegion != "CH", geoRegion != "FL") %>%
  group_by(geoRegion, pop) %>%
  summarise(sum = sum(`Fälle`)) %>%
  mutate(per100k = round(100000*sum/pop, 0)) %>%
  arrange(geoRegion) %>%
  select(geoRegion, per100k)

ch_vacc <- ch_vacc_adm %>%
  full_join(ch_vacc_del, by = c("geoRegion", "date")) %>%
  full_join(ch_vacc_full, by = c("geoRegion", "date")) %>%
  select(geoRegion, date, pop, sumTotal.y, sumTotal.x, sumTotal, per100PersonsTotal)%>%
  rename(geounit = geoRegion, 
         ncumul_delivered_doses = sumTotal.y, 
         ncumul_vacc_doses = sumTotal.x, 
         ncumul_fully_vacc = sumTotal) %>%
  group_by(geounit) %>%
  mutate(new_vacc_doses = ncumul_vacc_doses-lag(ncumul_vacc_doses))%>%
  mutate(ncumul_firstdoses_vacc = ncumul_vacc_doses - ncumul_fully_vacc) %>%
  mutate(ncumul_onlyfirstdoses_vacc = ncumul_firstdoses_vacc - ncumul_fully_vacc) %>%
  fill(pop, ncumul_delivered_doses) %>%
  ungroup()


vacc_ch_2nd <- ch_vacc %>%
  select(geounit, date, pop, ncumul_fully_vacc, ncumul_onlyfirstdoses_vacc) %>%
  drop_na(ncumul_fully_vacc) %>%
  filter(geounit != "CH" & geounit != "FL" & geounit != "CHFL" & date == max(date)-13) %>%
  mutate(first_pct = ncumul_onlyfirstdoses_vacc*100/pop,
         second_pct = ncumul_fully_vacc*100/pop) %>%
  select(geounit, second_pct, pop) %>%
  slice(1:26)


### combine and plot case and vaccination data
combined <- full_join(vacc_ch_2nd, bag_kanton_choro, by = c("geounit" = "geoRegion"))
