library(tidyverse)

#############################
# This calculates the sampling frequency from the actual data for all LTER sites
# It matches the documented sampling frequency of 30 days for the Jornada and
# 7 days for Harvard, HJ Andrews, and Hubbard Brook

harvard = read_csv('raw_data/harvard_forest/hf003-03-spring.csv') %>%
  mutate(year=lubridate::year(date)) %>%
  select(doy=julian, year) %>%
  distinct() %>%
  mutate(dataset='harvard',site_id='1')

jornada = read_csv('raw_data/jornada/JornadaStudy_287_npp_perennial_plant_phenology_0.csv', skip=45) %>%
  mutate(date = as.Date(DATE, '%m/%d/%Y'), year=lubridate::year(date), doy = lubridate::yday(date)) %>%
  select(year, doy, site_id = SITE) %>%
  distinct() %>%
  mutate(dataset='jornada')

hjandrews = read_csv('raw_data/hjandrews/TV07501_v1_without_encoding.csv') %>%
  mutate(year = lubridate::year(SAMPLEDATE), doy=lubridate::yday(SAMPLEDATE)) %>%
  select(year, doy, site_id = PLOT) %>%
  distinct() %>%
  mutate(dataset='hjandrews')

hubbard = read_csv('raw_data/hubbard_brook/phn.txt') %>%
  gather(site, status, -Date, -DAY, -SEASON, -SPECIES) %>%
  filter(SEASON=='SPRING') %>%
  mutate(year=lubridate::year(Date)) %>%
  select(year, doy=DAY, site_id = site) %>%
  distinct() %>%
  mutate(dataset='hubbard')

sampling_dates = harvard %>%
  bind_rows(jornada) %>%
  bind_rows(hjandrews) %>%
  bind_rows(hubbard)

################################################
sampling_dates = sampling_dates %>%
  group_by(dataset, site_id, year) %>%
  arrange(doy) %>%
  mutate(sample_id = 1:n())

sampling_dates_offset = sampling_dates %>%
  mutate(sample_id = sample_id - 1) %>%
  rename(next_doy=doy)

sampling_dates = sampling_dates %>%
  left_join(sampling_dates_offset, by=c('year','dataset','site_id','sample_id')) %>%
  mutate(sampling_interval = next_doy - doy)

####################################################

sampling_frequency = sampling_dates %>%
  group_by(dataset) %>%
  summarise(frequency = mean(sampling_interval, na.rm=T))

sampling_years = sampling_dates %>%
  group_by(dataset) %>%
  summarise(num_unique_years = n_distinct(year),
            first_year = min(year),
            last_year = max(year)) %>%
  ungroup() %>%
  mutate(total_years = last_year - first_year)

