library(tidyverse)
library(lubridate)


data_dir = '~/data/phenology/harvard_forest/'


plant_info = read_csv(paste0(data_dir, 'hf003-01-plant.csv')) %>%
  select(tree.id, species=latin) %>%
  mutate(species=tolower(species))
observations = read_csv(paste0(data_dir, 'hf003-03-spring.csv')) %>%
  mutate(year=year(date)) %>%
  rename(doy=julian) 

#For each tree and year, get the  first day that
#Bud Break is > 50%
observations = observations %>%
  filter(bcon=='BB', bbrk > 50) %>%
  select(doy, tree.id, year) %>%
  group_by(tree.id, year) %>%
  top_n(1, -doy) %>%
  ungroup()


#Some individual tree.id codes don't have a species name in plant_info. 
#assign them one using the common 4 letter code.
observations$tree.id = substr(observations$tree.id, 1,4)
plant_info$tree.id = substr(plant_info$tree.id, 1, 4)
species_codes = plant_info %>%
  select(tree.id, species) %>%
  distinct() %>%
  filter(!is.na(species))

observations = observations %>%
  left_join(species_codes, by='tree.id') %>%
  select(-tree.id)

#Don't want trees not identified to species
observations = observations %>%
  filter(!species %in% c("amelanchier sp", "crataegus sp", "aronia sp", "rhododendron sp"))

write_csv(observations, './cleaned_data/harvard_observations.csv')

#########################################################
#Downloaded from the PRISM website. tmean for 1990-01-01 - 2016-12-32 for Latitude: 42.5429   Longitude: -72.2011
temperature_data = read_csv(paste0(data_dir, 'PRISM_Harvard_forest_tmean.csv'), skip = 10)
colnames(temperature_data) = c('date','temp')
temperature_data = temperature_data %>%
  mutate(year=year(date), doy=yday(date)) 

#Limit temp data to fall and mid summer
temperature_data = temperature_data %>%
  filter(doy <=180 | doy >= 240)

#Assigne fall temp to the next years growing season
#Set jan 1 as doy 0, anything before that as negative doy's
temperature_data = temperature_data %>%
  mutate(year = ifelse(doy>=240, year+1, year)) %>%
  mutate(base_date = as_date(paste0(year,'-01-01'))) %>%
  mutate(doy = date - base_date) %>%
  select(-date, -base_date)

write_csv(temperature_data, './cleaned_data/harvard_temp.csv')



