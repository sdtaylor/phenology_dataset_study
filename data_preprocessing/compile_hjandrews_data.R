library(tidyverse)
library(lubridate)
source('./data_preprocessing/processing_utils.R')

species_names = read_csv('raw_data/hjandrews/species_names.csv')
species_status_info = read_csv('raw_data/hjandrews/species_status_info.csv')


observations = read_csv('raw_data/hjandrews/TV07501_v1_without_encoding.csv') %>%
  rename(species_code=SPECIES) %>%
  left_join(species_names, by='species_code') %>%
  select(year = YEAR, plot = PLOT, individual_id = TAG, date = SAMPLEDATE, species, vegatative = VEG_CODE, reproductive = RPRO_CODE) %>%
  gather(obs_type, obs_code, vegatative, reproductive)

#Makes sites numerica to work in the python models
site_info = read_csv('non_npn_site_info.csv') %>%
  filter(dataset=='hjandrews') %>%
  select(Site_ID, plot=note)

observations = observations %>%
  left_join(site_info, by='plot') %>%
  select(-plot)


#Convert observations of leavs/flowers to status codes. 
observations = observations %>%
  left_join(species_status_info, by=c('species','obs_type')) %>%
  mutate(status = 1*(obs_code == target_obs_code)) %>%
  select(-obs_type, -obs_code, -target_obs_code)

#Add in doy and process
observations = observations %>%
  mutate(doy = yday(date)) %>%
  select(-date) %>%
  process_phenology_observations() %>%
  group_sites_together() %>%
  apply_minimum_observation_threshold(min_num_obs = 30)

write_csv(observations, './cleaned_data/hjandrews_observations.csv')

species_counts = observations1 %>%
  group_by(species, Phenophase_ID) %>%
  tally()

#Record the species and phenophase type to use in NPN data filter
species = observations %>% 
  select(species, Phenophase_ID) %>%
  distinct() %>%
  mutate(dataset='hjandrews')

append_species_file(species)
