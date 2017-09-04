library(tidyverse)
library(lubridate)
source('./data_preprocessing/processing_utils.R')

data_dir = './raw_data/harvard_forest/'


plant_info = read_csv(paste0(data_dir, 'hf003-01-plant.csv')) %>%
  select(tree.id, species=latin) %>%
  mutate(species=tolower(species))
observations = read_csv(paste0(data_dir, 'hf003-03-spring.csv')) %>%
  mutate(year=year(date)) %>%
  rename(doy=julian)


#Some individual tree.id codes don't have a species name in plant_info. 
#assign them one using the common 4 letter code.
observations$species_code = substr(observations$tree.id, 1,4)
plant_info$species_code = substr(plant_info$tree.id, 1, 4)
species_codes = plant_info %>%
  select(species_code, species) %>%
  distinct() 

observations = observations %>%
  left_join(species_codes, by='species_code') %>%
  select(species, individual_id = tree.id, date, doy, budbreak_percent = bbrk, flower_percent = fopn, year)

#All the other datasets have different sites, so add  a dummy one here
observations$Site_ID = 1

#Set the status based on thresholds in the % observed of flower or budbreak
observations = observations %>%
  mutate(vegetative = (budbreak_percent > 10)*1,
         reproductive = (flower_percent > 20)*1)

#Put in the format needed for processing code.
#Assign some species phenophase_id's for conifers
observations = observations %>%
  gather(obs_type, status, vegetative, reproductive) %>%
  mutate(Phenophase_ID = ifelse(obs_type=='vegetative', 371, 501)) %>%
  mutate(Phenophase_ID = ifelse(species == 'tsuga canadensis' & obs_type=='vegetative', 480, Phenophase_ID),
         Phenophase_ID = ifelse(species == 'pinus strobus' & obs_type=='vegetative', 496, Phenophase_ID)) %>%
  select(Site_ID, doy, year, species, status, individual_id, Phenophase_ID) %>%
  process_phenology_observations()

#Don't want trees not identified to species
observations = observations %>%
  filter(!species %in% c("amelanchier sp", "crataegus sp", "aronia sp", "rhododendron sp"))

write_csv(observations, './cleaned_data/harvard_observations.csv')

species_counts = observations %>%
  group_by(species, Phenophase_ID) %>%
  tally()

#Record the species and phenophase type to use in NPN data filter
species = observations %>% 
  select(species, Phenophase_ID) %>%
  distinct() %>%
  mutate(dataset='harvard')

append_species_file(species)
