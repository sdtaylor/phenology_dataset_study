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
  rename(individual_id = tree.id)

#All the other datasets have different sites, so add  a dummy one here
observations$Site_ID = 1

#Set the doy for budbreak as the first day >10% of leafs bursting is observed
observations = observations %>%
  mutate(status= (bbrk > 10)*1) %>%
  select(Site_ID, doy, year, species, status, individual_id) %>%
  process_phenology_observations()

#Don't want trees not identified to species
observations = observations %>%
  filter(!species %in% c("amelanchier sp", "crataegus sp", "aronia sp", "rhododendron sp"))

write_csv(observations, './cleaned_data/harvard_observations.csv')

species_counts = observations %>%
  group_by(species) %>%
  tally()

#Record the species and phenophase type to use in NPN data filter
species = observations %>% 
  select(species) %>%
  distinct() %>%
  mutate(dataset='harvard', Phenophase_ID=371)

#tusga (evergreen conifer) and pinus (pine) have special budbreak phenophase_ids
species$Phenophase_ID[species$species=='tsuga canadensis'] = 480
species$Phenophase_ID[species$species=='pinus strobus'] = 496

append_species_file(species)
