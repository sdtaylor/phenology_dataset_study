library(tidyverse)
library(lubridate)
source('./data_preprocessing/processing_utils.R')

####################################################################
#From the NPN dataset extract the doy observations for an
#select species and specied phenophase (ie flowering vs leafout)
subset_npn_data = function(this_species, this_phenophase){
  obs_subset = all_observations %>%
    filter(species==this_species, Phenophase_ID == this_phenophase)
  
  if(nrow(obs_subset)==0){return(data.frame())}

  return(obs_subset)
}

##################################################################################
data_dir = '~/data/phenology/npn_core/'

#Some species are in > dataset, so only get distinct species/phenophases
non_npn_species = read_csv('./cleaned_data/non_npn_species_list.csv') %>%
  select(-dataset) %>%
  distinct()

#The raw npn data
all_observations = read_csv(paste0(data_dir,'status_intensity_observation_data.csv')) %>%
  select(Site_ID, individual_id = Individual_ID, Phenophase_ID, Observation_Date, status = Phenophase_Status, Genus, Species) %>%
  mutate(species= tolower(paste(Genus,Species,sep=' ')), 
         year   = lubridate::year(Observation_Date),
         doy    = lubridate::yday(Observation_Date))

#pull out data for each species, each with a specific phenological event (pine needles, decid leaves, flowers, etc)
processed_data = non_npn_species %>%
  rowwise() %>%
  do(subset_npn_data(this_species = .$species, this_phenophase = .$Phenophase_ID)) %>%
  ungroup() %>%
  filter(status>=0) %>%
  select(Site_ID, species, individual_id, year, doy, status) %>%
  process_phenology_observations()

#At the moment climate data starts in 2009
#TODO: get more climate data
processed_data = processed_data %>%
  filter(year>=2009)

observations_per_species = processed_data %>%
  group_by(species) %>%
  tally()

#Minimum 40 observations for each species after all prior filtering
processed_data = processed_data %>%
  group_by(species) %>%
  filter(n() > 40) %>%
  ungroup()

write_csv(processed_data, './cleaned_data/npn_observations.csv') 

