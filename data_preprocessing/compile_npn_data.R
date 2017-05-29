library(tidyverse)
library(lubridate)

####################################################################
#From the NPN dataset extract the doy observations for an
#select species and specied phenophase (ie flowering vs leafout)
get_species_phenophase_observations = function(this_species, this_phenophase){
  obs_subset = all_observations %>%
    filter(species==this_species, Phenophase_ID == this_phenophase)
  
  if(nrow(obs_subset)==0){return(data.frame())}
  
  #site,year where a phenophase==0 was the first observation in a year
  phenophase_0 = obs_subset %>%
    group_by(Site_ID, year, Individual_ID) %>%
    top_n(1, -doy) %>%
    ungroup() %>%
    filter(Phenophase_Status==0) %>%
    select(Site_ID, year, Individual_ID) %>%
    mutate(keep='yes')
  
  #Keep only observations that were preceded by an observation of phenophase_status=0
  obs_subset = obs_subset %>%
    filter(Phenophase_Status==1) %>%
    group_by(Site_ID, year, Individual_ID) %>%
    top_n(1, -doy) %>%
    ungroup() %>%
    left_join(phenophase_0, by=c('Site_ID','year','Individual_ID')) %>%
    filter(keep=='yes') %>%
    select(-keep, -Phenophase_Status, -Observation_Date)
  
  obs_subset$Phenophase_ID=this_phenophase
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
  select(Site_ID, Individual_ID, Phenophase_ID, Observation_Date, Phenophase_Status, Genus, Species) %>%
  mutate(species= tolower(paste(Genus,Species,sep=' ')), 
         year   = lubridate::year(Observation_Date),
         doy    = lubridate::yday(Observation_Date))

#pull out data for each species, each with a specific phenological event (pine needles, decid leaves, flowers, etc)
processed_data = non_npn_species %>%
  rowwise() %>%
  do(get_species_phenophase_observations(this_species = .$species, this_phenophase = .$Phenophase_ID)) %>%
  ungroup() %>%
  select(Site_ID, species, year, doy)

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

