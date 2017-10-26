library(tidyverse)
library(lubridate)
source('./data_preprocessing/processing_utils.R')

####################################################################
#From the NPN dataset extract the doy observations for an
#select species and specied phenophase (ie flowering vs leafout)
#distinct() to compact down multiple observations on the same day
subset_npn_data = function(this_species, this_phenophase){
  obs_subset = all_observations %>%
    filter(species==this_species, Phenophase_ID == this_phenophase) %>%
    distinct()
  
  if(nrow(obs_subset)==0){return(data.frame())}

  return(obs_subset)
}

##################################################################################
data_dir = '~/data/phenology/npn_core/'


#Add some extra species that aren't in any long term studies, but to 
#compare results to Crimmens et al. 2017
extra_spp = doy_cutoffs = read.table(header = TRUE, sep=',', text = '
species,Phenophase_ID
quercus alba,501
amelanchier canadensis,371
amelanchier canadensis,501
cornus florida,371
betula papyrifera,501')

#Some species are in > dataset, so only get distinct species/phenophases
non_npn_species = read_csv('./cleaned_data/non_npn_species_list.csv') %>%
  select(-dataset) %>%
  bind_rows(extra_spp) %>%
  distinct()


#The raw npn data
all_observations = read_csv(paste0(data_dir,'status_intensity_observation_data.csv')) %>%
  select(Site_ID, individual_id = Individual_ID, Phenophase_ID, Observation_Date, status = Phenophase_Status,
         intensity_id = Intensity_Category_ID, intensity = Intensity_Value, Genus, Species) %>%
  mutate(species= tolower(paste(Genus,Species,sep=' ')), 
         year   = lubridate::year(Observation_Date),
         doy    = lubridate::yday(Observation_Date))

#pull out data for each species, each with a specific phenological event (pine needles, decid leaves, flowers, etc)
processed_data = non_npn_species %>%
  rowwise() %>%
  do(subset_npn_data(this_species = .$species, this_phenophase = .$Phenophase_ID)) %>%
  ungroup() %>%
  filter(status>=0) %>%
  select(Site_ID, species, individual_id, year, doy, status, Phenophase_ID) %>%
  process_phenology_observations(prior_obs_cutoff = 14)

#Core NPN collection started in 2009.
#2017 does not have climate data available yet
processed_data = processed_data %>%
  filter(year>=2009, year<2017)

#Don't include observations past Aug 1 for flowers, or June 21 for buds
doy_cutoffs = read.table(header = TRUE, sep=',', text = '
Phenophase_ID,doy_cutoff
371,172
501,213
480,172
488,172
496,172
')

processed_data = processed_data %>%
  left_join(doy_cutoffs, by='Phenophase_ID') %>%
  filter(doy <= doy_cutoff)

#More than 1 individual observed at a site in year? Take  the average value
processed_data = processed_data %>%
  group_by(species, Site_ID, year, Phenophase_ID) %>%
  summarise(doy = round(mean(doy),0)) %>%
  ungroup()

observations_per_species = processed_data %>%
  group_by(species, Phenophase_ID) %>%
  tally()


#Minimum 30 observations for each species after all prior filtering
processed_data = processed_data %>%
  group_by(species, Phenophase_ID) %>%
  filter(n() > 30) %>%
  ungroup()

write_csv(processed_data, './cleaned_data/npn_observations.csv') 

