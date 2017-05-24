library(tidyverse)
library(lubridate)

data_dir = '~/data/phenology/npn_core/'

site_info = read_csv(paste0(data_dir,'ancillary_site_data.csv')) %>%
  select(Site_ID, Latitude, Longitude)

non_npn_species = read_csv('./cleaned_data/non_npn_species_list.csv')

observations = read_csv(paste0(data_dir,'status_intensity_observation_data.csv')) %>%
  filter(Phenophase_ID == 371, Phenophase_Status>=0) %>%
  select(date=Observation_Date, Site_ID, Genus, Species, Phenophase_Status, Individual_ID) %>%
  mutate(year = year(date), doy=yday(date), species = tolower(paste(Genus, Species))) %>%
  filter(species %in% non_npn_species$species)
  filter(Site_ID %in% sites_with_env_data$Site_ID)

#site,year,species where a budbreak==0 was the first observation in a year
budbreak_0 = observations %>%
  group_by(species, Site_ID, year, Individual_ID) %>%
  top_n(1, -doy) %>%
  filter(Phenophase_Status==0) %>%
  select(species, Site_ID, year, Individual_ID) %>%
  mutate(keep='yes')

#Keep only budburst observations that were preceded by an observation of no budburst
observations = observations %>%
  filter(Phenophase_Status==1) %>%
  left_join(budbreak_0, by=c('species','Site_ID','year','Individual_ID')) %>%
  filter(keep=='yes') %>%
  select(-keep, -Phenophase_Status, -date)
  
#Unlikely that people are observing true bud break past august
#TODO: be more systematic about this.
observations = observations %>%
  filter(doy<240)

#The first budburst date for each individual. doy for multiple individuals
#are averaged for each site.
observations = observations %>%
  group_by(Site_ID, species, year, Individual_ID) %>%
  top_n(1, -doy) %>%
  ungroup() %>%
  group_by(Site_ID, species, year) %>%
  summarise(doy=round(mean(doy)))

#At the moment climate data starts in 2009
#TODO: get more climate data
observations = observations %>%
  filter(year>=2009)

#Minimum 60 observations for each species after all prior filtering
observations = observations %>%
  group_by(species) %>%
  filter(n() > 60) %>%
  ungroup()

write_csv(observations, './cleaned_data/NPN_observations.csv')

