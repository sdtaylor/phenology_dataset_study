library(tidyverse)
library(lubridate)

data_dir = '~/data/phenology/'

################################################################
temperature_data = read_csv(paste0(data_dir,'site_PRISM_values.csv')) %>%
  mutate(date = as.Date(as.character(date), '%Y%m%d')) %>%
  mutate(year = year(date), doy = yday(date)) %>%
  rename(temp=value) %>%
  select(-var)

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

#NPN data only goes till 2016
temperature_data = temperature_data %>%
  filter(year<=2016)

#List of sites which have environmental data
#to filter observations with
sites_with_env_data=temperature_data %>%
  group_by(Site_ID, year) %>%
  summarise(num_na=sum(is.na(temp))) %>%
  filter(num_na==0) %>%
  select(Site_ID) %>%
  distinct()

write_csv(temperature_data, './cleaned_data/npn_temp.csv')

######################################################################################
site_info = read_csv(paste0(data_dir,'site_data.csv')) %>%
  select(Site_ID, Latitude, Longitude)

observations = read_csv(paste0(data_dir,'observations_spp_of_interest.csv')) %>%
  filter(Phenophase_ID == 371, Phenophase_Status>=0) %>%
  select(date=Observation_Date, Site_ID, species, Phenophase_Status) %>%
  mutate(year = year(date), doy=yday(date)) %>%
  filter(Site_ID %in% sites_with_env_data$Site_ID)

#site,year,species where a budbreak==0 was the first observation in a year
budbreak_0 = observations %>%
  group_by(species, Site_ID, year) %>%
  top_n(1, -doy) %>%
  filter(Phenophase_Status==0) %>%
  select(species, Site_ID, year) %>%
  mutate(keep='yes')

observations = observations %>%
  filter(Phenophase_Status==1) %>%
  left_join(budbreak_0, by=c('species','Site_ID','year')) %>%
  filter(keep=='yes') %>%
  select(-keep, -Phenophase_Status, -date)
  
#Unlikely that people are observing true bud break past august
#TODO: be more systematic about this.
observations = observations %>%
  filter(doy<240)

#Get mean doy for multiple observations of the same species
observations = observations %>%
  group_by(Site_ID, species, year) %>%
  summarise(doy=round(mean(doy)))

write_csv(observations, './cleaned_data/NPN_observations.csv')





