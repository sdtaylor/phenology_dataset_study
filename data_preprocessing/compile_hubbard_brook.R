library(tidyverse)
library(lubridate)
source('./data_preprocessing/processing_utils.R')

data_dir = './raw_data/hubbard_brook/'

#Tidy data to one site observation per line
observations = read_csv(paste0(data_dir, 'phn.txt')) %>%
  gather(site, status, -Date, -DAY, -SEASON, -SPECIES) %>%
  rename(doy=DAY, season=SEASON, common_name=SPECIES) 

#Use scientific names
species_names = read.table(header=TRUE, sep=',', stringsAsFactors = FALSE, text=
'common_name, species
American_Beech,fagus grandifolia
Sugar_Maple,acer saccharum
Yellow_Birch,betula alleghaniensis')

observations = observations %>%
  left_join(species_names, by='common_name') %>%
  select(-common_name)

#Switch site identifer to a number so it works in my python numpy code models
sites = unique(observations$site)
new_site_ids = data.frame(site = sites,
                          Site_ID = 1:length(sites))

observations = observations %>%
  left_join(new_site_ids, by='site')

#Get year of observation
observations = observations %>%
  mutate(Date = as.Date(Date), year=year(Date)) 

#Exclude fall data, exclude missing values
observations = observations %>%
  filter(season=='SPRING', status!=(-9))

#status for spring are:
#0: winter conditions
#1: bud swelling
#2: small leaves or flowers
#3: leafs 1/2 of final length, leafs obscure half the sky as seen thru crown
#3.5: leaves 3/4 expanded, sky mostly obscured, crown not yet in summer condition
#4: fully expanded, canopy in summer conditions
#At each site the  above observations are made for 3 trees, which are then averaged together.
#I define budbreak as status >=1.6. This is where the most likely values for the 3 trees are c(1,2,2)
#TODO: make budbreak the midpoint of first obs >=1.6 and the prior obs.
observations = observations %>%
  mutate(status = (status >= 1.6)*1) %>%
  select(species, year, Site_ID, doy, status) %>%
  mutate(individual_id=1) %>% #Dummy variable for individual_id
  process_phenology_observations()

#Check to ensure there are no duplicates
there_are_duplicates = nrow(observations) != nrow(distinct(select(observations, Site_ID, species, year)))
if(there_are_duplicates) stop('duplicates')


write_csv(observations, './cleaned_data/hubbardbrook_observations.csv')

#Record the species present to use in NPN data filter
species = observations %>% 
  select(species) %>%
  distinct() %>%
  mutate(dataset='hubbard', Phenophase_ID=371)

append_species_file(species)
