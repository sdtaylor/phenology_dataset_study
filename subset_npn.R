#Subset the NPN data to species I'm interested in 
library(tidyverse)

species_of_interest = c('prunus serotina','viburnum cassinoides','acer saccharum','betula alleghaniensis','betula lenta','betula papyrifera',
                        'sambucus pubens','vaccinium corymbosum','viburnum alnifolium','acer pensylvanicum','acer rubrum','populus tremuloides',
                        'betula populifolia','fraxinus americana','hamamelis virginia','nemopanthus mucronata','castanea dentata','fagus grandifolia',
                        'ilex verticillata','lyonia ligustina','quercus alba','quercus rubra','nyssa sylvatica','quercus velutina','kalmia latifolia',
                        'kalmia angustifolia','tsuga canadensis','pinus strobus','cornus alterniflora','cornus alternifolia')


all_observations = read_csv('~/data/phenology/observation_data.csv')

all_observations = all_observations %>%
  mutate(full_species_name=tolower(paste(Genus,Species,sep=' ')))

all_observations = all_observations %>%
  filter(full_species_name %in% species_of_interest)

all_observations = all_observations %>%
  select(species=full_species_name, Observation_Date, Phenophase_ID, Phenophase_Description, Phenophase_Status, Site_ID, Individual_ID)

write_csv(all_observations, '~/data/phenology/observations_spp_of_interest.csv')
