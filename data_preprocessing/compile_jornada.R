library(tidyverse)
library(lubridate)

data_dir = './raw_data/jornada'

observations = read_csv(file.path(data_dir,'JornadaStudy_287_npp_perennial_plant_phenology_0.csv'), skip = 45) %>%
  rename(Code = SPP, doy=JD, Site=SITE, PercentNON_Rep=`PercentNON-REP`) %>%
  mutate(date = as.Date(DATE, '%m/%d/%Y'), year=year(date))

plant_names = read_csv(file.path(data_dir,'Jornada_LTER_Plant_list.csv'))%>%
  select(species, Code)

#There are a few duplicate 4 letter species codes due to species synonyms. Rather than deal
#with it I will just exclude them as they are rare and not in the NPN dataset for comparison
species_code_duplicates = plant_names %>%
  group_by(Code) %>%
  tally() %>%
  filter(n>1)

observations = observations %>%
  filter(!Code %in% species_code_duplicates$Code)

#attach full species names. those without full names are unidentified to full species (ie. ARSP, UKGR)
observations = observations %>%
  left_join(plant_names, by='Code') %>%
  filter(!is.na(species))

#Metadata from the raw data file
# COMMENT  - Comment section [*see history log refers to file: JornadaStudy_287_npp_perennial_plant_phenology.his]
#PercentDORMANT = percent of all phenophase observations that are Dormant (= Dormant count divided by count sum of all phenophases)
#PercentNON-REP = percent of all phenophase observations that are Non-reproductive (= Dormant count divided by count sum of all phenophases)
#PercentBUD = percent of all phenophase observations that are Bud (= Bud count divided by count sum of all phenophases)
#PercentFLOWER = percent of all phenophase observations that are Flower (= Dormant count divided by count sum of all phenophases)
#PercentFRUIT = percent of all phenophase observations that are Fruit (= Dormant count divided by count sum of all phenophases)


process_phenology_data = function(this_species, this_phenophase){
  if(this_phenophase==371){
    obs_subset = observations %>%
      mutate(status = (PercentDORMANT<75)*1)
  } else if(this_phenophase==501){
    obs_subset = observations %>%
      mutate(status = (PercentFLOWER>25)*1) 
  }
  
  obs_subset = obs_subset %>%
    filter(species==this_species)
  
  #site,year,species where a budbreak==0 was the first observation in a year
  phenophase_0 = obs_subset %>%
    group_by(species, Site, year) %>%
    top_n(1, -doy) %>%
    ungroup() %>%
    filter(status==0) %>%
    select(species, Site, year, status_0_doy=doy) %>%
    mutate(keep='yes')
  
  #Keep only observations that were preceded by an observation of the phenophase==0
  obs_subset = obs_subset %>%
    filter(status==1) %>%
    group_by(species,Site,year) %>%
    top_n(1,-doy) %>%
    ungroup() %>%
    left_join(phenophase_0, by=c('species','Site','year')) %>%
    filter(keep=='yes') %>% 
    select(species,Site,year, doy)
  
  return(obs_subset)
}

#Chose these by hand. The only non-grass with sufficent observations
species = data.frame(species=c('prosopis glandulosa','larrea tridentata'),
                            Phenophase_ID = c(501,501))

processed_data = species %>%
  rowwise() %>%
  do(process_phenology_data(this_species = .$species, this_phenophase = .$Phenophase_ID))

#Makes sites numerica to work in the numpy models
site_info = read_csv('non_npn_site_info.csv') %>%
  filter(dataset=='jornada') %>%
  select(Site_ID, Site=note)

processed_data = processed_data %>%
  left_join(site_info, by='Site') %>%
  select(-Site)


write_csv(processed_data, './cleaned_data/jornada_observations.csv')

#Append to the same file written by other scripts
species$dataset='jornada'

non_npn_species_file = './cleaned_data/non_npn_species_list.csv'
if(file.exists(non_npn_species_file)){
  read_csv(non_npn_species_file) %>%
    bind_rows(species) %>%
    distinct() %>%
    write_csv(non_npn_species_file)
} else {
  write_csv(species, non_npn_species_file)
}
