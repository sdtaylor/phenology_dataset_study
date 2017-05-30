library(tidyverse)
source('./data_preprocessing/processing_utils.R')

rmbl = readxl::read_excel('./raw_data/rmbl/phenology data_1973_2012 from Amy Iler.xlsx', sheet='All DATA') %>%
  select(-HABITAT, plot=PLOT, species=SPECIES, year=YEAR, flower_count=FLOWER_COUNT, doy=DOY) %>%
  filter(year>=1974) %>%
  mutate(species = tolower(species))


#Counting flowers means that observations of 0 flowers go unrecorded. Here I infer those
#by using all obsevations from each plot in each year. The plots are 2mx2m so it seems safe to assume this. 
#The first observation at each site in each year
all_observations = rmbl %>%
  select(plot,year,doy) %>%
  distinct()

all_plot_species_year_observations = rmbl %>%
  select(plot,year,species) %>%
  distinct() %>%
  right_join(all_observations, by=c('plot','year'))

#Threshold for first flowering is 10% of the maximum count of flowers for each site,species,year
#Same as Iler et al. 2013
max_yearly_count = rmbl %>%
  group_by(plot,species,year) %>%
  filter(flower_count == max(flower_count)) %>%
  filter(doy==min(doy)) %>%
  ungroup() %>%
  mutate(first_flower_threshold = flower_count*0.1) %>%
  select(-flower_count, -doy)


#takes a vector of whether a count is past the first flowering threshold
#and return the first day which this is true
get_past_threshold_doy = function(doy, thresholds){
  position = which.max(thresholds)
  if(position < 1){
    return(NA)
  } else {
    return(doy[position])
  }
}
#takes a vector of whether a count is past the first flowering threshold
#and return the first day which this is true
get_prior_past_threshold_doy = function(doy, thresholds){
  position = which.max(thresholds)-1
  if(position < 1){
    return(NA)
  } else {
    return(doy[position])
  }
}


#First flowering date is the  midpoint between the 2 dats it passes the threshold
#again from Iler et al. 2013.
#This dataset is a organized differently than the others because it has flower counts,
#so I'm not using the common code in processing_utils.R
first_flowering = rmbl %>%
  right_join(all_plot_species_year_observations, by=c('species','plot','year','doy')) %>%
  mutate(flower_count = ifelse(is.na(flower_count), 0, flower_count)) %>%
  left_join(max_yearly_count, by=c('plot','species','year')) %>%
  mutate(past_threshold = (flower_count>=first_flower_threshold)*1) %>%
  group_by(species, plot, year) %>%
  filter(!any(is.na(first_flower_threshold))) %>%
  do(past_threshold_doy = get_past_threshold_doy(.$doy, .$past_threshold), 
     prior_threshold_doy = get_prior_past_threshold_doy(.$doy, .$past_threshold)) %>%
  ungroup() 

#dplyr::do() only returns a list for some reason, get rid of that.
first_flowering$past_threshold_doy = sapply(first_flowering$past_threshold_doy, unlist)
first_flowering$prior_threshold_doy = sapply(first_flowering$prior_threshold_doy, unlist)

first_flowering$first_flowing_doy = with(first_flowering, round(prior_threshold_doy + (past_threshold_doy - prior_threshold_doy)/2))

#Species from iler et al. 2013 which had sig (> 2 d_aic) better fit with temperature models than snowmelt models
#iler_species = c('arenaria congesta','delphinium barbeyi','dugaldia hoopesii','erythronium grandiflorum','gentiana parryi','geranium richardsonii',
#                 'helianthella quinquenervis','heliomeris multiflora','hydrophyllum fendleri','noccaea montana','oreochrysum parryi',
#                 'pedicularis bracteosa','potentilla gracilis v. pulcherrima','potentilla hippiana','pseudocymopterus montanus',
#                 'pyrrocoma crocea','sedum rosea','taraxacum officinale','valeriana capitata')

#I looked thru the npn species list manually and matched which ones match this dataset
npn_species_matches = c('fragaria virginiana','achillea millefolium','taraxacum officinale','rosa woodsii','heracleum maximum')

first_flowering = first_flowering %>%
  filter(!is.na(first_flowing_doy)) %>%
  select(species, year, doy=first_flowing_doy) %>%
  filter(species %in% npn_species_matches)

#limit to 1983 to present to match available PRISM data
first_flowering = first_flowering %>%
  filter(year >= 1983)

species_counts = first_flowering %>%
  group_by(species) %>%
  tally()

#Dummy site_id because the python models use it
first_flowering$Site_ID=1

write_csv(first_flowering, './cleaned_data/rmbl_observations.csv')

#Record the species present to use in NPN data filter
species = first_flowering %>% 
  select(species) %>%
  distinct() %>%
  mutate(dataset='rmbl', Phenophase_ID=501)

append_species_file(species)