library(tidyverse)
library(lubridate)


data_dir = '~/data/harvard_forest/'
plant_info = read_csv(paste0(data_dir, 'hf003-01-plant.csv')) %>%
  select(tree.id, species=latin) %>%
  mutate(species=tolower(species))
observations = read_csv(paste0(data_dir, 'hf003-03-spring.csv')) %>%
  mutate(year=year(date)) %>%
  rename(doy=julian) 
#temperature_data_local = read_csv(paste0(data_dir, 'hf004-02-filled.csv')) %>%
#  mutate(date = round_date(datetime, 'day')) %>%
#  group_by(date) %>%
#  summarize(temp = mean(ta.2.5m.filled, na.rm=T)) %>%
#  mutate(year=year(date), doy=yday(date))

#Downloaded from the PRISM website. tmean for 1990-01-01 - 2016-12-32 for Latitude: 42.5429   Longitude: -72.2011
temperature_data = read_csv(paste0(data_dir, 'PRISM_Harvard_forest_tmean.csv'), skip = 10)
colnames(temperature_data) = c('date','temp')
temperature_data = temperature_data %>%
  mutate(year=year(date), doy=yday(date))

#For each tree and year, get the  first day that
#Bud Break is > 50%
observations = observations %>%
  filter(bcon=='BB', bbrk > 50) %>%
  select(date, doy, tree.id, year) %>%
  group_by(tree.id, year) %>%
  top_n(1, -doy) %>%
  ungroup()



#Some individual tree.id codes don't have a species name in plant_info. 
#assign them one using the common 4 letter code.
observations$tree.id = substr(observations$tree.id, 1,4)
plant_info$tree.id = substr(plant_info$tree.id, 1, 4)
species_codes = plant_info %>%
  select(tree.id, species) %>%
  distinct() %>%
  filter(!is.na(species))

observations = observations %>%
  left_join(species_codes, by='tree.id') %>%
  select(-tree.id)

######################################################################################
#Calculate growing and chilling degree days


get_degree_days = function(year_to_get, doy_to_get, deg_day_threshold=0){
  temp = temperature_data %>%
    filter(year==year_to_get, doy<=doy_to_get) %>%
    filter(temp>deg_day_threshold)
  #tots=sum(temp$temp, na.rm=T)
  #if(is.na(tots)) print(paste0(year_to_get,'-',doy_to_get,'-',tots))
  return(sum(temp$temp, na.rm=T))
}

get_chill_days = function(year_to_get, doy_to_get, chill_threshold=0){
  temp = temperature_data %>%
    filter(year==year_to_get, doy<=doy_to_get) %>%
    mutate(is_chill_day = ifelse(temp<chill_threshold, 1,0))
  #tots=sum(temp$is_chill_day, na.rm=T)
  #if(is.na(tots)) print(paste0(year_to_get,'-',doy_to_get,'-',tots))
  return(as.numeric(sum(temp$is_chill_day, na.rm=T)))
}


observations = observations %>%
  rowwise() %>%
  mutate(GDD=get_degree_days(year_to_get = year, doy_to_get = doy), NCD=get_chill_days(year_to_get = year, doy_to_get = doy)) %>%
  ungroup()

write_csv(observations, './cleaned_data/harvard_observations.csv')
