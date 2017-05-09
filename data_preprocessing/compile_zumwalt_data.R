library(tidyverse)
library(lubridate)

data_dir = '~/data/phenology/zumwalt/'

observations = read_csv(paste0(data_dir,'status_intensity_observation_data.csv')) 


obs_dates = observations %>% 
  group_by(date) %>% 
  summarise(num_species = n_distinct(tree.id)) %>% 
  mutate(year=lubridate::year(date)) %>%
  arrange(date)

obs_dates$days_since_last = NA
for(i in 2:nrow(obs_dates)){
  obs_dates$days_since_last[i] = obs_dates$date[i] - obs_dates$date[i-1]
}


%>%
  filter(Phenophase_ID == 371, Phenophase_Status>=0) %>%
  select(date=Observation_Date, Site_ID, species, Phenophase_Status, Individual_ID) %>%
  mutate(year = year(date), doy=yday(date)) %>%
  filter(Site_ID %in% sites_with_env_data$Site_ID)

