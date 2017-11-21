library(tidyverse)

# This currently just makes, for each dataset, a file lats and longs, but in the future could do other things 
# that are dataset site specific. 

dataset_configs = yaml::yaml.load_file('config.yaml')$dataset_configs

# For non-npn datasets I compiled this short file manually
site_info = read.csv('non_npn_site_info.csv')

# add on NPN sites
site_info = read_csv('~/data/phenology/npn_core/ancillary_site_data.csv') %>%
  dplyr::select(Site_ID, Latitude, Longitude) %>%
  dplyr::distinct() %>%
  mutate(dataset='npn', start_year=2009, end_year=2016) %>%
  bind_rows(site_info)

site_info = site_info %>%
  rename(lat = Latitude, lon = Longitude)

for(dataset_i in 1:length(dataset_configs)){
  dataset_site_info_file = dataset_configs[[dataset_i]]$site_info_file
  dataset_name    = dataset_configs[[dataset_i]]$dataset_name
  
  site_info %>%
    filter(dataset == dataset_name) %>%
    write_csv(dataset_site_info_file)
}
