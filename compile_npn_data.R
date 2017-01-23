library(tidyverse)
library(lubridate)

data_dir = '~/data/phenology/'

site_info = read_csv(paste0(data_dir,'site_data.csv')) %>%
  select(Site_ID, Latitude, Longitude)

observations = read_csv(paste0(data_dir,'observations_spp_of_interest.csv')) %>%
  filter(Phenophase_ID == 371, Phenophase_Status==1) %>%
  select(date=Observation_Date, Site_ID, species) %>%
  mutate(year = year(date), doy=yday(date))

temperature_data = read_csv(paste0(data_dir,'site_PRISM_values.csv')) %>%
  mutate(date = as.Date(as.character(date), '%Y%m%d')) %>%
  mutate(year = year(date), doy = yday(date)) %>%
  rename(temp=value) %>%
  select(-var)

##############################################################################
#Compile degree days and chill days for each year

compile_deg_days = function(df){
  if(length(unique(df$year))>1) stop('>1 year')
  df = df %>%
    arrange(doy)
  
  df$is_chill_day = ifelse(df$temp<=0, 1, 0)
  df$deg_day      = ifelse(df$temp>0, df$temp, 0)
  df$NCD = 0
  df$GDD = 0
  
  for(i in 1:nrow(df)){
    df$NCD[i] = sum(df$is_chill_day[1:i])
    df$GDD[i] = sum(df$deg_day[1:i])
    
  }
  
  df = df %>% 
    select(-is_chill_day, -deg_day)
  
}

temp_data_with_gdd_file = paste0(data_dir,'site_PRISM_values_gdd_ncd.csv')
if(file.exists(temp_data_with_gdd_file)){
  temperature_data = read_csv(temp_data_with_gdd_file)
} else {
  temperature_data = temperature_data %>% 
    group_by(Site_ID, year) %>%
    do(compile_deg_days(.))
  write_csv(temperature_data, temp_data_with_gdd_file)
}

######################################################################################
#mean_spring_temperature = temperature_data %>%
#  filter(doy<60) %>%
#  group_by(Site_ID, year) %>%
#  summarize(mean_spring_temp = mean(value))

observations = observations %>%
  left_join(select(temperature_data, Site_ID, year, doy, NCD, GDD), by=c('Site_ID','year','doy'))
rm(temperature_data, temp_data_with_gdd_file)

#Unlikely that people are observing true bud break past august, or in the first few days of Jan
#when no GDD has accumulated.
#TODO: be more systematic about this.
observations = observations %>%
  filter(doy<240)

#Some sites are missing GDD and NCD data, likely because they're near a large water body
#and don't have a prism cell. 
observations = observations[complete.cases(observations),]


write_csv(observations, './cleaned_data/NPN_observations.csv')





