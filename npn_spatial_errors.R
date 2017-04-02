library(tidyverse)
library(sp)

###################################################
#Param estimates for all NPN models
all_results = read_csv('./results/all_results.csv') %>%
  rename(T1=t1_int) %>%
  filter(dataset=='npn')

all_results = all_results %>%
  gather(Parameter, value, -boostrap_num, -dataset, -species) 

parameter_means = all_results %>%
  group_by(species, Parameter, dataset) %>%
  summarise(param_mean = mean(value)) %>%
  ungroup() %>%
  spread(Parameter, param_mean)

#########################################################
#NPN Data
observations = read_csv('./cleaned_data/NPN_observations.csv')
temperature  = read_csv('./cleaned_data/npn_temp.csv')

#########################################################
#doy estimate given model parameters, site, and year
calculate_doy_estimate = function(t1,b,c,F_,site_id,this_year){
  temp_data = temperature %>%
    filter(Site_ID==site_id, year==this_year)
  
  temp_data$temp =   1 / (1 + exp(b*(temp_data$temp-c)))
  temp_data$temp[temp_data$doy<t1]=0
  
  temp_data$forcing=0
  for(d in 1:nrow(temp_data)){
    temp_data$forcing[d] = sum(temp_data$temp[1:d])
  }
  
  temp_data$doy[match(TRUE,temp_data$forcing>=F_)]
}

#########################################################
#Make doy estimates based on modeled parameters

observations$doy_estimate=NA
for(i in 1:nrow(observations)){
  this_obs = observations[i,]
  params = parameter_means %>%
    filter(species==this_obs$species)
  
  observations$doy_estimate[i] = calculate_doy_estimate(t1=params$T1, b=params$b, c=params$c, F_=params$F,site_id=this_obs$Site_ID, this_year=this_obs$year)
  print(i)
  
}

observations$error = with(observations, doy-doy_estimate)
#########################################################
#lat long
site_info = read_csv('~/data/phenology/npn/observations_top_20.csv') %>%
  dplyr::select(Site_ID, Latitude, Longitude) %>%
  dplyr::distinct()

observations2 = observations %>%
  left_join(site_info, by='Site_ID')

########################################################
#http://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
#Examples use the inverse of site_distances, but I think that's because
#library(ape) is made for phylogenetics?
#TODO: look into that
calculate_morans = function(df){
  site_distances = as.matrix(dist(cbind(df$Longitude, df$Latitude)))
  #site_distances = 1/site_distances
  diag(site_distances) = 0
  ape::Moran.I(df$error, site_distances)$p.value
}

moran_p_values = observations2 %>%
  group_by(species, year) %>%
  do(p_value = calculate_morans(.)) %>%
  ungroup()





