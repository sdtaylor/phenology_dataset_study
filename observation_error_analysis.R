library(tidyverse)
library(sp)

###################################################
#Param estimates for all NPN models
all_results = read_csv('./results/all_results_bootstrapped.csv') %>%
  rename(T1=t1_int) 

all_results = all_results %>%
  gather(Parameter, value, -boostrap_num, -dataset, -species) 

parameter_means = all_results %>%
  group_by(species, Parameter, dataset) %>%
  summarise(param_mean = mean(value)) %>%
  ungroup() %>%
  spread(Parameter, param_mean)

#########################################################
#NPN Data
npn_observations = read_csv('./cleaned_data/NPN_observations.csv')
npn_temperature  = read_csv('./cleaned_data/npn_temp.csv')
npn_observations$dataset='NPN'

#harvard data
harvard_observations = read_csv('./cleaned_data/harvard_observations.csv')
harvard_temperature  = read_csv('./cleaned_data/harvard_temp.csv')
harvard_temperature$Site_ID=1
harvard_observations$Site_ID=1
harvard_observations$dataset='Harvard'

#Keep only species that are present in NPN dataset
npn_species = unique(npn_observations$species)

harvard_observations = harvard_observations %>% 
  filter(species %in% npn_species)
#########################################################
#doy estimate given model parameters, site, and year
calculate_doy_estimate = function(t1,b,c,F_,site_id,this_year, temp_df){
  temp_data = temp_df %>%
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
#Make doy estimates & errors based on modeled parameters
npn_observations$doy_estimate=NA
for(i in 1:nrow(npn_observations)){
  this_obs = npn_observations[i,]
  params = parameter_means %>%
    filter(species==this_obs$species, dataset == 'npn')
  
  npn_observations$doy_estimate[i] = calculate_doy_estimate(t1=params$T1, b=params$b, c=params$c, F_=params$F,
                                                            site_id=this_obs$Site_ID, this_year=this_obs$year,
                                                            temp_df = npn_temperature)
  print(i)
  
}
npn_observations$error = with(npn_observations, doy-doy_estimate)
#####
harvard_observations$doy_estimate=NA
for(i in 1:nrow(harvard_observations)){
  this_obs = harvard_observations[i,]
  params = parameter_means %>%
    filter(species==this_obs$species, dataset == 'harvard')
  
  harvard_observations$doy_estimate[i] = calculate_doy_estimate(t1=params$T1, b=params$b, c=params$c, F_=params$F,
                                                            site_id=this_obs$Site_ID, this_year=this_obs$year,
                                                            temp_df = harvard_temperature)
  print(i)
  
}
harvard_observations$error = with(harvard_observations, doy-doy_estimate)
#########################################################

all_errors = npn_observations %>%
  bind_rows(harvard_observations)

error_stats = all_errors %>%
  group_by(species, dataset) %>%
  summarise(error_mean = mean(error), error_sd=sd(error)) %>%
  ungroup()

new_names=c('Q. rubra', 'Q. alba', 'F. grandifolia', 'P. tremuloides', 'A. rubrum', 'B. papyrifera', 'A. saccharum', 'P. serotina')
old_names = c('quercus rubra','quercus alba','fagus grandifolia','populus tremuloides','acer rubrum','betula papyrifera','acer saccharum','prunus serotina')
all_errors$species = factor(all_errors$species, levels=old_names, labels=new_names)

error_stats = all_errors %>%
  group_by(species, dataset) %>%
  summarise(error_mean = mean(error), error_sd=sd(error)) %>%
  ungroup() %>%
  mutate(text1 = paste('mu: ', round(error_mean,2)),
         text2 = paste('sigma^2: ', round(error_sd,2))) %>%
  left_join(data.frame(species=new_names,
                       x_placement=c(40, 30, 50, 40, 50, 70, 70, 70)), by='species')


figure_1st_row = c('Q. rubra', 'Q. alba', 'F. grandifolia', 'P. tremuloides')
all_errors$figure_row = ifelse(all_errors$species %in% figure_1st_row, 'first','second')
error_stats$figure_row = ifelse(error_stats$species %in% figure_1st_row, 'first','second')

first_row = ggplot(filter(all_errors, figure_row=='first'), aes(error, group=dataset, fill=dataset)) +
  geom_histogram(bins=30) +
  geom_vline(xintercept = 0) +  
  ylim(0,100) +
  geom_text(data = filter(error_stats, figure_row=='first'), aes(x=x_placement, y=85, label=text1),size=5, parse=TRUE)+
  geom_text(data = filter(error_stats, figure_row=='first'), aes(x=x_placement, y=65, label=text2),size=5, parse=TRUE)+
  scale_color_manual(values=c('#E69F00','#0072B2')) +
  scale_fill_manual(values=c('#E69F00','#0072B2')) +
  facet_grid(dataset~species, scales = 'free_x')+
  theme_bw()+
  labs(x = '', y = NULL) + 
  theme(legend.position = "none",
        strip.text.x=element_text(size=22),
        strip.text.y=element_text(size=22),
        axis.text = element_text(size = 15))

second_row = ggplot(filter(all_errors, figure_row=='second'), aes(error, group=dataset, fill=dataset)) +
  geom_histogram(bins=30) +
  geom_vline(xintercept = 0) +  
  ylim(0,100) +
  geom_text(data = filter(error_stats, figure_row=='second'), aes(x=x_placement, y=85, label=text1),size=5, parse=TRUE)+
  geom_text(data = filter(error_stats, figure_row=='second'), aes(x=x_placement, y=65, label=text2),size=5, parse=TRUE)+
  scale_color_manual(values=c('#E69F00','#0072B2')) +
  scale_fill_manual(values=c('#E69F00','#0072B2')) +
  facet_grid(dataset~species, scales = 'free_x')+
  theme_bw()+
  labs(x = 'Budburst Day of Year Error', y = NULL) + 
  theme(legend.position = "none",
        strip.text.x=element_text(size=22),
        strip.text.y=element_text(size=22),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 25))

gridExtra::grid.arrange(first_row, second_row)


#########################################################
#Coordinates of npn sites to get the morans statistic
site_info = read_csv('~/data/phenology/npn/observations_top_20.csv') %>%
  dplyr::select(Site_ID, Latitude, Longitude) %>%
  dplyr::distinct()

npn_observations2 = npn_observations %>%
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
  moran_stats = as.data.frame(ape::Moran.I(df$error, site_distances))
  moran_stats$n = nrow(df)
  return(as.data.frame(moran_stats))
  
}

moran_I_values = npn_observations2 %>%
  group_by(species, year) %>%
  do(calculate_morans(.)) %>%
  ungroup()

moran_I_values$observed = round(moran_I_values$observed, 2)

moran_table_obs = moran_I_values %>%
  mutate(observed_table = ifelse(p.value<0.05, paste0(observed,'*'), as.character(observed))) %>%
  select(Species=species, year, observed_table) %>%
  spread(year, observed_table)

moran_table_obs$Species = factor(moran_table_obs$Species, levels=old_names, labels=new_names)

table_theme = gridExtra::ttheme_default(base_size = 24)
gridExtra::grid.table(moran_table_obs, rows=NULL, theme = table_theme)



