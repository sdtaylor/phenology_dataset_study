library(tidyverse)
#Compare out of sample verification between datasets


oos_data = read_csv('./results/out_of_sample_doy_estimates.csv')
#Keep only species that are present in NPN dataset
npn_species = oos_data %>%
  filter(observation_source == 'npn') %>%
  select(species) %>%
  distinct()

oos_data = oos_data %>% 
  filter(species %in% npn_species$species)


oos_estimates = oos_data %>%
  group_by(model_name, observation_source, parameter_source, site_observed, species, year_observed) %>%
  summarise(n=n(), doy_estimated = mean(doy_estimated), doy_observed = mean(doy_observed)) %>%
  ungroup() %>%
  mutate(error = doy_observed - doy_estimated) %>%
  filter(parameter_source=='npn')


####################################################

new_names=c('Q. rubra', 'Q. alba', 'F. grandifolia', 'P. tremuloides', 'A. rubrum', 'B. papyrifera', 'A. saccharum', 'P. serotina')
old_names = c('quercus rubra','quercus alba','fagus grandifolia','populus tremuloides','acer rubrum','betula papyrifera','acer saccharum','prunus serotina')
oos_estimates$species = factor(oos_estimates$species, levels=old_names, labels=new_names)

#The in graph mu and standard devitation. See ?plotmath for details of special symbols
error_stats = oos_estimates %>%
  group_by(species, observation_source, parameter_source) %>%
  summarise(error_mean = mean(error), error_sd=sd(error)) %>%
  ungroup() %>%
  mutate(text1 = paste('mu: ', round(error_mean,2)),
         text2 = paste('sigma: ', round(error_sd,2))) %>%
  left_join(data.frame(species=new_names,
                       x_placement=c(40, 30, 50, 40, 50, 70, 70, 70)), by='species')


figure_1st_row = c('Q. rubra', 'Q. alba', 'F. grandifolia', 'P. tremuloides')
oos_estimates$figure_row = ifelse(oos_estimates$species %in% figure_1st_row, 'first','second')
error_stats$figure_row = ifelse(error_stats$species %in% figure_1st_row, 'first','second')

first_row = ggplot(filter(oos_estimates, figure_row=='first'), aes(error, group=observation_source, fill=observation_source)) +
  geom_histogram(bins=30) +
  geom_vline(xintercept = 0) +  
  ylim(0,100) +
  geom_text(data = filter(error_stats, figure_row=='first'), aes(x=x_placement, y=85, label=text1),size=5, parse=TRUE)+
  geom_text(data = filter(error_stats, figure_row=='first'), aes(x=x_placement, y=65, label=text2),size=5, parse=TRUE)+
  scale_color_manual(values=c('#E69F00','#0072B2')) +
  scale_fill_manual(values=c('#E69F00','#0072B2')) +
  facet_grid(observation_source~species, scales = 'free_x')+
  theme_bw()+
  labs(x = '', y = NULL) + 
  theme(legend.position = "none",
        strip.text.x=element_text(size=22),
        strip.text.y=element_text(size=22),
        axis.text = element_text(size = 15))

second_row = ggplot(filter(oos_estimates, figure_row=='second'), aes(error, group=observation_source, fill=observation_source)) +
  geom_histogram(bins=30) +
  geom_vline(xintercept = 0) +  
  ylim(0,100) +
  geom_text(data = filter(error_stats, figure_row=='second'), aes(x=x_placement, y=85, label=text1),size=5, parse=TRUE)+
  geom_text(data = filter(error_stats, figure_row=='second'), aes(x=x_placement, y=65, label=text2),size=5, parse=TRUE)+
  scale_color_manual(values=c('#E69F00','#0072B2')) +
  scale_fill_manual(values=c('#E69F00','#0072B2')) +
  facet_grid(observation_source~species, scales = 'free_x')+
  theme_bw()+
  labs(x = 'Budburst Day of Year Error (observation - prediction)', y = NULL) + 
  theme(legend.position = "none",
        strip.text.x=element_text(size=22),
        strip.text.y=element_text(size=22),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 25))

gridExtra::grid.arrange(first_row, second_row)


###########################################################################3
#Analysis of spatial autocorrelation in NPN errors
#########################################################
#Coordinates of npn sites to get the morans statistic
site_info = read_csv('~/data/phenology/npn/observations_top_20.csv') %>%
  dplyr::select(Site_ID, Latitude, Longitude) %>%
  dplyr::distinct()

npn_observations = oos_estimates %>%
  filter(observation_source=='npn', parameter_source=='npn') %>%
  left_join(site_info, by=c('site_observed'='Site_ID'))

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

moran_I_values = npn_observations %>%
  group_by(species, year_observed) %>%
  do(calculate_morans(.)) %>%
  ungroup()

moran_I_values$observed = round(moran_I_values$observed, 2)

moran_table_obs = moran_I_values %>%
  mutate(observed_table = ifelse(p.value<0.05, paste0(observed,'*'), as.character(observed))) %>%
  select(Species=species, year_observed, observed_table) %>%
  spread(year_observed, observed_table)

moran_table_obs$Species = factor(moran_table_obs$Species, levels=old_names, labels=new_names)

table_theme = gridExtra::ttheme_default(base_size = 24)
gridExtra::grid.table(moran_table_obs, rows=NULL, theme = table_theme)


#Maps of error. probably a suppliment material.
#TODO: print moran stats directly on them. 
ggplot(filter(npn_observations, year_observed==2015), aes(x=Longitude, y=Latitude, color=error, size=5)) +
  geom_point() +
  scale_color_gradient2(low='red',mid='white',high='green') +
  facet_wrap(~species)

