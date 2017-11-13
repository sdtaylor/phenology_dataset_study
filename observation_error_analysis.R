library(tidyverse)
library(cowplot)
library(ggforce)
#Compare out of sample verification between datasets
config = yaml::yaml.load_file('config.yaml')

predictions = read_csv(config$predictions_file)
#Keep only species that are present in NPN dataset
npn_species = predictions %>%
  filter(observation_source == 'npn') %>%
  select(species) %>%
  distinct()

predictions = predictions %>%
  filter(species %in% npn_species$species)

#Pull out phenophase and identify as leaf or flower instead of numbers
predictions = predictions %>% 
  mutate(phenophase = stringr::word(species,2,2, ' - '),
         species = stringr::word(species,1,1,' - '))

predictions$phenophase = as.numeric(predictions$phenophase)

phenophase_types = read.table(header=TRUE, sep=',', stringsAsFactors = FALSE, text='
                              phenophase,phenophase_type
                              371,Budburst
                              496,Budburst
                              480,Budburst
                              488,Budburst
                              501,Flowers')

predictions = predictions %>%
  left_join(phenophase_types, by='phenophase') %>%
  select(-phenophase) %>%
  rename(phenophase = phenophase_type)

##################################################################
# Skill scores using the corrosponding LTS dataset as the base estimate
# > 0 means that the NPN datset did better than LTS dataset (the  LTS datset)

# npn_estimates = predictions %>%
#   filter(parameter_source=='npn') %>%
#   rename(npn_doy_estimated = doy_estimated) %>%
#   select(-doy_observed, -parameter_source)
# 
# me_values = predictions %>%
#   filter(parameter_source != 'npn') %>%
#   left_join(npn_estimates, by=c('model_name','observation_source','species','observation_id','phenophase')) %>%
#   group_by(model_name, observation_source, parameter_source, species, phenophase) %>%
#   summarize(me = 1 - (sum((npn_doy_estimated - doy_observed)^2) / sum((doy_estimated - doy_observed)^2)))

##########################################################
#R^2 from a 1:1 line
r2=function(actual, predicted){
  actual=actual
  predicted=predicted
  1 - (sum((actual - predicted) ** 2) / sum((actual - mean(actual)) ** 2))
}

model_errors = predictions %>%
  filter(data_type=='test') %>%
  group_by(model_name, observation_source, parameter_source, species, phenophase) %>%
  summarise(rmse = sqrt(mean((doy_estimated - doy_observed)^2))) %>%
  ungroup() %>%
  gather(error_type, error_value, rmse) %>%
  mutate(error_value = round(error_value,2)) %>%
  mutate(is_lts_model = parameter_source!='npn')

# Apply more pleasing names to everything for figures
model_names = c('gdd','gdd_fixed','linear_temp','naive','uniforc','alternating')
pretty_model_names = c('GDD','Fixed GDD','Linear Temp','Naive','Uniforc','Alternating')
datasets = c('harvard','hjandrews','hubbard','jornada','npn')
pretty_dataset_names = c('Harvard Forest','H.J. Andrews','Hubbard Brook','Jornada','NPN')

model_errors$model_name = factor(model_errors$model_name, levels = model_names, labels = pretty_model_names)
model_errors$parameter_source = factor(model_errors$parameter_source, levels=datasets, labels = pretty_dataset_names)

point_size=4
point_shapes = c(17,13)
r2_lower_limit = 0
rmse_upper_limit = 20
color_pallete=c("grey42", "#E69F00", "#56B4E9", "#CC79A7", "#009E73")

model_errors = model_errors %>%
  mutate(zoomed_subset = ifelse(error_type == 'r2', error_value > r2_lower_limit,
                                error_value < rmse_upper_limit))

npn_rmse_plot = model_errors %>%
  filter(observation_source == 'npn', error_type=='rmse') %>%
  ggplot(aes(x=model_name, y=error_value, group=parameter_source, color=parameter_source)) + 
  geom_jitter(width = 0.2, size=point_size, aes(shape = phenophase)) +
  geom_boxplot(inherit.aes = FALSE, aes(x=model_name, y=error_value), alpha=0, outlier.shape = NA, size=0.8) +
  scale_shape_manual(values=point_shapes) + 
  scale_color_manual(values=color_pallete) +
  theme_linedraw() +
  theme(plot.margin = unit(c(1,0,0,0),'cm')) +
  labs(y='RMSE', x='') + 
  facet_zoom(y = zoomed_subset==TRUE) 

lts_rmse_plot = model_errors %>%
  filter(observation_source != 'npn', error_type=='rmse') %>%
  ggplot(aes(x=model_name, y=error_value, group=parameter_source, color=parameter_source)) + 
  geom_jitter(width = 0.2, size=point_size, aes(shape = phenophase)) +
  geom_boxplot(inherit.aes = FALSE, aes(x=model_name, y=error_value), alpha=0, outlier.shape = NA, size=0.8) +
  scale_shape_manual(values=point_shapes) + 
  scale_color_manual(values=color_pallete) +
  theme_linedraw() +
  labs(y = 'RMSE', x='', color='Parameter Source', shape='Phenophase') +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.background = element_rect(color='black'),
        plot.margin = unit(c(1,0,0,0),'cm')) +
  facet_zoom(y = zoomed_subset==TRUE)

legend_alone = get_legend(lts_rmse_plot)
remove_legend = theme(legend.position = 'none')

top_row = plot_grid(npn_rmse_plot + remove_legend)
bottom_row = plot_grid(lts_rmse_plot + remove_legend)

top_row_text = 'A. Model error for observations in NPN dataset'
bottom_row_text = 'B. Model error for observations in LTS datasets'
plot_grid(top_row, bottom_row, legend_alone, ncol=1, labels=c(top_row_text, bottom_row_text, ''), 
          rel_heights = c(1,1,0.2), hjust=-0.07, vjust=2.7, label_size=12)


#write_csv(error_analysis, 'observation_errors.csv')


###########################################################
# Straight up model comparison to ask "Whats the best NPN model to predict LTS obs.?"
# Beginnings are here.
# spp_short_names = read_csv('final_species_abbr.csv')
# model_errors = model_errors %>% 
#   left_join(spp_short_names, by='species')
# 
# # model comparison extrapolating LTS models out to NPN data
# ggplot(filter(model_errors, observation_source=='npn', is_lts_model==TRUE), aes(x=species_short, y=error_value, group=model_name, color=model_name)) +
#   geom_jitter(width=0.2, size=point_size, aes(shape=phenophase)) +
#   scale_shape_manual(values=point_shapes) +
#   #ylim(0,20) +
#   theme(axis.text.x = element_text(vjust = 0.99, hjust=0, angle=-45, debug = F))
# 
#   scale_color_manual(values=color_pallete) 
# 
# # model comparison extrapolating NPN models to LTS data
# ggplot(filter(model_errors, observation_source!='npn', is_lts_model==FALSE), aes(x=species_short, y=error_value, group=model_name, color=model_name)) +
#   geom_jitter(width=0.2, size=point_size, aes(shape=phenophase)) +
#   scale_shape_manual(values=point_shapes) +
#   theme(axis.text.x = element_text(vjust = 0.99, hjust=0, angle=-45, debug = F))

############################################################
x=ggplot(filter(predictions, observation_source=='hjandrews'), aes(x=doy_observed, y=doy_estimated_mean, group=parameter_source, color=parameter_source)) +
  geom_point() +
  geom_smooth(method='lm', se=FALSE, size=1) +
  geom_abline(intercept=0, slope=1) + 
  #annotate('text', label= 'NPN   Harvard', size=3, x=110, y=150) +
  #geom_text(data=filter(errors, observation_source='harvard'), aes(x=x_placement, y=))
  facet_grid(model_name~species) +
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1),
        axis.text.x = element_text(size = 20), 
        axis.text.y = element_text(size = 20),
        strip.text.x=element_text(size=22),
        axis.text = element_text(size = 20),
        legend.position = c(0.85, 0.18)) + theme(legend.text = element_text(size = 20), 
                                                 legend.title = element_text(size = 20))

ggsave('obs_vs_predicted_hjandrews.png', plot=x, height=40, width=180, units = 'cm', limitsize = FALSE)


###########################################################################3
#Analysis of spatial autocorrelation in NPN errors
#########################################################
#Coordinates of npn sites to get the morans statistic
site_info = read_csv('~/data/phenology/npn/observations_top_20.csv') %>%
  dplyr::select(Site_ID, Latitude, Longitude) %>%
  dplyr::distinct()

npn_observations = predictions %>%
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

