library(tidyverse)
library(cowplot)
#Compare out of sample verification between datasets


oos_data = read_csv('./results/out_of_sample_doy_estimates.csv')
#Keep only species that are present in NPN dataset
npn_species = oos_data %>%
  filter(observation_source == 'npn') %>%
  select(species) %>%
  distinct()

oos_data = oos_data %>%
  filter(species %in% npn_species$species)

#Compress the bootstrapped estimates down to a single estimate
oos_estimates = oos_data %>%
  group_by(model_name, observation_source, parameter_source, species, observation_id) %>%
  summarise(n=n(), doy_estimated = mean(doy_estimated), doy_observed = mean(doy_observed), doy_observed_max = max(doy_observed)) %>%
  ungroup()

#Sanity checks. this should all equal (ie. only one unique observed value per site/model/species/observation_id/etc)
if(!with(oos_estimates, all(doy_observed==doy_observed_max))){stop('more than 1 observation/observation_id')}
#And only 250 estimates (from 250 bootstraps) per observation id
if(unique(oos_estimates$n) != 250){stop('Too many samples per observation_id')}

oos_estimates = oos_estimates %>%
  select(-doy_observed_max, -n)

#Pull out phenophase and identify as leaf or flower instead of numbers
oos_estimates = oos_estimates %>% 
  mutate(phenophase = stringr::word(species,2,2, ' - '),
         species = stringr::word(species,1,1,' - '))

oos_estimates$phenophase = as.numeric(oos_estimates$phenophase)

phenophase_types = read.table(header=TRUE, sep=',', stringsAsFactors = FALSE, text='
                              phenophase,phenophase_type
                              371,Budburst
                              480,Budburst
                              488,Budburst
                              501,Flowers')

oos_estimates = oos_estimates %>%
  left_join(phenophase_types, by='phenophase') %>%
  select(-phenophase) %>%
  rename(phenophase = phenophase_type)

##################################################################
# Skill scores using the corrosponding LTS dataset as the base estimate
# > 0 means that the NPN datset did better than LTS dataset (the  LTS datset)

# npn_estimates = oos_estimates %>%
#   filter(parameter_source=='npn') %>%
#   rename(npn_doy_estimated = doy_estimated) %>%
#   select(-doy_observed, -parameter_source)
# 
# me_values = oos_estimates %>%
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

model_errors = oos_estimates %>%
  group_by(model_name, observation_source, parameter_source, species, phenophase) %>%
  summarise(rmse = sqrt(mean((doy_estimated - doy_observed)^2)),
            r2   = r2(doy_observed, doy_estimated), n=n()) %>%
  ungroup() %>%
  gather(error_type, error_value, rmse, r2) %>%
  mutate(error_value = round(error_value,2))

# Apply more pleasing names to everything for figures
model_names = c('gdd','gdd_fixed','linear_temp','naive','unichill','uniforc')
pretty_model_names = c('GDD','Fixed GDD','Linear Temp','Naive','Unichill','Uniforc')
datasets = c('harvard','hjandrews','hubbard','jornada','npn')
pretty_dataset_names = c('Harvard Forest','H.J. Andrews','Hubbard Brook','Jornada','NPN')

model_errors$model_name = factor(model_errors$model_name, levels = model_names, labels = pretty_model_names)
model_errors$parameter_source = factor(model_errors$parameter_source, levels=datasets, labels = pretty_dataset_names)

point_size=3
point_shapes = c(17,13)
color_pallete=c("grey42", "#E69F00", "#56B4E9", "#CC79A7", "#009E73")

npn_r2_error_plot = model_errors %>%
  filter(observation_source == 'npn', parameter_source!='NPN', error_type=='r2') %>%
  ggplot(aes(x=model_name, y=error_value, group=parameter_source, color=parameter_source)) + 
  geom_jitter(width = 0.2, size=point_size, aes(shape = phenophase)) +
  # scale_size_continuous(breaks=c(100,200,500,5000)) + 
  ylim(0,1) +
  scale_shape_manual(values=point_shapes) +
  scale_color_manual(values=color_pallete) +
  theme_linedraw() +
  labs(y = 'R^2', x='') 

npn_rmse_error_plot = model_errors %>%
  filter(observation_source == 'npn', parameter_source!='NPN', error_type=='rmse') %>%
  ggplot(aes(x=model_name, y=error_value, group=parameter_source, color=parameter_source)) + 
  geom_jitter(width = 0.2, size=point_size, aes(shape = phenophase)) +
  # scale_size_continuous(breaks=c(100,200,500,5000)) + 
  ylim(0,100) +
  scale_shape_manual(values=point_shapes) + 
  scale_color_manual(values=color_pallete) +
  theme_linedraw() +
  labs(y='RMSE', x='') 

lts_r2_error_plot = model_errors %>%
  filter(observation_source != 'npn', parameter_source=='NPN', error_type=='r2') %>%
  ggplot(aes(x=model_name, y=error_value, group=parameter_source, color=parameter_source)) + 
  geom_jitter(width = 0.2, size=point_size, aes(shape = phenophase)) +
  # scale_size_continuous(breaks=c(100,200,500,5000)) + 
  ylim(0,1) +
  scale_shape_manual(values=point_shapes) + 
  scale_color_manual(values=color_pallete) +
  theme_linedraw() +
  labs(y = 'R^2', x='') 

lts_rmse_error_plot = model_errors %>%
  filter(observation_source != 'npn', parameter_source=='NPN', error_type=='rmse') %>%
  ggplot(aes(x=model_name, y=error_value, group=parameter_source, color=parameter_source)) + 
  geom_jitter(width = 0.2, size=point_size, aes(shape = phenophase)) +
  # scale_size_continuous(breaks=c(100,200,500,5000)) + 
  ylim(0,40) +
  scale_shape_manual(values=point_shapes) + 
  scale_color_manual(values=color_pallete) +
  theme_linedraw() +
  labs(y = 'RMSE', x='', color='Parameter Source', shape='Phenophase') +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal',
        legend.background = element_rect(color='black'))

legend_alone = get_legend(lts_rmse_error_plot)
remove_legend = theme(legend.position = 'none')

top_row = plot_grid(npn_r2_error_plot + remove_legend, npn_rmse_error_plot + remove_legend)
bottom_row = plot_grid(lts_r2_error_plot + remove_legend, lts_rmse_error_plot + remove_legend)

top_row_text = 'A. Model error metrics for observations in NPN dataset'
bottom_row_text = 'B. Model error metrics for observations in LTS datasets'
plot_grid(top_row, bottom_row, legend_alone, ncol=1, labels=c(top_row_text, bottom_row_text, ''), 
          rel_heights = c(1,1,0.2), hjust=-0.07, vjust=0.5, label_size=12)


#write_csv(error_analysis, 'observation_errors.csv')

############################################################
x=ggplot(filter(oos_estimates, observation_source=='hjandrews'), aes(x=doy_observed, y=doy_estimated_mean, group=parameter_source, color=parameter_source)) +
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

