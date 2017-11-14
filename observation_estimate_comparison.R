library(tidyverse)
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
480,Budburst
488,Budburst
501,Flowers')

predictions = predictions %>%
  left_join(phenophase_types, by='phenophase') %>%
  select(-phenophase) %>%
  rename(phenophase = phenophase_type)

############################################################

npn_model_estimates = predictions %>%
  filter(parameter_source == 'npn') %>%
  select(-doy_observed, -parameter_source) %>%
  rename(doy_estimated_npn_model = doy_estimated)

observation_estimate_comparison = predictions %>%
  filter(parameter_source != 'npn') %>%
  rename(doy_estimated_non_npn_model = doy_estimated, non_npn_parameter_source = parameter_source) %>%
  left_join(npn_model_estimates, by=c('model_name','observation_source','species','observation_id','phenophase')) %>%
  filter(complete.cases(.)) %>%
  mutate(model_estimate_difference = doy_estimated_non_npn_model - doy_estimated_npn_model)


############################################################
#Make density plots of distribution of estimate differences
# for(this_observation_source in c('hubbard','hjandrews','harvard')){
#   p_title = paste('Observations from ',this_observation_source)
#   p_filename = paste0('obs_vs_predicted_compare_',this_observation_source,'.png')
# p = ggplot(filter(observation_estimate_comparison, observation_source==this_observation_source), 
#            aes(model_estimate_difference, fill=as.factor(phenophase), group=as.factor(phenophase))) +
#   #geom_density(aes(y=..scaled..), alpha=0.8) +
#   geom_histogram(bins=50, position = 'identity', alpha=0.7) +
#   scale_fill_brewer(palette = 'Set2') +
#   geom_vline(xintercept = 0) +
#   ggtitle(p_title) +
#   xlab('Difference between long term dataset model estimate and NPN model estimate') +
#   ylab('Density') +
#   facet_wrap(model_name~species, nrow=5, scales='free_y') +
#   theme_bw() +  
#   theme(legend.position = "bottom", 
#         legend.direction = "horizontal")

# print(p)
# ggsave(p_filename, plot=p, height=20, width=80, units = 'cm', limitsize = FALSE)

# }

##############################################################
#Fancy table of all estimate differences and their p-values of being different from 0
#0 implies that they generally give similar estimates

#Convert each unique entity to a p-value
# p_values = observation_estimate_comparison %>%
#   group_by(model_name, observation_source, species, phenophase) %>%
#   filter(model_name != 'naive') %>%
#   dplyr::summarize(p_value = round(t.test(model_estimate_difference)$p.value, 3), n=n()) %>%
#   ungroup() %>%
#   spread(model_name, p_value)
# 
# write_csv(p_values, 'observation_estimate_comparison_p_values.csv')
# gridExtra::grid.table(p_values)

#############################################################
#Explain differences in observations by either number of observers
#or size of the sampling error in the npn dataset

npn_sampling_info = read_csv('cleaned_data/npn_species_sampling_data.csv')
npn_distances = read_csv('cleaned_data/npn_mean_distance_to_long_term_sites.csv')


# Mean absolute difference between long_term_study models and npn_models. 
# < 0 means npn model estimated higher (later in year) doy

estimate_differences_lts_observations = observation_estimate_comparison %>%
  group_by(model_name, observation_source, non_npn_parameter_source, species, phenophase) %>%
  summarise(rmsd = sqrt(mean(model_estimate_difference^2))) %>%
  ungroup() %>%
  filter(observation_source == non_npn_parameter_source) %>%
  left_join(npn_sampling_info, by='species') %>%
  left_join(npn_distances, by=c('species','observation_source'='dataset'))

estimate_differences_npn_observations = observation_estimate_comparison %>%
  group_by(model_name, observation_source, non_npn_parameter_source, species, phenophase) %>%
  summarise(rmsd = sqrt(mean(model_estimate_difference^2))) %>%
  ungroup() %>%
  filter(observation_source == 'npn') %>%
  left_join(npn_sampling_info, by='species') %>%
  left_join(npn_distances, by=c('species','observation_source'='dataset'))

################################################################
# Graph of RMSD model, phenophase, lts dataset

# Apply more pleasing names to everything for figures
model_names = c('gdd','gdd_fixed','linear_temp','naive','alternating','uniforc')
pretty_model_names = c('GDD','Fixed GDD','Linear','Naive','Alternating','Uniforc')
datasets = c('harvard','hjandrews','hubbard','jornada','npn')
pretty_dataset_names = c('Harvard Forest','H.J. Andrews','Hubbard Brook','Jornada','NPN')

estimate_differences_lts_observations$model_name = factor(estimate_differences_lts_observations$model_name, levels=model_names, labels=pretty_model_names)
estimate_differences_npn_observations$model_name = factor(estimate_differences_npn_observations$model_name, levels=model_names, labels=pretty_model_names)
estimate_differences_lts_observations$non_npn_parameter_source = factor(estimate_differences_lts_observations$non_npn_parameter_source,
                                                                           levels=datasets, labels=pretty_dataset_names)
estimate_differences_npn_observations$non_npn_parameter_source = factor(estimate_differences_npn_observations$non_npn_parameter_source,
                                                                           levels=datasets, labels=pretty_dataset_names)

point_size=6
point_shapes = c(17,13)
color_pallete=c("grey42", "#E69F00", "#56B4E9", "#CC79A7")

common_theme_elements = theme(axis.text = element_text(size=18),
                              axis.title.x = element_text(size=20),
                              panel.grid.major.y = element_line(colour = "grey80", size=0.5),
                              panel.grid.minor.y = element_line(colour = "grey85", size=0.5))

npn_estimate_differences = ggplot(estimate_differences_npn_observations, aes(x=model_name, y=rmsd, group=non_npn_parameter_source, color=non_npn_parameter_source)) + 
  geom_jitter(width = 0.2, size=point_size, aes(shape = phenophase)) +
  geom_boxplot(inherit.aes = FALSE, aes(x=model_name, y=rmsd), alpha=0) +
  ylim(0,70) +
  scale_shape_manual(values=point_shapes) + 
  scale_color_manual(values=color_pallete) +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        legend.position = 'none',
        plot.margin = unit(c(1,0,0.5,0),'cm')) +
  common_theme_elements +
  labs(x='',y='')

lts_estimate_differences = ggplot(estimate_differences_lts_observations, aes(x=model_name, y=rmsd, group=non_npn_parameter_source, color=non_npn_parameter_source)) + 
  geom_jitter(width = 0.2, size=point_size, aes(shape = phenophase)) +
  geom_boxplot(inherit.aes = FALSE, aes(x=model_name, y=rmsd), alpha=0) +
  ylim(0,70) +
  scale_shape_manual(values=point_shapes) + 
  scale_color_manual(values=color_pallete) +
  theme_bw() +
  theme(legend.position = c(0.35,0.8),
        legend.box = 'horizontal',
        legend.text = element_text(size=15),
        legend.title = element_text(size=15),
        legend.background = element_rect(color='black'),
        plot.margin = unit(c(1,0,0.5,0),'cm')) +
  common_theme_elements +
  labs(y='',x='Model',color='LTS Parameter Source', shape='Phenophase')

npn_label = 'A. Root mean square difference between NPN and LTS estimates when compared over all NPN sites'
lts_label = 'B. Root mean square difference between NPN and LTS estimates when compared at local LTS sites'
cowplot::plot_grid(npn_estimate_differences, lts_estimate_differences, labels=c(npn_label, lts_label), ncol=1,
                   hjust=-0.08, vjust=1.1, label_size=12)

# The median of npn observations with the unichill model. This is reported in text since it's cutoff on the graph
estimate_differences_lts_observations %>%
  filter(model_name=='Unichill') %>%
  pull(rmsd) %>%
  median()
#################################################################
# Models explaining RMSD
fixed_gdd_lts_rmsd = estimate_differences_lts_observations %>%
  filter(model_name=='Fixed GDD')

all_results = estimate_differences_lts_observations %>%
  group_by(model_name) %>%
  do(broom::tidy(lm(log(.$rmsd) ~ log(.$num_observers) + log(.$mean_distance)))) %>%
  ungroup() %>%
  mutate(is_sig = p.value<0.05)


fixed_gdd_model = lm(log(rmsd) ~ log(num_observers) + log(mean_distance) + log(area_sampled), data=fixed_gdd_lts_rmsd)
fixed_gdd_model_step = step(fixed_gdd_model, direction = 'both')


summary(lm(log(rmsd) ~ log(num_observers) + log(mean_distance) + model_name, data=estimate_differences_lts_observations))

summary(lm(rmsd ~ num_observers + mean_distance, data=estimate_differences))



random_effects_model = lmer(log(rmsd) ~ log(num_observers) + log(mean_distance) + (log(num_observers) + log(mean_distance)|model_name), data=estimate_differences_lts_observations)
summary(random_effects_model)

ggplot(filter(estimate_differences, num_observers<200), aes(x=num_observers, y=rmsd, group=model_name, color=model_name)) +
  geom_point() +
  geom_smooth(method = 'lm', se=FALSE)





