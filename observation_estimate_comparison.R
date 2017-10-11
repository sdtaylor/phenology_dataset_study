library(tidyverse)
#Compare out of sample verification between datasets


oos_data = read_csv('./results/out_of_sample_doy_estimates.csv')
#Keep only species that are present in NPN dataset
npn_species = oos_data %>%
  filter(observation_source == 'npn') %>%
  select(species) %>%
  distinct()


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
371,Leaves
480,Leaves
488,Leaves
501,Flowers')

oos_estimates = oos_estimates %>%
  left_join(phenophase_types, by='phenophase') %>%
  select(-phenophase) %>%
  rename(phenophase = phenophase_type)

############################################################
#Add an ensemble model

# ensemble_model_estimates = oos_estimates %>%
#   group_by(observation_source, parameter_source, species, observation_id, phenophase) %>%
#   summarise(doy_estimated = mean(doy_estimated), doy_observed = mean(doy_observed)) %>%
#   ungroup() %>%
#   mutate(model_name='ensemble')
# 
# oos_estimates = oos_estimates %>%
#   bind_rows(ensemble_model_estimates)

############################################################

npn_model_estimates = oos_estimates %>%
  filter(parameter_source == 'npn') %>%
  select(-doy_observed, -parameter_source) %>%
  rename(doy_estimated_npn_model = doy_estimated)

observation_estimate_comparison = oos_estimates %>%
  filter(parameter_source != 'npn') %>%
  rename(doy_estimated_non_npn_model = doy_estimated, non_npn_parameter_source = parameter_source) %>%
  left_join(npn_model_estimates, by=c('model_name','observation_source','species','observation_id','phenophase')) %>%
  filter(complete.cases(.)) %>%
  mutate(model_estimate_difference = doy_estimated_non_npn_model - doy_estimated_npn_model)


############################################################
#Make density plots of distribution of estimate differences
for(this_observation_source in c('hubbard','hjandrews','harvard')){
  p_title = paste('Observations from ',this_observation_source)
  p_filename = paste0('obs_vs_predicted_compare_',this_observation_source,'.png')
p = ggplot(filter(observation_estimate_comparison, observation_source==this_observation_source), 
           aes(model_estimate_difference, fill=as.factor(phenophase), group=as.factor(phenophase))) +
  geom_density(aes(y=..scaled..), alpha=0.8) +
  #geom_histogram(bins=50, position = 'identity', alpha=0.7) +
  scale_fill_brewer(palette = 'Set2') +
  geom_vline(xintercept = 0) +
  ggtitle(p_title) +
  xlab('Difference between long term dataset model estimate and NPN model estimate') +
  ylab('Density') +
  facet_grid(model_name~species) +
  theme_bw() +  
  theme(legend.position = "bottom", 
        legend.direction = "horizontal")

print(p)
#ggsave(p_filename, plot=p, height=20, width=80, units = 'cm', limitsize = FALSE)

}

##############################################################
#Fancy table of all estimate differences and their p-values of being different from 0
#0 implies that they generally give similar estimates

#Convert each unique entity to a p-value
p_values = observation_estimate_comparison %>%
  group_by(model_name, observation_source, species, phenophase) %>%
  filter(model_name != 'naive') %>%
  dplyr::summarize(p_value = round(t.test(model_estimate_difference)$p.value, 3), n=n()) %>%
  ungroup() %>%
  spread(model_name, p_value)

write_csv(p_values, 'observation_estimate_comparison_p_values.csv')
gridExtra::grid.table(p_values)

#############################################################
#Explain differences in observations by either number of observers
#or size of the sampling error in the npn dataset

npn_sampling_info = read_csv('cleaned_data/npn_species_sampling_data.csv')
npn_distances = read_csv('cleaned_data/npn_mean_distance_to_long_term_sites.csv')


# Mean absolute difference between long_term_study models and npn_models. 
# < 0 means npn model estimated higher (later in year) doy

estimate_differences = observation_estimate_comparison %>%
  group_by(model_name, observation_source, non_npn_parameter_source, species, phenophase) %>%
  summarise(rmsd = sqrt(mean(model_estimate_difference^2))) %>%
  ungroup() %>%
  filter(observation_source == non_npn_parameter_source) %>%
  left_join(npn_sampling_info, by='species') %>%
  left_join(npn_distances, by=c('species','observation_source'='dataset'))

summary(lm(log(rmsd) ~ log(num_observers) + log(mean_distance) + model_name, data=estimate_differences))

summary(lm(rmsd ~ num_observers + mean_distance, data=estimate_differences))



random_effects_model = lmer(log(rmsd) ~ log(num_observers) + log(mean_distance) + (log(num_observers) + log(mean_distance)|model_name), data=estimate_differences)
summary(random_effects_model)

ggplot(filter(estimate_differences, num_observers<200), aes(x=num_observers, y=rmsd, group=model_name, color=model_name)) +
  geom_point() +
  geom_smooth(method = 'lm', se=FALSE)





