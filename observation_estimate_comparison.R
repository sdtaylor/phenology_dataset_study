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

npn_model_estimates = oos_estimates %>%
  filter(parameter_source == 'npn') %>%
  select(-doy_observed, -parameter_source) %>%
  rename(doy_estimated_npn_model = doy_estimated)

observation_estimate_comparison = oos_estimates %>%
  filter(parameter_source != 'npn') %>%
  rename(doy_estimated_non_npn_model = doy_estimated) %>%
  left_join(npn_model_estimates, by=c('model_name','observation_source','species','observation_id','phenophase')) %>%
  filter(complete.cases(.)) %>%
  mutate(model_estimate_difference = doy_estimated_non_npn_model - doy_estimated_npn_model)


############################################################
#Make density plots of distribution of estimate differences
for(observation_source in c('hubbard','hjandrews','harvard')){
  p_title = paste('Observations from ',observation_source)
p = ggplot(filter(observation_estimate_comparison, observation_source==observation_source), 
           aes(model_estimate_difference, fill=as.factor(phenophase), group=as.factor(phenophase))) +
  geom_density(aes(y=..scaled..), alpha=0.8) +
  geom_vline(xintercept = 0) +
  ggtitle(p_title) +
  xlab('Difference between long term dataset model estimate and NPN model estimate') +
  ylab('Density') +
  facet_grid(model_name~species) +
  theme_bw()

print(p)
}

##############################################################
#Fancy table of all estimate differences and their p-values of being different from 0
#0 implies that they generally give similar estimates
library(tables)

#Convert each unique entity to a p-value
p_values = observation_estimate_comparison %>%
  group_by(model_name, observation_source, species, phenophase) %>%
  filter(model_name != 'naive') %>%
  dplyr::summarize(p_value = round(t.test(model_estimate_difference)$p.value, 3)) %>%
  ungroup() %>%
  spread(model_name, p_value)

















