library(tidyverse)

# During review there was a question of the sensitivity of the 30 day threshold to NPN observations (see methods)
# I tested that sensitivity by running all analysis again with a 15 day cutoff. This compares some of the stats on 
# that (ie. the decrease in sample size from a more conservative cutoff) and also plots the 1:1 comparison of 
# each estimated parameter between the two cutoffs


##########################
# Figure S1, the 1:1 charts of different parameters

cutoff_15_day = read_csv('results/cutoff_15days/model_parameters.csv.gz')
cutoff_15_day$cutoff = 'cutoff_15'

cutoff_30_day = read_csv('results/cutoff_30days/model_parameters.csv.gz') 
cutoff_30_day$cutoff = 'cutoff_30'

both = cutoff_15_day %>%
  bind_rows(cutoff_30_day) %>%
  filter(dataset=='npn', !parameter_name %in% c('run_time','num_iterations') )  %>%
  group_by(model, parameter_name, species, cutoff) %>%
  summarise(parameter_value = mean(value)) %>%
  ungroup() %>%
  spread(cutoff, parameter_value) %>%
  fortify() %>%
  filter(complete.cases(.))

total_sensitivity_comparisons = length(unique(both$species))

sensitivity_figure = ggplot(both, aes(x=cutoff_15, y=cutoff_30)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope=1) + 
  facet_wrap(model~parameter_name, scales='free') + 
  labs(x='Parameter estimates using a 15 day cutoff',
       y='Parameter estimates using a 30 day cutoff')

ggsave('manuscript/figure_s1_parameter_estimates_from_cutoff_sensitivity.png', plot=sensitivity_figure, height=20, width=30,dpi=200, units = 'cm')

#############################################
##########################
# The decrease in sample size moving from a 30 day to a 15 day cutoff
npn_species = cutoff_30_day %>%
  filter(!parameter_name %in% c('run_time','num_iterations')) %>%
  select(dataset, species) %>%
  filter(dataset=='npn') %>%
  distinct()

###################

sample_sizes_15d = read_csv('results/cutoff_15days/predictions.csv.gz') %>%
  select(observation_source, species, observation_id) %>%
  distinct() %>%
  filter(species %in% npn_species$species) %>%
  filter(observation_source=='npn') %>%
  group_by(species) %>%
  summarise(sample_size_15d = n()) %>%
  ungroup()

sample_sizes_30d = read_csv('results/cutoff_30days/predictions.csv.gz') %>%
  select(observation_source, species, observation_id) %>%
  distinct() %>%
  filter(species %in% npn_species$species) %>%
  filter(observation_source=='npn') %>%
  group_by(species) %>%
  summarize(sample_size_30d = n()) %>%
  ungroup()

all_sample_sizes = sample_sizes_15d %>%
  full_join(sample_sizes_30d, by='species') %>%
  mutate(phenophase = stringr::word(species,2,2, ' - '),
         species = stringr::word(species,1,1,' - '))

all_sample_sizes$n_decrease = all_sample_sizes$sample_size_30d - all_sample_sizes$sample_size_15d
all_sample_sizes$n_percent_decrease = round(all_sample_sizes$n_decrease / all_sample_sizes$sample_size_15d, 2)

# Total unique species in the analaysis
total_30_day_spp = all_sample_sizes %>%
  filter(!is.na(sample_size_30d)) %>%
  select(species) %>%
  distinct() %>%
  nrow()

total_15_day_spp = all_sample_sizes %>%
  filter(!is.na(sample_size_15d)) %>%
  select(species) %>%
  distinct() %>%
  nrow()

# Total species/phenophase combinations for comparison. Since 1 species can have either budburst, flowering, or both.
# add 3 to both of these to account for 3 species being in both Harvard and Hubbard Brook
spp_phenophase_combos_30day = sum(!is.na(all_sample_sizes$sample_size_30d))
spp_phenophase_combos_15day = sum(!is.na(all_sample_sizes$sample_size_15d))

total_n_decrease = sum(all_sample_sizes$sample_size_30d) - sum(all_sample_sizes$sample_size_15d, na.rm=T)
total_n_decrease_percent = (sum(all_sample_sizes$sample_size_30d) - sum(all_sample_sizes$sample_size_15d, na.rm=T)) / sum(all_sample_sizes$sample_size_30d)

                                                                
