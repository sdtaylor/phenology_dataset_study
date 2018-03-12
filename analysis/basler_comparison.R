library(tidyverse)
library(patchwork)
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

#########################################################
# Account for spatially corrected models. See note in
# compare_parameters.R

# Remove spatial models fit to LTS data
predictions = predictions %>%
  filter(!(parameter_source!='npn' & model_name %in% c('msb','m1')))

# Copy the LTS GDD and Alternating model to compare with the
# corrected NPN ones.
lts_models = predictions %>%
  filter(parameter_source!='npn', model_name %in% c('gdd','alternating'))
lts_models$model_name = with(lts_models, ifelse(model_name=='gdd','m1',
                                                ifelse(model_name=='alternating','msb','unk')))
if(any(lts_models$model_name=='unk')){stop('unknown model in lts subset')}  

predictions = predictions %>%
  bind_rows(lts_models)

rm(lts_models)


##########################################################

model_errors = predictions %>%
  group_by(data_type, model_name, observation_source, parameter_source, species, phenophase) %>%
  summarise(rmse = sqrt(mean((doy_estimated - doy_observed)^2)), sample_size=n()) %>%
  ungroup() %>%
  mutate(rmse = round(rmse,2)) %>%
  mutate(is_lts_model = parameter_source!='npn', is_lts_obs = observation_source != 'npn')

# Apply more pleasing names to everything for figures
model_names = c('gdd','m1','gdd_fixed','linear_temp','naive','alternating','msb','uniforc')
pretty_model_names = c('GDD','M1','Fixed GDD','Linear','Naive','Alternating','MSB','Uniforc')
datasets = c('harvard','hjandrews','hubbard','jornada','npn')
pretty_dataset_names = c('Harvard Forest','H.J. Andrews','Hubbard Brook','Jornada','NPN')

model_errors$model_name = factor(model_errors$model_name, levels = model_names, labels = pretty_model_names)
model_errors$parameter_source = factor(model_errors$parameter_source, levels=datasets, labels = pretty_dataset_names)
model_errors$observation_source = factor(model_errors$observation_source, levels=datasets, labels = pretty_dataset_names)


###########################################################

npn_lts_comparison = model_errors %>%
  filter(data_type=='test') %>%
  filter(observation_source != 'Hubbard Brook', parameter_source!='Hubbard Brook') %>%
  mutate(scenario = case_when(
                        is_lts_model & is_lts_obs ~ 'A',
                        is_lts_model & (!is_lts_obs) ~'B',
                        (!is_lts_model) & (!is_lts_obs) ~'C',
                        (!is_lts_model) & is_lts_obs ~ 'D'
  ))

npn_lts_comparison = npn_lts_comparison %>%
  select(model_name, species, phenophase, rmse, scenario) %>%
  spread(key=scenario, value=rmse) %>%
  mutate(A_C_diff = A-C, C_B_diff = C-B) %>%
  select(-A,-B,-C,-D) %>%
  gather(comparison, difference, A_C_diff, C_B_diff) %>%
  mutate(data_from = 'this_study')


##########################################################
basler_data = tribble(
  ~species, ~method, ~scenario, ~rmse,
  'aesculus hippocastanum','external validation','B',9,
  'aesculus hippocastanum','pooled validation','C',8,
  'aesculus hippocastanum','site-specific validation','A',5,
  
  'betula pendula','external validation','B',8,
  'betula pendula','pooled validation','C',7,
  'betula pendula','site-specific validation','A',4,
  
  'fagus sylvatica','external validation','B',8.5,
  'fagus sylvatica','pooled validation','C',7,
  'fagus sylvatica','site-specific validation','A',5,
  
  'quercus robur','external validation','B',9,
  'quercus robur','pooled validation','C',7.5,
  'quercus robur','site-specific validation','A',5,
  
  'larix decidua','external validation','B',10,
  'larix decidua','pooled validation','C',9,
  'larix decidua','site-specific validation','A',6,
  
  'picea abies','external validation','B',9,
  'picea abies','pooled validation','C',8,
  'picea abies','site-specific validation','A',5
)

basler_data = basler_data %>%
  select(-method) %>%
  spread(scenario, rmse) %>%
  mutate(A_C_diff = A-C, C_B_diff = C-B) %>%
  select(-A,-C,-B) %>%
  gather(comparison, difference, A_C_diff, C_B_diff) %>%
  mutate(data_from='basler2016')

############################################################

basler_mean_values = basler_data %>%
  group_by(comparison) %>%
  summarise(basler_value = mean(difference))

comparison_names = c('A_C_diff','C_B_diff')
pretty_comparison_names = c('Scenario A - Scenario C','Scenario C - Scenario B')
npn_lts_comparison$comparison = factor(npn_lts_comparison$comparison, levels=comparison_names, labels=pretty_comparison_names)
basler_mean_values$comparison = factor(basler_mean_values$comparison, levels=comparison_names, labels=pretty_comparison_names)

# t_stats = npn_lts_comparison %>%
#   group_by(model_name, comparison) %>%
#   summarize(mean_value = mean(difference), sd=sd(difference), se=sd(difference)/sqrt(n())) %>%
#   ungroup() %>%
#   left_join(basler_mean_values) %>%
#   mutate(t_stat = (basler_value - mean_value) / se) %>%
#   mutate(p_value = 2 * pnorm(-abs(t_stat)))

basler_figure = ggplot(npn_lts_comparison, aes(difference)) + 
  #geom_histogram(bins=40, fill='grey50') + 
  geom_density(fill='grey70') +
  geom_vline(data=basler_mean_values, aes(xintercept=basler_value), color='black',size=1.4 ) +
  facet_grid(comparison~model_name) +
  theme_bw() +
  labs(y='',x='Difference between scenarios') +
  theme(strip.text.x = element_text(size=20),
        strip.text.y = element_text(size=12),
        axis.text = element_text(size=15),
        axis.title.x = element_text(size=20),
        strip.background = element_rect(fill='grey95'))

ggsave(basler_figure, filename = 'manuscript/basler_figure.png', width = 60, height = 15, units = 'cm')
