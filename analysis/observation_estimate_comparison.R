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
496,Budburst
501,Flowers')

predictions = predictions %>%
  left_join(phenophase_types, by='phenophase') %>%
  select(-phenophase) %>%
  rename(phenophase = phenophase_type)

########################################################
# Only use estimates for held out observations
predictions = predictions %>%
  filter(data_type=='test')

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
############################################################

npn_model_estimates = predictions %>%
  filter(parameter_source == 'npn') %>%
  select(-doy_observed, -parameter_source) %>%
  rename(doy_estimated_npn_model = doy_estimated)

observation_estimate_comparison = predictions %>%
  filter(parameter_source != 'npn') %>%
  # Exclude the 3 common species between hubbard and harvard in this comparison
  filter(!(observation_source == 'hubbard' & parameter_source == 'harvard')) %>%
  filter(!(observation_source == 'harvard' & parameter_source == 'hubbard')) %>%
  rename(doy_estimated_non_npn_model = doy_estimated, non_npn_parameter_source = parameter_source) %>%
  left_join(npn_model_estimates, by=c('model_name','observation_source','species','observation_id','phenophase')) %>%
  filter(complete.cases(.)) %>%
  mutate(model_estimate_difference = doy_estimated_non_npn_model - doy_estimated_npn_model)

observation_estimate_comparison = observation_estimate_comparison %>%
  mutate(is_lts_obs = case_when(
    observation_source=='npn'~'At NPN Sites',
    observation_source!='npn'~'At LTER Sites'
  ))

model_names = c('gdd','m1','gdd_fixed','linear_temp','naive','alternating','msb','uniforc')
pretty_model_names = c('GDD','M1','Fixed GDD','Linear','Naive','Alternating','MSB','Uniforc')
observation_estimate_comparison$model_name = factor(observation_estimate_comparison$model_name, levels=model_names, labels = pretty_model_names)

r2_values = observation_estimate_comparison %>%
  group_by(model_name, is_lts_obs) %>%
  summarise(r2= 1 - (sum((doy_estimated_npn_model - doy_estimated_non_npn_model)**2) / sum((doy_estimated_npn_model - mean(doy_estimated_npn_model))**2))) %>%
  ungroup() %>%
  mutate(plot_text=paste('R^2 == ',round(r2,2)),
         text_y_pos=205, text_x_pos=60)

fig2=ggplot(observation_estimate_comparison, aes(x=doy_estimated_npn_model, y=doy_estimated_non_npn_model, color=interaction(species,phenophase))) +
  geom_point(alpha=0.6) + 
  geom_text(data=r2_values, aes(x=text_x_pos, y=text_y_pos, label=plot_text), inherit.aes = FALSE, parse=TRUE) +
  geom_abline(slope=1, intercept = 1) +
  scale_color_viridis_d() +
  facet_grid(is_lts_obs~model_name) +
  theme_bw() + 
  theme(legend.position = 'none') + 
  labs(x='Estimates from NPN derived models',
       y='Estimates from LTER derived models')

ggsave(fig2, filename = paste0(config$image_save_directory,'figure_estimate_compare.png'), height = 14, width = 40, units = 'cm')
