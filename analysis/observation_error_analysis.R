library(tidyverse)
library(kableExtra)
library(knitr)

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
  summarise(rmse = sqrt(mean((doy_estimated - doy_observed)^2)), 
            pearson = cor(doy_estimated, doy_observed, method='pearson'),
              sample_size=n()) %>%
  ungroup() %>%
  gather(error_type, error_value, rmse, pearson) %>%
  mutate(error_value = round(error_value,4)) %>%
  mutate(is_lts_model = parameter_source!='npn', is_lts_obs = observation_source != 'npn',
         is_npn_model = !is_lts_model, is_npn_obs = !is_lts_obs)

# Take out harvard/hubbard comparisons since they have species in common
model_errors = model_errors %>%
  filter(!(parameter_source=='hubbard' & observation_source=='harvard')) %>%
  filter(!(parameter_source=='harvard' & observation_source=='hubbard'))


# Apply more pleasing names to everything for figures
model_names = c('gdd','m1','gdd_fixed','linear_temp','naive','alternating','msb','uniforc')
pretty_model_names = c('GDD','M1','Fixed GDD','Linear','Naive','Alternating','MSB','Uniforc')
datasets = c('harvard','hjandrews','hubbard','jornada','npn')
pretty_dataset_names = c('Harvard Forest','H.J. Andrews','Hubbard Brook','Jornada','NPN')


model_errors %>%
  filter(model_name == 'gdd',
         error_type == 'rmse',
         data_type=='test',
         parameter_source != 'hubbard',
         is_npn_obs) %>%
  mutate(model_source = ifelse(is_npn_model, 'npn_model_error','lter_model_error')) %>%
  #mutate(sample_id = 1:n()) %>%
  select(data_type, species, phenophase, error_value, model_source) %>%
  spread(model_source, error_value) %>%
  ggplot(aes(x=lter_model_error, y=npn_model_error)) +
  geom_point() +
  geom_abline(intercept = 0, slope=1)


##############
# all the errors, not just RMSE
# point plots with NPN on x axis and LTER on y axis, split out by species.
predictions %>%
  mutate(model_source = ifelse(parameter_source=='npn', 'npn_model','lter_model')) %>%
  filter(model_name == 'uniforc',
         data_type=='test',
         parameter_source != 'hubbard') %>%
  filter(species %in% c('acer rubrum',"quercus rubra","populus tremuloides","fagus grandifolia",'rhododendron macrophyllum','betula lenta')) %>%
  #filter(species %in% c("acer circinatum")) %>%
  mutate(error = doy_estimated - doy_observed) %>%
  mutate(species = snakecase::to_sentence_case(species)) %>%
  select(data_type, species, phenophase,year_observed,observation_source, observation_id, error, model_source) %>%
  spread(model_source, error) %>%
ggplot(aes(x=lter_model, y=npn_model)) +
  geom_point(size=4, color='#56B4E9') +
  geom_abline(intercept = 0, slope=1) +
  facet_wrap(~species, scales='free') +
  theme_bw(30) +
  labs(x='USA-NPN Model Error',y='LTER Model Error') 



##############
predictions %>%
  mutate(model_source = ifelse(parameter_source=='npn', 'npn_model','lter_model')) %>%
  filter(model_name == 'gdd',
         data_type=='test',
         observation_source == 'npn',
         parameter_source != 'hubbard') %>%
  filter(species %in% c('acer rubrum',"quercus rubra","populus tremuloides","fagus grandifolia",'rhododendron macrophyllum','betula lenta')) %>%
  #filter(species %in% c("acer circinatum")) %>%
  mutate(error = abs(doy_estimated - doy_observed)) %>%
  mutate(species = snakecase::to_sentence_case(species)) %>%
  select(data_type, species, phenophase,year_observed,observation_source, observation_id, error, model_source) %>%
  group_by(model_source) %>%
  summarise(error_mean = mean(error),
            error_sd = sd(error)) %>%
  ggplot(aes(x=model_source, y=error_mean)) +
  geom_col(color='black',fill='black', width=0.5) +   
  theme_bw(30) +
  labs(x='',y='Mean Error')


model_errors$model_name = factor(model_errors$model_name, levels = model_names, labels = pretty_model_names)
model_errors$parameter_source = factor(model_errors$parameter_source, levels=datasets, labels = pretty_dataset_names)
model_errors$observation_source = factor(model_errors$observation_source, levels=datasets, labels = pretty_dataset_names)
