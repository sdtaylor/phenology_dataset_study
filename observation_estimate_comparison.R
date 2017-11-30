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

#########################################################
# # Leave out the new models for now
predictions = predictions %>%
  filter(!model_name %in% c('m1','msb'))

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

estimate_differences_lts_observations = observation_estimate_comparison %>%
  group_by(model_name, observation_source, non_npn_parameter_source, species, phenophase) %>%
  summarise(rmsd = sqrt(mean(model_estimate_difference^2))) %>%
  ungroup() %>%
  filter(observation_source == non_npn_parameter_source)

estimate_differences_npn_observations = observation_estimate_comparison %>%
  group_by(model_name, observation_source, non_npn_parameter_source, species, phenophase) %>%
  summarise(rmsd = sqrt(mean(model_estimate_difference^2))) %>%
  ungroup() %>%
  filter(observation_source == 'npn')

################################################################
# Graph of RMSD model, phenophase, lts dataset

# Apply more pleasing names to everything for figures
model_names = c('gdd','gdd_fixed','linear_temp','naive','alternating','uniforc','m1','msb')
pretty_model_names = c('GDD','Fixed GDD','Linear','Naive','Alternating','Uniforc','M1','MSB')
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




