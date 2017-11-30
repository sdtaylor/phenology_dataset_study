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

#########################################################
# # Leave out the new models for now
predictions = predictions %>%
  filter(!model_name %in% c('m1','msb'))


##########################################################

model_errors = predictions %>%
  filter(data_type=='test') %>%
  group_by(model_name, observation_source, parameter_source, species, phenophase) %>%
  summarise(rmse = sqrt(mean((doy_estimated - doy_observed)^2)), sample_size=n()) %>%
  ungroup() %>%
  gather(error_type, error_value, rmse) %>%
  mutate(error_value = round(error_value,2)) %>%
  mutate(is_lts_model = parameter_source!='npn', is_lts_obs = observation_source != 'npn')

# Apply more pleasing names to everything for figures
model_names = c('gdd','gdd_fixed','linear_temp','naive','uniforc','alternating','m1','msb')
pretty_model_names = c('GDD','Fixed GDD','Linear','Naive','Uniforc','Alternating','M1','MSB')
datasets = c('harvard','hjandrews','hubbard','jornada','npn')
pretty_dataset_names = c('Harvard Forest','H.J. Andrews','Hubbard Brook','Jornada','NPN')

model_errors$model_name = factor(model_errors$model_name, levels = model_names, labels = pretty_model_names)
model_errors$parameter_source = factor(model_errors$parameter_source, levels=datasets, labels = pretty_dataset_names)


###########################################################

model_error_jitterplot_data = model_errors


point_size=4
point_shapes = c(17,13)
r2_lower_limit = 0
rmse_upper_limit = 20
color_pallete=c("grey42", "#E69F00", "#56B4E9", "#CC79A7", "#009E73")

model_error_jitterplot_data = model_error_jitterplot_data %>%
  mutate(zoomed_subset = ifelse(error_type == 'r2', error_value > r2_lower_limit,
                                error_value < rmse_upper_limit))

common_theme_attr = theme(axis.text = element_text(size=15),
                          legend.title = element_text(size=22),
                          legend.text = element_text(size=20),
                          legend.key.size = unit(10, 'mm'))

npn_rmse_plot = model_error_jitterplot_data %>%
  filter(observation_source == 'npn', error_type=='rmse') %>%
  ggplot(aes(x=model_name, y=error_value, group=parameter_source, color=parameter_source)) + 
  geom_jitter(width = 0.2, size=point_size, aes(shape = phenophase)) +
  geom_boxplot(inherit.aes = FALSE, aes(x=model_name, y=error_value), alpha=0, outlier.shape = NA, size=0.8) +
  scale_shape_manual(values=point_shapes) + 
  scale_color_manual(values=color_pallete) +
  theme_linedraw() +
  common_theme_attr +
  theme(plot.margin = unit(c(1,0,0,0),'cm')) +
  labs(y='RMSE', x='') + 
  facet_zoom(y = zoomed_subset==TRUE) 

lts_rmse_plot = model_error_jitterplot_data %>%
  filter(observation_source != 'npn', error_type=='rmse') %>%
  ggplot(aes(x=model_name, y=error_value, group=parameter_source, color=parameter_source)) + 
  geom_jitter(width = 0.2, size=point_size, aes(shape = phenophase)) +
  geom_boxplot(inherit.aes = FALSE, aes(x=model_name, y=error_value), alpha=0, outlier.shape = NA, size=0.8) +
  scale_shape_manual(values=point_shapes) + 
  scale_color_manual(values=color_pallete) +
  theme_linedraw() +
  common_theme_attr +
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


########################################################################################
# Pairwise comparison between LTS and NPN models
npn_model_errors = model_errors %>%
  filter(!is_lts_model) %>%
  select(-error_type, -is_lts_model, -parameter_source, sample_size) %>%
  rename(npn_error_value = error_value)

pairwise_comparison_data = model_errors %>%
  filter(is_lts_model) %>%
  rename(lts_error_value = error_value) %>%
  select(-error_type, -is_lts_model, -parameter_source) %>%
  left_join(npn_model_errors, by=c('model_name','observation_source','species','phenophase','is_lts_obs')) %>%
  mutate(model_difference = npn_error_value - lts_error_value)

pairwise_comparison_data$is_lts_obs = factor(pairwise_comparison_data$is_lts_obs, levels = c(FALSE, TRUE), labels = c('NPN Observations','LTS Observations'))

y_pos_line=0.15
indicator_lines=data.frame(x=c(-10, 10), xend=c(-25, 25), 
                           y=c(y_pos_line,y_pos_line), yend=c(y_pos_line,y_pos_line),
                           is_lts_obs='NPN Observations', model_name='GDD')
y_pos_text=0.12
indicator_text=data.frame(x=c(-22, 26), y=y_pos_text, t=c('NPN Models\n Better','LTS Models\n Better'),
                          is_lts_obs='NPN Observations', model_name='GDD')

ggplot(pairwise_comparison_data, aes(model_difference)) + 
  geom_density(alpha=0.8,fill='grey50', size=0.1) +
  #geom_density(alpha=0.5) +
  #geom_histogram(bins=50) +
  geom_vline(xintercept = 0, size=1) +
  facet_grid(is_lts_obs~model_name, scales = 'free_y') +
  geom_segment(data=indicator_lines, aes(x=x, xend=xend, y=y, yend=yend), size=0.8, arrow = arrow(length=unit(0.25,'cm')),
               inherit.aes = FALSE) +
  geom_text(data=indicator_text, aes(x=x,y=y, label=t),size=4.5, inherit.aes = F) +
  labs(y='',x='Difference between NPN and LTS derived errors') +
  theme_bw() +
  theme(strip.text.x = element_text(size=20),
        strip.text.y = element_text(size=16),
        axis.text = element_text(size=15),
        axis.title.x = element_text(size=20),
        strip.background = element_rect(fill='grey95'))

###########################################################
