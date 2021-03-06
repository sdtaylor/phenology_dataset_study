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

model_errors$model_name = factor(model_errors$model_name, levels = model_names, labels = pretty_model_names)
model_errors$parameter_source = factor(model_errors$parameter_source, levels=datasets, labels = pretty_dataset_names)
model_errors$observation_source = factor(model_errors$observation_source, levels=datasets, labels = pretty_dataset_names)

##########################################################
abbreviate_species_names = function(s){
  genus_abbr = paste0(toupper(substr(s, 1,1)), '. ')
  specific_epithet = stringr::word(s, 2,2)
  return(paste0(genus_abbr, specific_epithet))
}
###########################################################

scenarios_error_data = model_errors %>%
  filter(data_type=='test', error_type=='rmse') %>%
  mutate(scenario = case_when(
    is_lts_model & is_lts_obs ~ 'A',
    is_npn_model & is_lts_obs ~'B',
    is_lts_model & is_npn_obs ~'C',
    is_npn_model & is_npn_obs ~ 'D'
  ))


# For the scenario comparison, mark hubbard species and the associated NPN species uniquely so they don't
# get mixed up with harvard in the scenario comparisons
hubbard_data = scenarios_error_data %>%
  filter(parameter_source=='Hubbard Brook' | observation_source=='Hubbard Brook')
hubbard_data = scenarios_error_data %>%
  filter((parameter_source=='NPN' & observation_source=='NPN') & species %in% hubbard_data$species & phenophase=='Budburst') %>%
  bind_rows(hubbard_data)

hubbard_data = hubbard_data %>%
  mutate(species = paste0(species,'-HB'))

scenarios_error_data = scenarios_error_data %>%
  filter(!(parameter_source=='Hubbard Brook' | observation_source=='Hubbard Brook')) %>%
  bind_rows(hubbard_data)

rm(hubbard_data)

scenarios_error_data$species_phenophase = with(scenarios_error_data, paste(abbreviate_species_names(species),phenophase,sep=' - '))

#########################################################
#########################################################
# Main figure of the two metrics (scenario A - B and C -D)

rmse_metrics = scenarios_error_data %>%
  filter(error_type=='rmse') %>%
  select(model_name, species, phenophase, error_value, scenario) %>%
  spread(scenario, error_value) %>%
  mutate("With LTER Data" = A-B, "With USA-NPN Data" = C-D) %>%
  gather(metric, metric_value, "With LTER Data", "With USA-NPN Data")

# All p-values for this figure are < 0.001, so these are not actually in the figure
# but only mentioned in the text and caption
test_statistics = rmse_metrics %>%
  group_by(metric, model_name) %>%
  summarise(t_stat = t.test(metric_value)$statistic,
            p_value = t.test(metric_value)$p.value) %>%
  ungroup() %>%
  mutate(t_stat_text = paste0('italic(t) == ',round(t_stat,2)))

# The annotated lines
y_pos_line=0.15
indicator_lines=data.frame(x=c(-12, 12), xend=c(-27, 27),
                           y=c(y_pos_line,y_pos_line), yend=c(y_pos_line,y_pos_line),
                           is_lts_obs='NPN Observations', model_name='GDD')
y_pos_text=0.12
indicator_text=data.frame(x=c(-19, 22), y=y_pos_text, t=c('LTER Models\n Better','NPN Models\n Better'),
                          is_lts_obs='NPN Observations', model_name='GDD')


rmse_metrics_figure=ggplot(rmse_metrics, aes(metric_value)) +
  geom_density(alpha=0.8,fill='grey50', size=0.1) +
  #geom_density(alpha=0.5) +
  #geom_histogram(bins=50) +
  geom_vline(xintercept = 0, size=1) +
  facet_grid(metric~model_name, scales = 'free_y') +
  geom_segment(data=indicator_lines, aes(x=x, xend=xend, y=y, yend=yend), size=0.8, arrow = arrow(length=unit(0.25,'cm')),
               inherit.aes = FALSE) +
  geom_text(data=indicator_text, aes(x=x,y=y, label=t),size=4.5, inherit.aes = F) +
  geom_text(data=test_statistics, aes(x=18, y=0.07, label=t_stat_text), size=5, parse = TRUE) + 
  labs(y='',x='Difference between scenarios') +
  theme_bw() +
  theme(strip.text.x = element_text(size=20),
        strip.text.y = element_text(size=16),
        axis.text = element_text(size=15),
        axis.title.x = element_text(size=20),
        strip.background = element_rect(fill='grey95'))

ggsave(rmse_metrics_figure, filename = paste0(config$image_save_directory,'figure_4_rmse_metrics_density_plot.png'), width = 60, height = 15,dpi=1000, units = 'cm')
######################################################
######################################################
# supplement figure 7, absolute RMSE values of all scenarios

scenario_error = ggplot(scenarios_error_data, aes(x=species_phenophase, y=error_value, group=scenario, color=scenario)) +
  geom_point() +
  geom_line(size=0.8) +
  scale_color_viridis_d() +
  facet_wrap(~model_name, ncol=2)+
  theme(axis.text.x = element_text(size=6, angle = 90, hjust = 1, vjust=0.5, debug=F),
        panel.background = element_rect(fill='grey95'),
        panel.grid.major.y = element_line(size=0.3, color='black'),
        panel.grid.minor.y = element_line(size=0.15, color='grey50'),
        legend.position = c(0.38, 0.96), 
        legend.background = element_rect(fill='grey99'),
        #legend.key.size = unit(10,'mm'),
        legend.direction = "horizontal") +
  labs(y='RMSE',x='Species & Phenophase', color='Scenario') 

ggsave(scenario_error, filename = paste0(config$image_save_directory,'figure_s7_scenario_absolute_rmse.png'), width = 30, height = 20, dpi=1000, units = 'cm')


###########################################################
# Supplement figures
##########################################################
# Table with errors of each scenario (LTER->LTER, LTER-> NPN, NPN->LTER, NPN->NPN)
# and each model using all observations aggregated together.

best_scenario_model = predictions %>%
  filter(data_type=='test') %>%
  mutate(parameter_source = case_when(
            parameter_source!='npn' ~ 'LTER',
            parameter_source=='npn' ~ 'USA-NPN'),
         observation_source = case_when(
            observation_source != 'npn' ~ 'LTER',
            observation_source == 'npn' ~ 'USA-NPN')) %>%
  group_by(model_name, parameter_source, observation_source) %>%
  summarise(rmse = sqrt(mean((doy_estimated - doy_observed)^2)), 
            pearson = cor(doy_estimated, doy_observed, method='pearson'),
            n=n()) %>%
  ungroup() %>%
  gather(error_type, error_value, rmse, pearson) %>%
  mutate(error_value = round(error_value,2)) %>%
  mutate(model_name = factor(model_name, levels = model_names, labels = pretty_model_names, ordered = TRUE)) %>%
  mutate(model_error_type = paste(model_name, error_type, sep='-')) %>%
  select(-model_name, -error_type) %>%
  spread(model_error_type, error_value)

# This outputs a basic latex table. I styled it up and adjusted
# all the text at https://www.tablesgenerator.com/
kable(best_scenario_model, 'latex')

############################################
############################################
# Supplement figure 5. Species level out of sample RMSE
# or, if using  NPN data which model is best for each species/phenophase?

supplement_fig_5_6_theme =  theme(axis.text.x = element_text(size=10,angle=90,hjust = 0.2,vjust = 0.8, debug = FALSE),
                                 axis.title = element_text(size=15),
                                 strip.text = element_text(size=6.5)) +
  theme_grey()
  
supplement_fig5_data = model_errors %>%
  filter(data_type == 'test', error_type=='rmse') %>%
  select(species, phenophase, model_name, error_type, error_value, observation_source, parameter_source, sample_size)
supplement_fig5_data$species = abbreviate_species_names(supplement_fig5_data$species)

winning_models_rmse = supplement_fig5_data %>%
  group_by(species, phenophase, observation_source, parameter_source) %>%
  top_n(1, -error_value) %>%
  ungroup()

supplement_fig5 = ggplot(supplement_fig5_data, aes(x=model_name, y=error_value, color=observation_source, group=observation_source)) + 
  geom_point(size=1.5) +
  geom_line(size=1) +
  geom_point(data = winning_models_rmse, color="#D55E00", shape=4, size=3) +
  scale_color_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#CC79A7")) +
  facet_wrap(species~phenophase~parameter_source, labeller = 'label_both', scales='fixed') +
  labs(y='Root Mean Square Error', x='Model', color='Observation Source') +
  supplement_fig_5_6_theme

ggsave(supplement_fig5, filename = paste0(config$image_save_directory,'figure_s5_all_model_rmse.png'),
       width = 40, height = 50, units = 'cm')

############################################
# supplement figure 6. Species level out of sample pearson correlation

supplement_fig6_data = model_errors %>%
  filter(data_type == 'test', error_type=='pearson') %>%
  select(species, phenophase, model_name, error_type, error_value, observation_source, parameter_source, sample_size)
supplement_fig6_data$species = abbreviate_species_names(supplement_fig6_data$species)

winning_models_pearson = supplement_fig6_data %>%
  group_by(species, phenophase, observation_source, parameter_source) %>%
  top_n(1, error_value) %>%
  ungroup()

supplement_fig6 = ggplot(supplement_fig6_data, aes(x=model_name, y=error_value, color=observation_source, group=observation_source)) + 
  geom_point(size=1.5) +
  geom_line(size=1) +
  geom_point(data = winning_models_pearson, color="#D55E00", shape=4, size=3) +
  scale_color_manual(values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#CC79A7")) +
  facet_wrap(species~phenophase~parameter_source, labeller = 'label_both', scales='fixed') +
  labs(y='Pearson Correlation', x='Model', color='Observation Source') +
  supplement_fig_5_6_theme

ggsave(supplement_fig6, filename = paste0(config$image_save_directory,'figure_s6_all_model_pearson.png'),
       width = 40, height = 50, units = 'cm')

