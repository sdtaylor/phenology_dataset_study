library(tidyverse)

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
  gather(error_type, error_value, rmse) %>%
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
  filter(data_type=='test') %>%
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
  select(model_name, species, phenophase, error_value, scenario) %>%
  spread(scenario, error_value) %>%
  mutate("Metric 1 (A - B)" = A-B, "Metric 2 (C - D)" = C-D) %>%
  gather(metric, metric_value, "Metric 1 (A - B)", "Metric 2 (C - D)")

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
  labs(y='',x='Difference between scenarios') +
  theme_bw() +
  theme(strip.text.x = element_text(size=20),
        strip.text.y = element_text(size=16),
        axis.text = element_text(size=15),
        axis.title.x = element_text(size=20),
        strip.background = element_rect(fill='grey95'))

ggsave(rmse_metrics_figure, filename = 'manuscript/figure_rmse_metrics_density_plot.png', width = 60, height = 15, units = 'cm')

######################################################
######################################################
# supplement figure, absolute RMSE values of all scenarios

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

ggsave(scenario_error, filename = 'manuscript/supplement_scenario_absolute_rmse.png', width = 30, height = 20, units = 'cm')


#########################################################
# relative rankings of the different scenarios
# scenario_rankings = scenarios_error_data %>%
#   select(model_name, species_phenophase, scenario, error_value) %>%
#   group_by(model_name, species_phenophase) %>%
#   arrange(error_value) %>%
#   mutate(ranking = 1:4) %>%
#   ungroup() %>%
#   group_by(model_name, scenario) %>%
#   summarise(ranking_1 = sum(ranking==1)/n(),
#             ranking_2 = sum(ranking==2)/n(),
#             ranking_3 = sum(ranking==3)/n(),
#             ranking_4 = sum(ranking==4)/n()) %>%
#   ungroup() %>%
#   gather(ranking, rank_percent, ranking_1, ranking_2, ranking_3, ranking_4) %>%
#   group_by(scenario, ranking) %>%
#   summarize(rank_percent = round(mean(rank_percent),2)) %>%
#   spread(scenario, rank_percent)


###########################################################
# Supplement figures
##########################################################

############################################
############################################
# Supplement figure 1. Only NPN errors. 
# or, if using  NPN data which model is best for each species/phenophase?

supplement_fig_12_theme =  theme(axis.text.x = element_text(size=10,angle=90, debug = FALSE),
                                 axis.title = element_text(size=15),
                                 strip.text = element_text(size=6.5)) 
  
supplement_fig1_data = model_errors %>%
  filter(is_npn_model, is_npn_obs) %>%
  select(species, phenophase, model_name, error_value, data_type, observation_source, parameter_source, sample_size)
supplement_fig1_data$species = abbreviate_species_names(supplement_fig1_data$species)

winning_models_npn = supplement_fig1_data %>% 
  group_by(species, phenophase, observation_source, parameter_source, data_type) %>% 
  top_n(1, -error_value) %>%
  ungroup()

npn_sample_size_text = supplement_fig1_data %>%
  group_by(species, observation_source, parameter_source) %>%
  mutate(y_placement = max(error_value) + 1) %>%
  ungroup() %>%
  group_by(species, phenophase, data_type, observation_source, parameter_source) %>%
  summarize(y_placement = mean(y_placement), sample_size = mean(sample_size)) %>%
  ungroup() %>%
  mutate(x_placement = ifelse(data_type=='test', 2,7))

supplement_fig1 = ggplot(supplement_fig1_data, aes(x=model_name, y=error_value, color=data_type, group=data_type)) + 
  geom_point(size=1.5) +
  geom_line(size=1) +
  geom_point(data = winning_models_npn, color='black', shape=4, size=2) +
  geom_text(data=npn_sample_size_text, aes(label = paste0('n: ',sample_size), y=y_placement, x=x_placement)) +
  scale_color_manual(values = c("#CC6666", "#66CC99")) +
  facet_wrap(species~phenophase~parameter_source~observation_source, labeller = 'label_both', scales='free') +
  labs(y='Root Mean Square Error', x='Model', color='Data Type') +
  supplement_fig_12_theme

ggsave(supplement_fig1, filename = 'manuscript/supplement_best_npn_models.png',
       width = 40, height = 50, units = 'cm')

############################################
# supplement figure 2. Only LTS errors. 
# or, if using  LTS data which model is best for each species/phenophase?
# No point in using the spatial models, M1 and MSB, here

supplement_fig2_data = model_errors %>%
  filter(is_lts_model, is_lts_obs) %>%
  filter(!model_name %in% c('M1','MSB')) %>%
  filter(parameter_source==observation_source) %>%
  select(species, phenophase, model_name, error_value, data_type, observation_source, parameter_source, sample_size)
supplement_fig2_data$species = abbreviate_species_names(supplement_fig2_data$species)

winning_models_lts = supplement_fig2_data %>% 
  group_by(species, phenophase, observation_source, parameter_source, data_type) %>% 
  top_n(1, -error_value) %>%
  ungroup()

lts_sample_size_text = supplement_fig2_data %>%
  group_by(species, observation_source, parameter_source) %>%
  mutate(y_placement = max(error_value) + 1) %>%
  ungroup() %>%
  group_by(species, phenophase, data_type, observation_source, parameter_source) %>%
  summarize(y_placement = mean(y_placement), sample_size = mean(sample_size)) %>%
  ungroup() %>%
  mutate(x_placement = ifelse(data_type=='test', 2,5))

supplement_fig2 = ggplot(supplement_fig2_data, aes(x=model_name, y=error_value, color=data_type, group=data_type)) + 
  geom_point(size=1.5) +
  geom_line(size=1) +
  geom_point(data = winning_models_lts, color='black', shape=4, size=2) +
  geom_text(data=lts_sample_size_text, aes(label = paste0('n: ',sample_size), y=y_placement, x=x_placement)) +
  scale_color_manual(values = c("#CC6666", "#66CC99")) +
  facet_wrap(species~phenophase~parameter_source~observation_source, labeller = 'label_both', scales='free') +
  labs(y='Root Mean Square Error', x='Model', color='Data Type') +
  supplement_fig_12_theme


ggsave(supplement_fig2, filename = 'manuscript/supplement_best_lter_models.png',
       width = 40, height = 50, units = 'cm')
############################################
############################################
library(kableExtra)
library(knitr)
###
# Stats for manuscript

# Winning model percentages for NPN observations
npn_overal_best_models = winning_models_npn %>%
  group_by(model_name, data_type) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(data_source='NPN')

# Winning model percentages for LTS observations
lts_overal_best_models = winning_models_lts %>%
  group_by(model_name, data_type) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(data_source='LTER')

overall_best_models = npn_overal_best_models %>%
  bind_rows(lts_overal_best_models) %>%
  spread(model_name, n, fill=0) %>%
  select(data_source, data_type, Naive, Linear, 'Fixed GDD', GDD, M1, Uniforc, Alternating, MSB) %>%
  arrange(data_source, data_type)

#kable(overall_best_models, 'latex')
